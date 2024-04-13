#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
RGDP_Data <- read_excel("RGDP Data.xlsx")

# Define server logic required to draw a histogram
function(input, output, session) {

  covid_dummy_fn <- function(example_startyq, example_endyq)
    
  covid_dummy_ts <- ts(covid_dummy_fn,
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
  
  
  # formatting the data variable in terms of year and quarters
  
  #start_rownum = which(grepl(as.yearqtr("2000 Q1"), check$Time))
  #end_rownum = which(grepl(as.yearqtr(example_endq), check$Time))
  
  GDPGrowth_ts_df <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  
  start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df$Time))
  end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df$Time))
  
  basic_AR_input <- GDPGrowth_ts_df[start_rownum:end_rownum, ] %>% 
    select(growth_rate) %>% 
    as.matrix()
  
  start_plot = GDPGrowth_ts_df$Time[end_rownum - 10]
  
  h = as.numeric(input$h)
  
  #p = as.numeric(fitAR(basic_AR_input, h, covid_dummy)$p)
  
  #h = 2
  
  #Graph fn
  actual_values_graph <- function(example_startyq, example_endyq){
    
    training <- GDPGrowth_ts_df %>%
      #mutate(Time = as.yearqtr(Time)) %>%
      filter(Time > example_startyq) %>%
      filter(Time < example_endyq) %>% 
      tail(n = 9) %>%
      select(Time, growth_rate) %>%
      #mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 1) 
    
    joining_value <- GDPGrowth_ts_df %>%
      #mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time == example_endyq) %>% 
      select(Time, growth_rate) %>%
      #mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 3) 
    
    training_t <- bind_rows(training, joining_value) %>%
      mutate(category = 1) 
    
    training_p <- bind_rows(training, joining_value)
    
    return(list("training_p" = training_p))
  }
  
  
  predictions <- GDPGrowth_ts_df %>% 
    #mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > example_endyq) %>% 
    head(n = h) %>%
    mutate(new_growth_rate = 1)
  
  # Separate predictions into actual and predicted dataframes for plotting
  
  actual_test_values <- predictions %>% 
    select(Time, growth_rate) %>%
    mutate(category = 2)
  
  original_data <- rbind(training_t, actual_test_values)
  
  predicted_test_values <- predictions %>%
    select(Time, new_growth_rate) %>% 
    rename("growth_rate" = "new_growth_rate") %>% 
    mutate(category = 3) 
  
  predicted_data <- rbind(actual_values_graph(example_startyq, example_endyq)$training_p, predicted_test_values)
  
  time_data <- GDPGrowth_ts_df %>%
    filter(Time >= example_endyq) %>%
    select(Time) 
  
  data <- GDPGrowth_ts_df %>%
    #mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time < example_endyq) %>%
    select(Time) %>% 
    mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
  
  rmsfe_data <- cbind(time_data, fanplot_rmsfe(GDPGrowth_ts_df, basic_AR_input, predictions, h)) 
  
  fanplot_data <- rbind(data, rmsfe_data) %>%
    filter(Time >= example_endyq) %>%
    head(h+1)
  
  # recession blocks
  recessions <- c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991,
                  2001, 2007:2008)
  
  rectangles <- data.frame(
    xmin = as.yearqtr(c("1961 Q1", "1970 Q1", "1974 Q1", "1980 Q1", "1990 Q1", "2001 Q1", "2007 Q1")),
    xmax = as.yearqtr(c("1962 Q4", "1970 Q4", "1975 Q4", "1982 Q4", "1991 Q4", "2001 Q4", "2008 Q4")),
    ymin = -Inf,
    ymax = Inf
  )
  
  recession_block = rectangles %>%
    filter(xmin >= start_plot & xmax <= as.yearqtr(gsub(":", " ", input$year[2]))) #replace w start and end of lineplot
  
  model_1 <- ggplot() +
        
    geom_line(data = original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
    geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +

    geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
    labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
         color = "Legend") +  # Set the legend title
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid = element_blank(),
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line(color = "black"),
          plot.margin = margin(20,20,20,20))
  
  print(model_1)
  

}
