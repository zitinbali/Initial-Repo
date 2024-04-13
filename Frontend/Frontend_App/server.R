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
  
  start_rownum = which(grepl(example_startyq), GDPGrowth_ts_df$Time)
  end_rownum = which(grepl(example_endyq), GDPGrowth_ts_df$Time)
  
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
    mutate(new_growth_rate = c(1, 2))
  
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

}
