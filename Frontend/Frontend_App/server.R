library(shiny)
library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(xts)
library(AICcmodavg)
library(fresh)
library(RColorBrewer)
library(dynlm)


RGDP_Data <- read_excel("../Data/RGDP Data.xlsx")
#source("../../Backend/inputs.R")
#source("../../Backend/GDP Cleaning.R")
#source("../../Backend/ADL Data.R")
#source("../../Backend/AR_Model_Functions.R")
#source("../../Backend/ADL Functions.R")
#source("../../Backend/Combined ADL Model Functions.R")
#source("Graph Functions.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  ############################  
  ## RESTRICTING SLIDER RANGE
  ############################  
  
  count_quarters <- function(date1, date2) {
    year1 <- as.numeric(substr(date1, 1, 4)) # Extract year and quarter from the input dates
    quarter1 <- as.numeric(substr(date1, 7, 7))
    
    year2 <- as.numeric(substr(date2, 1, 4))
    quarter2 <- as.numeric(substr(date2, 7, 7))
    
    year_diff <- year2 - year1  # Calculate the difference in years
    
    quarter_diff <- (year_diff * 4) + (quarter2 - quarter1)     # Calculate the difference in quarters
    
    return(quarter_diff)
  }
  
  adjust_year_quarters <- function(date1, date2, year_quarters) {
    
    if (count_quarters(date1, date2) < 80) {
      
      if ((count_quarters(date2, tail(year_quarters, 1)) + 1) >= 80) { # Check if the second date can be adjusted forward by 20 quarters
        
        pos1 <- which(year_quarters == date1) # Find the position of the adjusted first date
        pos2 <- pos1 + 79
        return(c(date1, year_quarters[pos2]))
        
      } else {
        
        pos2 <- which(year_quarters == date2)
        pos1 <- pos2 - 79
        return(c(year_quarters[pos1], date2))
      }
    } else {
      return(c(date1, date2))
    }
  }
  
  observeEvent(input$year,{
    updateSliderTextInput(session,"year", selected = adjust_year_quarters(input$year[1], input$year[2], RGDP_Data$DATE))
  })
  
  #######################
  ## SAMPLE FILE DOWNLOAD
  #######################
  
  output$download_data <- downloadHandler(
    filename = function(){
      "FRED Unemployment.xls"
    },
    content = function(file){
      download.file("https://github.com/zitinbali/Initial-Repo/blob/main/Data/FRED%20Unemployment.xls", destfile = file)
    }
  )
  
  #######################
  ## ROLLING WINDOW INPUT
  #######################
  
  output$rolling_ADL <- renderUI({
    
    selectInput("rolling_ADL",
                "Select a Date: ",
                choices = RGDP_Data$DATE[which(input$year[2]==RGDP_Data$DATE):length(RGDP_Data$DATE)],
                selected = RGDP_Data$DATE[length(RGDP_Data$DATE)]
    )
  })
  
  

  covid_dummy_fn <- function(example_startyq, example_endyq)
    
  covid_dummy_ts <- ts(covid_dummy_fn,
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
  
  all_GDP_ts <- ts(all_GDP_data, 
                   start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                   end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
        frequency = 4)
  
  all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
    
  all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
  
  GDPGrowth_ts_df_sliced <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
    mutate(Time = as.yearqtr(Time)) %>%
    mutate(growth_rate = as.numeric(growth_rate))
  
  GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
  
  #####within model
  
  observeEvent(input$button1, {
    output$model1 <- renderPlot({
  
   h = as.numeric(input$h)
  
  #h = 2
  start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
  end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
  
  basic_AR_input <- GDPGrowth_ts_df_sliced[start_rownum:end_rownum, ] %>% 
    select(growth_rate) %>% 
    as.matrix()
  
  start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
  end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
  
 
  
  ## generating values for prediction graph
  predictions <- all_GDP_ts_df %>% 
    filter(Time > example_endyq) %>% 
    head(n = h) %>%
    mutate(new_growth_rate = c(1, 2)) #### 
  
  predicted_test_values <- predictions %>%
    select(Time, new_growth_rate) %>% 
    rename("growth_rate" = "new_growth_rate") %>% 
    mutate(category = 2) 
  
  predicted_data <- rbind(actual_values_graph(example_startyq, example_endyq, h)$training_p, predicted_test_values)
  
  # fanplot
  
  time_data <- all_GDP_ts_df %>%
    filter(Time >= example_endyq) %>%
    select(Time) %>%
    head(h+1)
  
  data <- GDPGrowth_ts_df %>%
    filter(Time < example_endyq) %>%
    select(Time) %>% 
    mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
  
  rmsfe_data <- cbind(time_data, fanplot_rmsfe(GDPGrowth_ts_df, basic_AR_input, predictions, h)) 
  rmsfe_data <- cbind(time_data, rmsfe_test)
  
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
    filter(xmin >= start_plot & xmax <= end_plot) #replace w start and end of lineplot
  
  model_1 <- ggplot() +
    geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
    geom_line(data = original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
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
  
    })
    
  })
  

}
