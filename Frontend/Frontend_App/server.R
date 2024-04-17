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
library(markdown)
library(DT)
library(lsei)
library(sandwich)

RGDP_Data <- read_excel("Data/RGDP Data.xlsx")

source("../../NEW Backend/GDP Cleaning Functions.R")
source("../../NEW Backend/AR_Model_Functions.R")
source("../../NEW Backend/ADL Data Functions.R")
source("../../NEW Backend/ADL Functions.R")
source("../../NEW Backend/ADL_Rolling.R")
source("../../NEW Backend/Combined ADL Functions.R")
source("../../NEW Backend/Aggregate Functions.R")
source("../../NEW Backend/DM_test.R")
source("Graph Functions.R")

check <- basic_cleaning(RGDP_Data)$check
perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
covid = c("2020 Q2", "2020 Q3")
covid_start = as.yearqtr(covid[1])
covid_end = as.yearqtr(covid[2])

ADL_variables <- c("baa_aaa_ts", "tspread_ts", "hstarts_ts", "consent_ts", 
                   "nasdaq_ts")


# defining ADL predictors

baa_aaa <- read_excel("Data/FRED BAA-AAA Data.xls", 
                      col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

tspread <- read_excel("Data/FRED Treasury Spread.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

hstarts <- read_excel("Data/FRED Hstarts.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

consent <- read_excel("Data/FRED Consumer Sentiment.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

nasdaq <- read_excel("Data/NASDAQCOM.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))


# source("../../NEW Backend/ADL_Rolling.R")
# source("../../NEW Backend/Combined ADL Functions.R")
# source("../../NEW Backend/DM_test.R")
# #source("../../NEW Backend/Granger Ramanathan.R")
# source("../../NEW Backend/Aggregate Functions.R")
# #source("../../NEW Backend/Uploading New Indicator.R")
 source("Graph Functions.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  options(DT.options = list(lengthChange = FALSE))
  
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
  
  output$rolling_input <- renderUI({
    
    selectInput("rolling_ADL",
                "Select Start of Test Window: ",
                choices = RGDP_Data$DATE[which(input$year[1]==RGDP_Data$DATE)+80:length(RGDP_Data$DATE)-1],
                selected = RGDP_Data$DATE[which(input$year[1]==RGDP_Data$DATE)+80]
    )
  })
  
  

  covid_dummy_fn <- function(example_startyq, example_endyq)
    
  covid_dummy_ts <- ts(covid_dummy_fn,
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
  
  # recession blocks
  recessions <- c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991,
                  2001, 2007:2008)
  
  rectangles <- data.frame(
    xmin = as.yearqtr(c("1961 Q1", "1970 Q1", "1974 Q1", "1980 Q1", "1990 Q1", "2001 Q1", "2007 Q1")),
    xmax = as.yearqtr(c("1962 Q4", "1970 Q4", "1975 Q4", "1982 Q4", "1991 Q4", "2001 Q4", "2008 Q4")),
    ymin = -Inf,
    ymax = Inf
  )
  
  #################
  ## BASIC AR MODEL
  #################
  
  ## MODEL 1 PLOT 
  
  observeEvent(input$button1, {
    output$model1 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
       edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))

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

      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)

    start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
    end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))

    start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
    end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]

    pred_df = AR_predict_all(as.matrix(GDPGrowth_ts), h, covid_dummy)
    
    ### full gdp data
    
    start_rownum = which(grepl(example_startyq, check$Time))
    end_rownum = which(grepl(example_endyq, check$Time)) 
    
    row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
    row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
    
    full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
    
    ## generating values for prediction graph
    predictions <- all_GDP_ts_df %>%
      filter(Time > example_endyq) %>%
      head(n = h) %>%
      mutate(new_growth_rate = pred_df$predictions)

    predicted_test_values <- predictions %>%
      select(Time, new_growth_rate) %>%
      rename("growth_rate" = "new_growth_rate") %>%
      mutate(category = 2)

    predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)

    # fanplot

    time_data <- full_GDP_growth %>%
      filter(Time >= example_endyq) %>%
      select(Time) %>%
      head(h+1)

  data <- GDPGrowth_ts_df_sliced %>%
    filter(Time < example_endyq) %>%
    select(Time) %>%
    mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)

  actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)
  joining_value = actual_values$joining_value

  #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
  rmsfe_df = pred_df$rmsfe

  rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h))
  #rmsfe_data <- cbind(time_data, rmsfe_test)

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
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
    geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
    geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
    geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
    scale_x_yearqtr(format = '%Y Q%q')+
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
    
    ## MODEL 1 TABLE
    
    output$table1 <- DT::renderDataTable({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      h = as.numeric(input$h)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      all_GDP_ts <- ts(all_GDP_data, 
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)
      
      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")
      
      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      
      pred_df = AR_predict_all(as.matrix(GDPGrowth_ts), h, covid_dummy)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>%
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)
      
      predictions
    })
    
  })
  
  #################
  ## REVISED AR MODEL
  #################
  
  ## MODEL 2 PLOT 
  
  observeEvent(input$button2, {
    output$model2 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      #add_data = add_data(input$data1, input$data2, input$data3, input$data4)
      #add_data_inputs = add_data$vector
      
      #timeframe = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4")
      
      #add_data_time = timeframe[1:add_data$length]
      #edge_time = timeframe[add_data$length+1: add_data$length+1+h]
      
      #edge_inputs = rep(0, h)
      
      #total_inputs = c(add_data_inputs, edge_inputs)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), 
                         growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      advanced_AR_input = adv_ar_input(RGDP_Data, perc_change_df, example_startq, example_endq)
      pred_df = AR_predict_all(advanced_AR_input, h, covid_dummy)
      
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      
      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(new_growth_rate = pred_df$predictions)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)

      
      # fanplot
      
      time_data <- full_GDP_growth %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(h+1)
      
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)
      joining_value = actual_values$joining_value
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmsfe
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
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
      
      model_2 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_2)
    })
    
    ## MODEL 2 TABLE
    
    output$table2 <- DT::renderDataTable({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)

      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      all_GDP_ts <- ts(all_GDP_data, 
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)
      
      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")
      
      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      
      advanced_AR_input = adv_ar_input(RGDP_Data, perc_change_df, example_startq, example_endq)
      pred_df = AR_predict_all(advanced_AR_input, h, covid_dummy)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>%
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)
      
      predictions
    })
  })
  
  #####################
  ## ADD DATA TO REV AR
  #####################
  
  observeEvent(input$add_data, {
    updateSliderInput(session, "year", value = c(input$year[1], "2023:Q4"))
    ## MODEL 2a ADD DATA PLOT
    output$model2a <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      h = as.numeric(input$h)
      #h=2
      add_data = add_data(as.numeric(input$data1), as.numeric(input$data2), as.numeric(input$data3), as.numeric(input$data4), h)
      add_data_inputs = add_data$vector
      
      timeframe = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4")
      
      add_data_time = timeframe[1:add_data$length]
      edge_time = timeframe[((add_data$length)+1): ((add_data$length)+h)]
      total_time = c(add_data_time, edge_time)
      
       example_endyq = as.yearqtr(tail(add_data_time, n=1))
       #example_endq = "2023 Q4"
      #example_endq = tail(add_data_time, n=1)
       edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4"), 
                          growth_rate = c(NA,NA,NA,NA,NA,NA,NA,NA)) %>%
         mutate(Time = as.yearqtr(Time)) %>%
         mutate(growth_rate = as.numeric(growth_rate))
       
       input_data_df = data.frame(Time = as.yearqtr(add_data_time), growth_rate = add_data_inputs) #%>%
       #mutate(category = 2)
       
       edge = rbind(input_data_df, tail(edge, n=8-nrow(input_data_df)))
       
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
 
       GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      advanced_AR_input = adv_ar_input(RGDP_Data, perc_change_df, example_startq, example_endq)
      pred_df = AR_predict_all(advanced_AR_input, h, covid_dummy)
      
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check)
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      full_GDP_growth <- rbind(full_GDP_growth, edge)
      
      ## generating values for prediction graph
      predictions <- GDPGrowth_ts_df_sliced %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(new_growth_rate = pred_df$predictions)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph_add(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, add_data_time, add_data_inputs, h)$training_p, predicted_test_values)
      
      # fanplot
      
      time_data <- full_GDP_growth %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(h+1)
      
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph_add(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, add_data_time, add_data_inputs, h)
      joining_value = actual_values$joining_value
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmsfe
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
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
      
      model_2a <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_2a)
    })
    
    ## MODEL 2a ADD DATA TABLE   
    
    output$table2a <- DT::renderDataTable({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      h = as.numeric(input$h)
      #h=2
      add_data = add_data(as.numeric(input$data1), as.numeric(input$data2), as.numeric(input$data3), as.numeric(input$data4), h)
      add_data_inputs = add_data$vector
      
      timeframe = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4")
      
      add_data_time = timeframe[1:add_data$length]
      edge_time = timeframe[((add_data$length)+1): ((add_data$length)+h)]
      total_time = c(add_data_time, edge_time)
      
      example_endyq = as.yearqtr(tail(add_data_time, n=1))
      #example_endq = "2023 Q4"
      #example_endq = tail(add_data_time, n=1)
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4"), 
                         growth_rate = c(NA,NA,NA,NA,NA,NA,NA,NA)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      input_data_df = data.frame(Time = as.yearqtr(add_data_time), growth_rate = add_data_inputs) #%>%
      #mutate(category = 2)
      
      edge = rbind(input_data_df, tail(edge, n=8-nrow(input_data_df)))
      
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
      
      GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      advanced_AR_input = adv_ar_input(RGDP_Data, perc_change_df, example_startq, example_endq)
      pred_df = AR_predict_all(advanced_AR_input, h, covid_dummy)
      
      ## generating values for prediction graph
      predictions <- GDPGrowth_ts_df_sliced %>% 
        filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>%
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)
    })
    
  })

  
  
  ####################
  ## INDIV ADL MODEL
  ####################
  
  ## MODEL 3 PLOT
  
  observeEvent(input$button3, {
    output$model3 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))

      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      # define all the ADL indicators 
      baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
      
      baa_aaa_ts <- ts(baa_aaa$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      tspread <- ADL_splice(tspread, example_startyq, example_endyq)
      
      tspread_ts <- ts(tspread$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
      
      hstarts_ts <- ts(hstarts$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      consent <- ADL_splice(consent, example_startyq, example_endyq)
      
      consent_ts <- ts(consent$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
      
      nasdaq_ts <- ts(nasdaq$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)
      
      
      X_name = rename_variable(input$select_ADL)
      X_df = get(X_name)
      
      pred_df = ADL_predict_all(GDPGrowth_ts, X_df, example_startq, example_endq, h, covid_dummy)
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(new_growth_rate = pred_df$predictions)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)
      
      # fanplot
      
      time_data <- full_GDP_growth %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(h+1)
      
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)
      joining_value = actual_values$joining_value
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmsfe
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
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
      
      model_3 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_3)
    })
    
    ## MODEL 3 TABLE
    
    output$table3 <- DT::renderDataTable({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      h = as.numeric(input$h)

      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))

      all_GDP_ts <- ts(all_GDP_data,
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)

      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>%
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")

      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)

      # define all the ADL indicators
      baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
      
      baa_aaa_ts <- ts(baa_aaa$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      tspread <- ADL_splice(tspread, example_startyq, example_endyq)
      
      tspread_ts <- ts(tspread$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
      
      hstarts_ts <- ts(hstarts$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      consent <- ADL_splice(consent, example_startyq, example_endyq)
      
      consent_ts <- ts(consent$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
      
      nasdaq_ts <- ts(nasdaq$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)
      
      
      X_name = rename_variable(input$select_ADL)
      X_df = get(X_name)
      

      pred_df = ADL_predict_all(GDPGrowth_ts, X_df, example_startq,
                                example_endq, h, covid_dummy)

      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>%
        filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>%
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)

      predictions

    })
    
  })

  
  ####################
  ## COMBINED ADL MODEL
  ####################
  
  ## MODEL 4 PLOT
  
  observeEvent(input$button4, {
    output$model4 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
      
      baa_aaa_ts <- ts(baa_aaa$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      tspread <- ADL_splice(tspread, example_startyq, example_endyq)
      
      tspread_ts <- ts(tspread$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
      
      hstarts_ts <- ts(hstarts$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      consent <- ADL_splice(consent, example_startyq, example_endyq)
      
      consent_ts <- ts(consent$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
      
      nasdaq_ts <- ts(nasdaq$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)
      
      # NEED TO FORM THE COMBINED DATASET FIRST
      X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
      # set colnames 
      colnames(X_comb_df) <- ADL_variables
      
      pred_df = ADL_comb_predict_all(GDPGrowth_ts, X_comb_df, ADL_variables,
                                     example_startq, example_endq, h, covid_dummy)
      
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(new_growth_rate = pred_df$predictions)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)
      
      # fanplot
      
      time_data <- full_GDP_growth %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(h+1)
      
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)
      joining_value = actual_values$joining_value
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmsfe
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
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
      
      model_4 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_4)
    })
    
    ## MODEL 4 TABLE
    
    output$table4 <- DT::renderDataTable ({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
      
      baa_aaa_ts <- ts(baa_aaa$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      tspread <- ADL_splice(tspread, example_startyq, example_endyq)
      
      tspread_ts <- ts(tspread$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
      
      hstarts_ts <- ts(hstarts$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      consent <- ADL_splice(consent, example_startyq, example_endyq)
      
      consent_ts <- ts(consent$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
      
      nasdaq_ts <- ts(nasdaq$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)
      
      # NEED TO FORM THE COMBINED DATASET FIRST
      X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
      # set colnames 
      colnames(X_comb_df) <- ADL_variables
      
      pred_df = ADL_comb_predict_all(GDPGrowth_ts, X_comb_df, ADL_variables,
                                     example_startq, example_endq, h, covid_dummy)
      
      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)
      
      predictions
      
    })
    
  })
  
 
  ####################
  ## ADD PREDICTOR MODEL
  ####################
  
  observeEvent(input$button5, {
    
    ## MODEL 5 PLOT
    
    output$model5 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + h]
      
      ## prep new predictor data
      
      new_predictor <- read_excel(input$excel_data$datapath, 
                                  col_names = c("Date", "Spread")) %>% 
        mutate(Date = as.yearqtr(Date), 
               Spread = as.numeric(Spread))
      
      new_predictor <- ADL_splice(new_predictor, example_startyq, example_endyq)
      
      new_predictor_ts <- ts(new_predictor$Spread, 
                             start = c(start_y, start_q), 
                             end = c(end_y, end_q), 
                             frequency = 4)
      
      pred_df = ADL_predict_all(GDPGrowth_ts, new_predictor_ts, example_startq,
                                example_endq, h, covid_dummy)
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      ## generating values for prediction graph
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>% 
        head(n = h) %>%
        mutate(new_growth_rate = pred_df$predictions)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)
      
      # fanplot
      
      time_data <- all_GDP_ts_df %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(h+1)
      
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)
      joining_value = actual_values$joining_value
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmsfe
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, h)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
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
      
      model_5 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_5)
    })
    
    ## MODEL 5 TABLE  
    
    output$table5 <- DT::renderDataTable ({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      h = as.numeric(input$h)
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      all_GDP_ts <- ts(all_GDP_data, 
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)
      
      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")
      
      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      
      pred_df = AR_predict_all(as.matrix(GDPGrowth_ts), h, covid_dummy)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
        head(n = h) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
        select(Date, Predictions, RMSFE) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions', "RMSFE"), digits=3)
      
      predictions
      
    })
  })
  
    
    #################
    ## AGGREGATE MODEL
    #################
  
  #source("../../NEW Backend/Granger Ramanathan.R")
    
    ## MODEL 6 PLOT 
    
    observeEvent(input$button6, {
      output$model6 <- renderPlot({
        example_startq = gsub(":", " ", "1976 Q4")
        example_endq = gsub(":", " ", "1999 Q1")
        example_startyq = as.yearqtr(gsub(":", " ", "1976 Q4"))
        example_endyq = as.yearqtr(gsub(":", " ", "1999 Q1"))
        start_y = as.numeric(year(as.yearqtr(gsub(":", " ", "1976 Q4"))))
        start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", "1976 Q4"))))
        end_y = as.numeric(year(as.yearqtr(gsub(":", " ", "1999 Q1"))))
        end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", "1999 Q1"))))
        
    
        ###################
        # NEW CONTENT HERE
        ###################
        
        GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
        GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
        all_GDP_data <- GDP_prep$all_GDP_data
        spliced_GDP <- GDP_prep$spliced_GDP
        sliced_perc_change <- GDP_prep$sliced_perc_change
        perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
        
        h = as.numeric(input$h)
        
        covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
        
        # Dummy if timeframe ends on 2020 Q2, start of covid
        if (example_startyq <= covid_start & example_endyq == covid_start){
          index = (covid_start - example_startyq) * 4 + 1
          covid_dummy[index] = -1
        }
        
        # Dummy if timeframe includes all of covid
        if (example_startyq <= covid_start & example_endyq >= covid_end){
          index = (covid_start - example_startyq) * 4 + 1
          covid_dummy[index] = -1
          covid_dummy[index + 1] = 1
        }
        
        
        covid_dummy_ts <- ts(covid_dummy,
                             start = c(start_y, start_q), 
                             end = c(end_y, end_q), 
                             frequency = 4)

        edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
          mutate(Time = as.yearqtr(Time)) %>%
          mutate(growth_rate = as.numeric(growth_rate))
        
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
        
        #GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
        
        #start_rownum_window = which(grepl(example_startq, GDPGrowth_ts_df_sliced$Time))
        #window_start = as.yearqtr(GDPGrowth_ts_df_sliced$Time[start_rownum_window + 60]) #test window starts 15 years after start of slider
        
        example_fhorizon = as.numeric(input$h)
        
        start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
        end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
        
        start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
        end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + example_fhorizon]
        
        # define all the ADL indicators
        baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
        
        baa_aaa_ts <- ts(baa_aaa$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        tspread <- ADL_splice(tspread, example_startyq, example_endyq)
        
        tspread_ts <- ts(tspread$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
        
        hstarts_ts <- ts(hstarts$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        consent <- ADL_splice(consent, example_startyq, example_endyq)
        
        consent_ts <- ts(consent$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
        
        nasdaq_ts <- ts(nasdaq$Spread, 
                        start = c(start_y, start_q), 
                        end = c(end_y, end_q), 
                        frequency = 4)
        
        
        # X_comb_df 
        X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
        # set colnames 
        colnames(X_comb_df) <- ADL_variables
        
        # perc_change_df_spliced
        start_rownum = which(grepl(example_startyq, check$Time))
        end_rownum = which(grepl(example_endyq, check$Time))
        
        row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
        row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4
        
        real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
        perc_change_df_spliced = perc_change_df[start_rownum:end_rownum,]
        
        ###########
        # ENDS HERE
        ###########
        
        
        pred_df = aggregate_output(GDPGrowth_ts, X_comb_df, RGDP_Data, 
                                   perc_change_df, perc_change_df_spliced, real_values,
                                   ADL_variables, example_startq, example_endq, example_fhorizon, covid_dummy, 
                                   baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
        
        #print(pred_df)
        #print(pred_df$predictions)
        
        
        ## generating values for prediction graph
        predictions <- all_GDP_ts_df %>% 
          filter(Time > example_endyq) %>% 
          head(n = example_fhorizon) %>%
          mutate(new_growth_rate = pred_df$predictions)
        
        predicted_test_values <- predictions %>%
          select(Time, new_growth_rate) %>% 
          rename("growth_rate" = "new_growth_rate") %>% 
          mutate(category = 2) 
        
        ### full gdp data
        
        predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h)$training_p, predicted_test_values)
        
        start_rownum = which(grepl(example_startyq, check$Time))
        end_rownum = which(grepl(example_endyq, check$Time)) 
        
        row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
        row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + example_fhorizon
        
        full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
        
        
        # fanplot
        
        time_data <- full_GDP_growth %>%
          filter(Time >= example_endyq) %>%
          select(Time) %>%
          head(example_fhorizon+1)
        
        data <- GDPGrowth_ts_df_sliced %>%
          filter(Time < example_endyq) %>%
          select(Time) %>% 
          mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
        
        actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, example_fhorizon)
        joining_value = actual_values$joining_value
        
        #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
        rmsfe_df = pred_df$rmsfe
        
        rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, example_fhorizon)) 
        #rmsfe_data <- cbind(time_data, rmsfe_test)
        
        fanplot_data <- rbind(data, rmsfe_data) %>%
          filter(Time >= example_endyq) %>%
          head(example_fhorizon+1)
        
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
        
        model_6 <- ggplot() +
          geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
          geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
          geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
          geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
          geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
          scale_x_yearqtr(format = '%Y Q%q')+ 
          labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
               color = "Legend") +  # Set the legend title
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                panel.grid = element_blank(),
                panel.border = element_blank(),  # Remove panel border
                axis.line = element_line(color = "black"),
                plot.margin = margin(20,20,20,20))
        
        print(model_6)
      })
      
      ## MODEL 6 TABLE
      
      output$table6 <- DT::renderDataTable({
        
        example_startq = gsub(":", " ", input$year[1])
        example_endq = gsub(":", " ", input$year[2])
        example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
        example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
        start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
        start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
        end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
        end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
        
        
        ###################
        # NEW CONTENT HERE
        ###################
        
        GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
        GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
        all_GDP_data <- GDP_prep$all_GDP_data
        spliced_GDP <- GDP_prep$spliced_GDP
        sliced_perc_change <- GDP_prep$sliced_perc_change
        perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
        
        h = as.numeric(input$h)
        
        covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
        
        # Dummy if timeframe ends on 2020 Q2, start of covid
        if (example_startyq <= covid_start & example_endyq == covid_start){
          index = (covid_start - example_startyq) * 4 + 1
          covid_dummy[index] = -1
        }
        
        # Dummy if timeframe includes all of covid
        if (example_startyq <= covid_start & example_endyq >= covid_end){
          index = (covid_start - example_startyq) * 4 + 1
          covid_dummy[index] = -1
          covid_dummy[index + 1] = 1
        }
        
        
        covid_dummy_ts <- ts(covid_dummy,
                             start = c(start_y, start_q), 
                             end = c(end_y, end_q), 
                             frequency = 4)
        
        edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
          mutate(Time = as.yearqtr(Time)) %>%
          mutate(growth_rate = as.numeric(growth_rate))
        
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
        
        GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
        
        #start_rownum_window = which(grepl(example_startq, GDPGrowth_ts_df_sliced$Time))
        #window_start = as.yearqtr(GDPGrowth_ts_df_sliced$Time[start_rownum_window + 60]) #test window starts 15 years after start of slider
        
        example_fhorizon = as.numeric(input$h)
        
        start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
        end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
        
        start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
        end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + example_fhorizon]
        
        # define all the ADL indicators
        baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
        
        baa_aaa_ts <- ts(baa_aaa$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        tspread <- ADL_splice(tspread, example_startyq, example_endyq)
        
        tspread_ts <- ts(tspread$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
        
        hstarts_ts <- ts(hstarts$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        consent <- ADL_splice(consent, example_startyq, example_endyq)
        
        consent_ts <- ts(consent$Spread, 
                         start = c(start_y, start_q), 
                         end = c(end_y, end_q), 
                         frequency = 4)
        
        nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
        
        nasdaq_ts <- ts(nasdaq$Spread, 
                        start = c(start_y, start_q), 
                        end = c(end_y, end_q), 
                        frequency = 4)
        
        
        # X_comb_df 
        X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
        # set colnames 
        colnames(X_comb_df) <- ADL_variables
        
        # perc_change_df_spliced
        start_rownum = which(grepl(example_startyq, check$Time))
        end_rownum = which(grepl(example_endyq, check$Time))
        
        row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
        row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4
        
        real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
        perc_change_df_spliced = perc_change_df[start_rownum:end_rownum,]
        
        ###########
        # ENDS HERE
        ###########
        
        pred_df = aggregate_output(GDPGrowth_ts, X_comb_df, RGDP_Data, 
                                   perc_change_df, perc_change_df_spliced, real_values,
                                   ADL_variables, example_startq, example_endq, example_fhorizon, covid_dummy, 
                                   baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
        
        predictions <- all_GDP_ts_df %>% 
          filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>%
          head(n = h) %>%
          mutate(Date = as.character(Time), Predictions = pred_df$predictions, RMSFE = pred_df$rmsfe) %>%
          select(Date, Predictions, RMSFE) %>% 
          datatable() %>% 
          formatRound(columns=c('Predictions', "RMSFE"), digits=3)
        
        
        predictions
      })
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      #start_rownum_window = which(grepl(example_startq, GDPGrowth_ts_df_sliced$Time))
      #window_start = as.yearqtr(GDPGrowth_ts_df_sliced$Time[start_rownum_window + 60]) #test window starts 15 years after start of slider
      
      example_fhorizon = as.numeric(input$h)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + example_fhorizon]
      
      # define all the ADL indicators
      baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
      
      baa_aaa_ts <- ts(baa_aaa$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      tspread <- ADL_splice(tspread, example_startyq, example_endyq)
      
      tspread_ts <- ts(tspread$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      hstarts <- ADL_splice(hstarts, example_startyq, example_endyq)
      
      hstarts_ts <- ts(hstarts$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      consent <- ADL_splice(consent, example_startyq, example_endyq)
      
      consent_ts <- ts(consent$Spread, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
      
      nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
      
      nasdaq_ts <- ts(nasdaq$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)
      
      
      # X_comb_df 
      X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
      # set colnames 
      colnames(X_comb_df) <- ADL_variables
      
      # perc_change_df_spliced
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time))
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4
      
      real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
      perc_change_df_spliced = perc_change_df[start_rownum:end_rownum,]
      
      text <- aggregate_output(GDPGrowth_ts, X_comb_df, RGDP_Data, 
                               perc_change_df, perc_change_df_spliced, real_values,
                               ADL_variables, example_startq, example_endq, example_fhorizon, covid_dummy, 
                               baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
      
      output$outlook_indicators <- renderText({
        
        indicators <- if(is.null(text$outlook_indicators)){
          "None"
        } else {
          paste(text$outlook_indicators, collapse = ", ")
        }
        return(paste("Indicators that forecast negative growth:", indicators))
      })
      
      output$poor_outlook <- renderText({
        return(text$outlook_message)
      })
      
      output$abnormal_high_indicators <- renderText({
        indicators <- if(is.null(text$abnormal$high_dev_indicators)){
          "None"
        } else {
          paste(text$abnormal$high_dev_indicators, collapse = ", ")
        }
        return(paste("Indicators with High Level Deviation:", indicators))
      })
      
      output$abnormal_med_indicators <- renderText({
        indicators <- if(is.null(text$abnormal$medium_dev_indicators)){
          "None"
        } else {
          paste(text$abnormal$medium_dev_indicators, collapse = ", ")
        }
        return(paste("Indicators with Medium Level Deviation:", indicators))
      })
      
      output$abnormal_message <- renderText({
        return(text$abnormal$message)
      })
      
    })
  
  ## OUTPUT MESSAGE
  

  ####################
  ## ROLLING WINDOW AR
  ####################
  
  ## MODEL 7 PLOT
  observeEvent(input$button7, {
    output$model7 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      h=1
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      #example_endyq = as.yearqtr("2005 Q1")
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      #end = as.yearqtr("2001 Q4")
      
      window_start_index = which(grepl(example_endyq, check$Time))
      window_start = check$Time[window_start_index+1]
      #window_start = example_endyq
      
      
      #window_start = as.yearqtr("2001 Q3")
      #window_start_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      #window_start = GDPGrowth_ts_df_sliced$Time[window_start_rownum+1] #how do i add one quarter to this bro
      
      window_length = (end - window_start) * 4 + 1
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + window_length]
      
      #start_rownum = which(grepl(example_startyq, check$Time))
      #end_rownum = which(grepl(example_endyq, check$Time))

      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4

      real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
      
      pred_df = rolling_window_adv(RGDP_Data, perc_change_df, window_start, covid_dummy, real_values, example_startyq, end, 1)
      
      ### full gdp data
      
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time)) 
      
      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4 + h
      
      full_GDP_growth = data.frame(check[row_start_slice:row_last_slice, 1:2])
      
      ## generating values for prediction graph
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time >= window_start) %>% 
        head(n = window_length) %>%
        mutate(new_growth_rate = pred_df$pred)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      #all_GDP_data, GDPGrowth_ts, example_startyq, example_endyq, window_start, window_length) (all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, window_start, window_length)
      predicted_data <- rbind(actual_values_graph_rolling(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, window_start, window_length)$training_p, predicted_test_values)
      
      
      # fanplot
      # extracting time column for predictions
      time_data <- full_GDP_growth %>%
        filter(Time >= example_endyq) %>%
        select(Time) %>%
        head(window_length+1)
      
      #mutate bounds of actual unpredicted data to be 0
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < example_endyq) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph_rolling(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, 
                                                  example_endyq, window_start, window_length)
      
      joining_value = actual_values$joining_value
      # original_data = actual_values$original_data %>%
      #   filter(Time <= end)
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmse
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe_rolling(rmsfe_df, joining_value, predictions, window_length)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
      fanplot_data <- rbind(data, rmsfe_data) %>%
        filter(Time >= example_endyq) %>%
        head(window_length+1)
      
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
      
      model_7 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = actual_values$original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_7)
    })
    
    ## MODEL 7 TABLE
    
    output$table7 <- DT::renderDataTable({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      h = 1
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      
      window_start_rownum = which(grepl(example_endyq, check$Time))
      
      window_start = check$Time[window_start_rownum + 1]
      
      window_length = (end - window_start) * 4 + 1
            
      start_rownum = which(grepl(example_startyq, check$Time))
      end_rownum = which(grepl(example_endyq, check$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + window_length]

      row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4
      
      real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
      
      X_df = rename_variable(input$select_rolling_ADL)
      
      pred_df = rolling_window_adv(RGDP_Data, perc_change_df, window_start, covid_dummy, real_values, example_startyq, end, h)
      #print(pred_df$pred)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > example_endyq) %>%
        head(n = window_length) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$pred) %>%
        select(Date, Predictions) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions'), digits=3)
      
      predictions
    })
    
    ## MODEL 7 DM TEST
    output$desc7 <- renderText({
      # example_startq = gsub(":", " ", input$year[1])
      # example_endq = gsub(":", " ", input$year[2])
      # example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      # example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      # start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      # start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      # end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      # end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      # 
      # 
      # ###################
      # # NEW CONTENT HERE
      # ###################
      # 
      # GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      # GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      # all_GDP_data <- GDP_prep$all_GDP_data
      # spliced_GDP <- GDP_prep$spliced_GDP
      # sliced_perc_change <- GDP_prep$sliced_perc_change
      # perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      # 
      # #h = as.numeric(input$h)
      # 
      # covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      # 
      # # Dummy if timeframe ends on 2020 Q2, start of covid
      # if (example_startyq <= covid_start & example_endyq == covid_start){
      #   index = (covid_start - example_startyq) * 4 + 1
      #   covid_dummy[index] = -1
      # }
      # 
      # # Dummy if timeframe includes all of covid
      # if (example_startyq <= covid_start & example_endyq >= covid_end){
      #   index = (covid_start - example_startyq) * 4 + 1
      #   covid_dummy[index] = -1
      #   covid_dummy[index + 1] = 1
      # }
      # 
      # 
      # covid_dummy_ts <- ts(covid_dummy,
      #                      start = c(start_y, start_q), 
      #                      end = c(end_y, end_q), 
      #                      frequency = 4)
      # 
      # 
      # edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
      #   mutate(Time = as.yearqtr(Time)) %>%
      #   mutate(growth_rate = as.numeric(growth_rate))
      # 
      # all_GDP_ts <- ts(all_GDP_data, 
      #                  start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
      #                  end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
      #                  frequency = 4)
      # 
      # all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
      #   rename("Time" = "time") %>%
      #   rename("growth_rate" = "value")
      # 
      # all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      # 
      # GDPGrowth_ts_df_sliced <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
      #   rename("Time" = "time") %>%
      #   rename("growth_rate" = "value")
      # 
      # 
      # window_end_str = input$rolling_ADL
      # end = as.yearqtr(gsub(":", " ", window_end_str))
      # 
      # window_start = example_endyq
      # 
      # start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      # end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      # 
      # row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
      # row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4
      # 
      # real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
      # perc_change_df_spliced = perc_change_df[start_rownum:end_rownum,]
      # 
      # pred1 = rolling_window(RGDP_Data, perc_change_df_spliced, window_start, covid_dummy, real_values, example_startyq, example_endyq)$pred
      # pred2 = rolling_window_adv(RGDP_Data, window_start, covid_dummy, real_values, example_startyq, end, h)$pred
      # 
      # pval = dm_test(real_values, pred1, pred2, example_startyq, end)
      # 
      # paste(pval)
    })
  })
  
  
  ####################
  ## ROLLING WINDOW ADL
  ####################
  
  ## MODEL 8 PLOT
  
  observeEvent(input$button8, {
    output$model8 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      #example_endyq = as.yearqtr("2005 Q1")
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      #h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      #end = as.yearqtr("2020 Q1")
      
      window_start = example_endyq
      
      #window_start_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      #window_start = GDPGrowth_ts_df_sliced$Time[window_start_rownum+1] #how do i add one quarter to this bro

      window_length = (end - window_start) * 4 + 1
      
      #X_df = baa_aaa_ts
      X_df = rename_variable(input$select_rolling_ADL)
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + window_length]
      
      pred_df = rolling_window_adl(perc_change_df_spliced, X_df, window_start, covid_dummy, real_values, example_startyq, end)
      
      ## generating values for prediction graph
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > window_start) %>% 
        head(n = window_length) %>%
        mutate(new_growth_rate = pred_df$pred) %>% 
        head(n = window_length-1)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, example_startyq, example_endyq, window_length)$training_p, predicted_test_values)
      
      # fanplot
      # extracting time column for predictions
      time_data <- all_GDP_ts_df %>%
        filter(Time >= window_start) %>%
        select(Time) %>%
        head(window_length+1)
      
      #mutate bounds of actual unpredicted data to be 0
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < window_start) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, example_startyq, window_start, window_length)
      joining_value = actual_values$joining_value
      original_data = actual_values$original_data %>%
        filter(Time <= end)
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmse
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, window_length)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
      fanplot_data <- rbind(data, rmsfe_data) %>%
        filter(Time >= example_endyq) %>%
        head(window_length)
      
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
      
      model_8 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      
      print(model_8)
    })
    
 ## MODEL 8 TABLE
    
    output$table8 <- DT::renderDataTable({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      #h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      all_GDP_ts <- ts(all_GDP_data, 
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)
      
      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")
      
      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      
      window_start = example_endyq
      window_length = (end - window_start) * 4 + 1
      
      X_df = rename_variable(input$select_rolling_ADL)
      
      pred_df = rolling_window_adl(perc_change_df_spliced, X_df, window_start, covid_dummy, real_values, example_startyq, end)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > window_start) %>%
        head(n = window_length) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$pred) %>%
        select(Date, Predictions) %>% 
        filter(row_number() <= n()-1) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions'), digits=3)
      
      predictions
    })
    
  })
  
  ###############################
  ## ROLLING WINDOW COMBINED ADL
  ###############################
  
  ## MODEL 9 PLOT
  
  ###############################
  ## ROLLING WINDOW COMBINED ADL
  ###############################
  
  observeEvent(input$button9, {
    output$model9 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      #h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      # pred_df = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, window_start, covid_dummy, real_values, example_startyq, example_endyq)
   
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
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
      
      GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      #end = as.yearqtr("2020 Q1")
      
      window_start = example_endyq
      
      #window_start_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      #window_start = GDPGrowth_ts_df_sliced$Time[window_start_rownum+1] #how do i add one quarter to this bro
      
      window_length = (end - window_start) * 4 + 1
      
      start_rownum = which(grepl(example_startyq, GDPGrowth_ts_df_sliced$Time))
      end_rownum = which(grepl(example_endyq, GDPGrowth_ts_df_sliced$Time))
      
      start_plot = GDPGrowth_ts_df_sliced$Time[end_rownum - 10]
      end_plot = GDPGrowth_ts_df_sliced$Time[end_rownum + window_length]
      
      pred_df = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, example_endyq, covid_dummy, real_values, example_startyq, end)
      
      ## generating values for prediction graph
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > window_start) %>% 
        head(n = window_length) %>%
        mutate(new_growth_rate = pred_df$pred) %>% 
        head(n = window_length-1)
      
      predicted_test_values <- predictions %>%
        select(Time, new_growth_rate) %>% 
        rename("growth_rate" = "new_growth_rate") %>% 
        mutate(category = 2) 
      
      predicted_data <- rbind(actual_values_graph(all_GDP_data, GDPGrowth_ts, example_startyq, example_endyq, window_length)$training_p, predicted_test_values)
      
      # fanplot
      # extracting time column for predictions
      time_data <- all_GDP_ts_df %>%
        filter(Time >= window_start) %>%
        select(Time) %>%
        head(window_length+1)
      
      #mutate bounds of actual unpredicted data to be 0
      data <- GDPGrowth_ts_df_sliced %>%
        filter(Time < window_start) %>%
        select(Time) %>% 
        mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
      actual_values = actual_values_graph(all_GDP_data, GDPGrowth_ts, example_startyq, window_start, window_length)
      joining_value = actual_values$joining_value
      original_data = actual_values$original_data %>%
        filter(Time <= end)
      
      #rmsfe_test = fanplot_rmsfe(rmsfe_df_test, joining_value, predictions, h)
      rmsfe_df = pred_df$rmse
      
      rmsfe_data <- cbind(time_data, fanplot_rmsfe(rmsfe_df, joining_value, predictions, window_length)) 
      #rmsfe_data <- cbind(time_data, rmsfe_test)
      
      fanplot_data <- rbind(data, rmsfe_data) %>%
        filter(Time >= example_endyq) %>%
        head(window_length)
      
      
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
      
      model_9 <- ggplot() +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
        geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
        geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = "Prediction")) +
        geom_line(data = original_data, aes(x = Time, y = growth_rate, color = "True Value")) +
        geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#deafda", alpha = 0.3) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
        scale_x_yearqtr(format = '%Y Q%q')+ 
        labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP",
             color = "Legend") +  # Set the legend title
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid = element_blank(),
              panel.border = element_blank(),  # Remove panel border
              axis.line = element_line(color = "black"),
              plot.margin = margin(20,20,20,20))
      print(model_9)
    })
  
    
    ## MODEL 9 TABLE
    
    output$table9 <- DT::renderDataTable({
      
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      ###################
      # NEW CONTENT HERE
      ###################
      
      GDP_prep <- GDP_prep(RGDP_Data, example_startq, example_endq)
      GDPGrowth_ts <- GDP_prep$GDPGrowth_ts
      all_GDP_data <- GDP_prep$all_GDP_data
      spliced_GDP <- GDP_prep$spliced_GDP
      sliced_perc_change <- GDP_prep$sliced_perc_change
      perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
      
      #h = as.numeric(input$h)
      
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      # Dummy if timeframe ends on 2020 Q2, start of covid
      if (example_startyq <= covid_start & example_endyq == covid_start){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
      }
      
      # Dummy if timeframe includes all of covid
      if (example_startyq <= covid_start & example_endyq >= covid_end){
        index = (covid_start - example_startyq) * 4 + 1
        covid_dummy[index] = -1
        covid_dummy[index + 1] = 1
      }
      
      
      covid_dummy_ts <- ts(covid_dummy,
                           start = c(start_y, start_q), 
                           end = c(end_y, end_q), 
                           frequency = 4)
      
      edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(0,0,0,0)) %>%
        mutate(Time = as.yearqtr(Time)) %>%
        mutate(growth_rate = as.numeric(growth_rate))
      
      all_GDP_ts <- ts(all_GDP_data, 
                       start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                       end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                       frequency = 4)
      
      all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
        rename("Time" = "time") %>%
        rename("growth_rate" = "value")
      
      all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
      
      window_end_str = input$rolling_ADL
      end = as.yearqtr(gsub(":", " ", window_end_str))
      
      window_start = example_endyq
      
      window_length = (end - window_start) * 4 + 1

      pred_df = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, example_endyq, covid_dummy, real_values, example_startyq, end)
      
      predictions <- all_GDP_ts_df %>% 
        filter(Time > window_start) %>% 
        head(n = window_length) %>%
        mutate(Date = as.character(Time), Predictions = pred_df$pred) %>%
        select(Date, Predictions) %>% 
        filter(row_number() <= n()-1) %>% 
        datatable() %>% 
        formatRound(columns=c('Predictions'), digits=3)
      
      predictions
      
    })
  })
 
  


}
