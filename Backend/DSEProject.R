library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(xts)
library(AICcmodavg)
library(fresh)
library(RColorBrewer)
library(dynlm)

RGDP_Data <- read_excel("../Data/RGDP Data.xlsx")

source("GDP Cleaning.R")
source("inputs.R")
source("ADL Data.R")
source("AR_Model_Functions.R")
source("ADL Functions.R")
source("Combined ADL Model Functions.R")

ui <- navbarPage(
  theme = shinythemes::shinytheme('flatly'),
  title = "GDP Growth Rate Predictor",
  fluid = TRUE,
  tags$style(
    HTML("
      .navbar-nav > li > a {
        padding-top: 15px;
        padding-bottom: 15px;
      }
      .tabbable > .nav > li > a[data-value='Comparing Revised Values'] {background-color: #9ba7a8;   color:white}
      .tabbable > .nav > li > a[data-value='Analysing Predictive Ability'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Add A Predictor'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Basic AR Model'] {background-color: transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='AR Model with Revised Values'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Individual ADL Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Combined ADL Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Aggregate Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li[class=active]    > a {background-color: #5092cf; color: white; border: transparent}
    ")
  ),
           wellPanel("", value = "models", icon = NULL,
                     fluidPage(
                       chooseSliderSkin("Flat", color = "#787D99"),
                       wellPanel(
                         sliderTextInput('year', 'Input time period', 
                                         choices = RGDP_Data$DATE[120:length(RGDP_Data$DATE)], #starting from 1976 Q4, the earliest start date all datasets have in common
                                         selected = c(RGDP_Data$DATE[140], RGDP_Data$DATE[200])),
                         #add in from_max to indicate start of test window
                         selectInput('h', 'Select Forecast Horizon (Number of Quarters ahead)', 
                                     choices = c("1","2", "3", "4"), 
                                     selected = "2", width = '50%'),
                         actionButton("show_prediction", "Show Prediction",
                                      style="background-color: #79818c")
                       ),
                       mainPanel(
                         width = 14,
                         tabsetPanel(
                           tabPanel("Comparing Revised Values",
                                    icon = icon("calculator"),
                           wellPanel(
                             style = "background-color: #f8f9fa",
                             tabsetPanel(
                               type = "pills",
                               tabPanel("Basic AR Model",
                                        plotOutput("model1"),
                                        textOutput("desc1"),
                                        headerPanel(""), # adds space btwn text and inputs
                                        helpText("This model can be updated with new values every year, input values to add to the current dataset to simulate model predictions for 2024."), 
                                        div(style="display:inline-block", textInput("data1" ,"2024 Q1:")),
                                        div(style="display:inline-block", textInput("data2" ,"2024 Q2:")),
                                        div(style="display:inline-block", textInput("data3" ,"2024 Q3:")),
                                        div(style="display:inline-block", textInput("data4" ,"2024 Q4:")),
                                        actionButton("add_data", "Add Data and Make Prediction", style="background-color: #79818c")
                                        ),
                               tabPanel("AR Model with Revised Values", 
                                        plotOutput("model2"),
                                        textOutput("desc2"))
                           )
                           )
                         ),
                         tabPanel("Analysing Predictive Ability",
                                  icon = icon("chart-line"),
                                  wellPanel(
                                    style = "background-color: #f8f9fa",
                                    tabsetPanel(
                                      type = "pills", 
                                      tabPanel("Individual ADL Model", 
                                               headerPanel(""), # adds space btwn text and inputs
                                               selectInput("select_ADL", "Select ADL Predictors",
                                                           choices = c("BAA-AAA Spread", "Treasury Spread", "Housing Starts", "Consumer Sentiment", "NASDAQ Composite Index"),
                                                           selected = "BAA-AAA Spread"),
                                               headerPanel(""), # adds space btwn text and inputs
                                               plotOutput("model3"),
                                               textOutput("desc3")),
                                      
                                      tabPanel("Combined ADL Model", plotOutput("model4"),
                                               textOutput("desc4")),

                                      tabPanel("Aggregate Model", plotOutput("model5"),
                                               textOutput("poor_outlook"),
                                               htmlOutput("abnormal_indicators"),
                                               textOutput("abnormal_message")
                                               ),
                                    )
                                  )
                         ),
                         tabPanel("Add A Predictor",
                                  icon = icon("table"),
                                  wellPanel(
                                    style = "background-color: #f8f9fa",
                                    tabsetPanel(
                                      fileInput("excel_data", "Upload a .xlsx file following the sample format.",
                                                multiple = FALSE,
                                                accept = c(".xlsx")),
                                      helpText("The data should only have two columns, with the left being quarters formatted as “YYYY QQ” and the left being the GDP growth rates. Feel free to refer to the sample file as necessary"),
                                      downloadButton("download_data", "Download a Sample File",
                                                     style="background-color: #79818c"),
                                      actionButton("show_ADL", "Generate ADL Model",
                                                   style="background-color: #79818c"),
                                      plotOutput("model6")
                                    )
                                  )
                         )
                       )
                     )
           )
  ),
)
  

server <- function(input, output, session) {
  
  #source("GDP Cleaning.R")
  #source("inputs.R")
  
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
  
 ############## 
 ## fitAR PREP 
 ##############
   
  # Using the basic AR model from lecture 
  # Inputs: Y - predicted variable, p - AR order, h - forecast horizon
  
  
  ##fitAR function for calculating predictions and models for aic
  fitAR=function(Y,h, dum){
    
    minimum = Inf
    
    for (p in 1:4){
      # create p lags + forecast horizon shift (=h option)
      aux = embed(Y, p+h)
      
      #  Y variable aligned/adjusted for missing data due to lags
      y = aux[,1] 
      
      # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)
      X = as.matrix(aux[,-c(1:(ncol(Y)*h))])
      
      # retrieve last p observations
      X.out = tail(aux,1)[1:ncol(X)] 
      
      # cutting dummy to shape
      dum = tail(dum, length(y))
      
      # estimate direct h-step AR(p) by OLS 
      model = lm(y~X+dum) 
      
      # extract coefficients
      coef = coef(model)[1:(ncol(X)+1)]
      
      #make a forecast using the last few observations: a direct h-step forecast.
      pred = c(1,X.out)%*%coef 
      
      #note the addition of a constant to the test observation vector
      
      #get unadjusted rmsfe (ignoring estimation uncertainty)
      rmsfe = sqrt(sum(model$residuals^2)/nrow(X))
      aic = AIC(model)
      
      if(aic < minimum){
        minimum = aic
        best_rmsfe = rmsfe
        best_p = p
        best_model = model
        best_pred = pred
        best_coef = coef
      }
    }
    #save estimated AR regression, prediction, and estimated coefficients
    return(list("model"=best_model,"pred"=best_pred,"coef"=best_coef, "rmsfe" = best_rmsfe, "aic"=minimum, "p" = best_p)) 
  }

  
  ##fitAR_preds for predictions at each step along forecast horizon h for line graph
  fitAR_preds <- function(Y, h, dum) {
    preds = numeric(h)
    for(i in 1:h){
      #test_AR <- as.matrix(check$growth_rate)
      preds[i] = fitAR(Y, i, dum)$pred  
    }
    return(preds)
  }
  
  #################
  ## MODEL 1 OUTPUT
  #################
  
  observeEvent(input$show_prediction, {
    output$model1 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      test <- as.matrix(check$growth_rate)
      
      covid = c("2020 Q2", "2020 Q3")
      covid_start = as.yearqtr(covid[1])
      covid_end = as.yearqtr(covid[2])
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      
      # Timeframe cannot start from during covid or after
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
        
  # formatting the data variable in terms of year and quarters
  
  start_rownum = which(grepl(as.yearqtr(gsub(":", " ", input$year[1])), check$Time))
  end_rownum = which(grepl(as.yearqtr(gsub(":", " ", input$year[2])), check$Time))
  
  basic_AR_input <- check[start_rownum:end_rownum, ] %>% 
    select(growth_rate) %>% 
    as.matrix()
  
  start_plot = check$Time[end_rownum - 10]
  
  h = as.numeric(input$h)
  
  p = as.numeric(fitAR(basic_AR_input, h, covid_dummy)$p)
      
  training <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[1]))) %>%
    filter(Time < as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    tail(n = 9) %>%
    select(Time, growth_rate) %>%
    mutate(growth_rate = as.numeric(growth_rate)) %>%
    mutate(category = 1) 
  
  joining_value <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time == as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    select(Time, growth_rate) %>%
    mutate(growth_rate = as.numeric(growth_rate)) %>%
    mutate(category = 3) 

  training_t <- bind_rows(training, joining_value) %>%
    mutate(category = 1) 
  
  training_p <- bind_rows(training, joining_value)
  
  predictions <- check %>% 
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    head(n = h) %>%
    mutate(new_growth_rate = c(fitAR_preds(basic_AR_input, h, covid_dummy)))

    # Separate predictions into actual and predicted dataframes for plotting
    actual_test_values <- predictions %>% 
      select(Time, growth_rate) %>%
      mutate(category = 2)

    predicted_test_values <- predictions %>%
      select(Time, new_growth_rate) %>% 
      rename("growth_rate" = "new_growth_rate") %>% 
      #l_join(joining_value, by = "growth_rate") %>%
      mutate(category = 3) 
      
    
    original_data <- rbind(training_t, actual_test_values)
    predicted_data <- rbind(training_p, predicted_test_values)
    
    # creating data for fanplot
    predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
    
      
    fanplot_rmsfe <- function(full_df, input_df, predictions, h) {
      predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                      upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
      predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
        
      for(i in 2:(h+1)){
        rmsfe = fitAR(input_df, i, covid_dummy_ts)$rmsfe 
        predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate + 1.28*rmsfe
        predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate - 1.28*rmsfe
        predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate + 0.67*rmsfe
        predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate - 0.67*rmsfe
      }
        return(predictions_rmsfe)
    }
    
    ##create fanplot dataframe with time column
    time_data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time >= as.yearqtr(gsub(":", " ", input$year[2]))) %>%
      select(Time) 
    
    data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time < as.yearqtr(gsub(":", " ", input$year[2]))) %>%
      select(Time) %>% 
      mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
      
    rmsfe_data <- cbind(time_data, fanplot_rmsfe(check, basic_AR_input, predictions, h)) 
    
    fanplot_data <- rbind(data, rmsfe_data) %>%
      filter(Time >= as.yearqtr(gsub(":", " ", input$year[2]))) %>%
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
      geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
      geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
      scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FF0000"), 
                             limits = c(1, 3), guide = "none") +
      geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
      #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
      labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_blank(),
            panel.border = element_blank(),  # Remove panel border
            axis.line = element_line(color = "black"),
            plot.margin = margin(20,20,20,20))
    plot(model_1)
  })
})
  
  
  #########################
  ## Advanced AR Model Prep
  #########################
  
 
  #################
  ## MODEL 2 OUTPUT
  #################

  observeEvent(input$show_prediction, {
    output$model2 <- renderPlot({
      example_startq = gsub(":", " ", input$year[1])
      example_endq = gsub(":", " ", input$year[2])
      example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
      example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
      start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
      start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
      end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
      end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
      
      
      covid = c("2020 Q2", "2020 Q3")
      covid_start = as.yearqtr(covid[1])
      covid_end = as.yearqtr(covid[2])
      covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
      
      
      # Timeframe cannot start from during covid or after
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
      
        #####################
        ## revise GDP values
        #####################
      
      spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                                 "2024 Q1", as.yearqtr(gsub(":", " ", input$year[1])), as.yearqtr(gsub(":", " ", input$year[2])), 3, 0)
      
      post_prep_gdp <- prep_func(spliced_GDP, 40)
      post_prep_gdp_df <- post_prep_gdp$df
      post_prep_gdp_delta = post_prep_gdp$delta
      
      sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                        "1965 Q4", "2024 Q1", 
                                        gsub(":", " ", input$year[1]), gsub(":", " ", input$year[2]), 2, 1)
      all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
                                    gsub(":", " ", input$year[1]), gsub(":", " ", input$year[2]))
    
  #dummy = covid_dum(as.yearqtr(gsub(":", " ", input$year[1])), as.yearqtr(gsub(":", " ", input$year[2])))
  
  #start_rownum = which(grepl(as.yearqtr(gsub(":", " ", input$year[1])), check$Time))
  #end_rownum = which(grepl(as.yearqtr(gsub(":", " ", input$year[2])), check$Time))
  advanced_AR_input <- as.matrix(all_GDP_data)
  
  h = as.numeric(input$h)
  
  start_plot = check$Time[end_rownum - 10]
  
  training <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[1]))) %>%
    filter(Time <= as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    tail(10) %>% 
    select(Time, growth_rate) %>%
    mutate(growth_rate = as.numeric(growth_rate)) %>%
    mutate(category = 1) 
  
  joining_value <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time == as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    select(Time, growth_rate) %>%
    mutate(growth_rate = as.numeric(growth_rate)) %>%
    mutate(category = 3) 

  training_t <- bind_rows(training, joining_value) %>%
    mutate(category = 1) 
  
  training_p <- bind_rows(training, joining_value)
  
  predictions <- check %>% 
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    head(n = h) %>%
    mutate(new_growth_rate = c(fitAR_preds(advanced_AR_input, h, covid_dummy)))
  
  # Separate predictions into actual and predicted dataframes for plotting
  actual_test_values <- predictions %>% 
    select(Time, growth_rate) %>%
    mutate(category = 2)
  
  predicted_test_values <- predictions %>%
    select(Time, new_growth_rate) %>% 
    rename("growth_rate" = "new_growth_rate") %>% 
    #l_join(joining_value, by = "growth_rate") %>%
    mutate(category = 3) 
  
  
  original_data <- rbind(training_t, actual_test_values)
  predicted_data <- rbind(training_p, predicted_test_values)
  
  # creating data for fanplot
  predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
  
  fanplot_rmsfe <- function(full_df, input_df, predictions, h) {
    predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                    upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
    predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
    predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
    predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
    predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
    
    for(i in 2:(h+1)){
      rmsfe = fitAR(input_df, i, covid_dummy)$rmsfe 
      predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate + 1.28*rmsfe
      predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate - 1.28*rmsfe
      predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate + 0.67*rmsfe
      predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate - 0.67*rmsfe
    }
    return(predictions_rmsfe)
  }
  
  ##create fanplot dataframe with time column
  time_data <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time >= as.yearqtr(gsub(":", " ", input$year[2]))) %>%
    select(Time) 
  
  data <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time < as.yearqtr(gsub(":", " ", input$year[2]))) %>%
    select(Time) %>% 
    mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
  
  rmsfe_data <- cbind(time_data, fanplot_rmsfe(check, advanced_AR_input, predictions, h)) 
  
  fanplot_data <- rbind(data, rmsfe_data) %>%
    filter(Time >= as.yearqtr(gsub(":", " ", input$year[2]))) %>%
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
    filter(xmin >= start_plot & xmax <= as.yearqtr(gsub(":", " ", input$year[2]))) #replace w range of lineplot
  
  model_2 <- ggplot() +
    geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
    geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
    scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FF0000"), 
                           limits = c(1, 3), guide = "none") +
    geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
    geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
    #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
    labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
    theme_minimal() +
    scale_x_yearqtr(breaks = seq(min(predicted_data$Time), max(predicted_data$Time), by = 1)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid = element_blank(),
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line(color = "black"),
          plot.margin = margin(20,20,20,20))
    
    plot(model_2)
  })
})
  
  
  #####################
  ## ADL functions
  #####################
  #source("Backend/GDP Cleaning.R")
  #source("Backend/ADL Data.R")
  #source("Backend/AR_Model_Functions.R")
  #source("Backend/ADL Functions.R")
  
observeEvent(input$show_prediction, {
  example_startq = gsub(":", " ", input$year[1])
  example_endq = gsub(":", " ", input$year[2])
  example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
  example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
  start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
  start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
  end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
  end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
  output$model3 <- renderPlot({
    
    covid = c("2020 Q2", "2020 Q3")
    covid_start = as.yearqtr(covid[1])
    covid_end = as.yearqtr(covid[2])
    covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
    
    
    # Timeframe cannot start from during covid or after
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

    
    ##############
    # Predictor data
    ##############
    ADL_splice <- function(data, window_start, window_end){
      start_rownum = which(grepl(window_start, data$Date))
      end_rownum = which(grepl(window_end, data$Date))
      
      output <- data[start_rownum:end_rownum+1, ]
      
      return(output)
    }
    
    
    baa_aaa <- read_excel("FRED BAA-AAA Data.xls", 
                          col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
    
    baa_aaa_ts <- ts(baa_aaa$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    
    
    tspread <- read_excel("FRED Treasury Spread.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    # NOTE: tspread is only from 1976 Q4 onward, so we can't accept forecast horizons earlier, at least not for this ADL model
    
    tspread <- ADL_splice(tspread, example_startyq, example_endyq)
    
    tspread_ts <- ts(tspread$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    fred_hstarts <- read_excel("FRED Hstarts.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    fred_hstarts <- ADL_splice(fred_hstarts, example_startyq, example_endyq)
    
    fred_hstarts_ts <- ts(fred_hstarts$Spread, 
                          start = c(start_y, start_q), 
                          end = c(end_y, end_q), 
                          frequency = 4)
    
    
    consent <- read_excel("FRED Consumer Sentiment.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    consent <- ADL_splice(consent, example_startyq, example_endyq)
    
    consent_ts <- ts(consent$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    nasdaq <- read_excel("NASDAQCOM.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
    
    nasdaq_ts <- ts(nasdaq$Spread, 
                    start = c(start_y, start_q), 
                    end = c(end_y, end_q), 
                    frequency = 4)
    
  
    
    ##############
    # GDP prep
    ##############
    
    spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                               "2024 Q1", as.yearqtr(gsub(":", " ", input$year[1])), as.yearqtr(gsub(":", " ", input$year[2])), 3, 0)
    
    post_prep_gdp <- prep_func(spliced_GDP, 40)
    post_prep_gdp_df <- post_prep_gdp$df
    post_prep_gdp_delta = post_prep_gdp$delta
    
    # revise GDP values
    
    # note that the last input should be in a string format
    ###################
      
    sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                      "1965 Q4", "2024 Q1", 
                                      example_startq, example_endq, 2, 1)
    all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
                                  example_startq, example_endq)
    
    GDPGrowth_ts <- ts(all_GDP_data, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
    
    GDPGrowth_ts_df <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
      rename("Time" = "time") %>%
      rename("growth_rate" = "value")
    
    start_rownum = which(grepl(as.yearqtr(example_startq), GDPGrowth_ts_df$Time))
    end_rownum = which(grepl(as.yearqtr(example_endq), GDPGrowth_ts_df$Time))
    #indiv_ADL_input <- as.matrix(all_GDP_data)
    
    h = as.numeric(input$h)
    
    rename_variable <- function(input_string) { 
      var = NULL
      if (input_string == "BAA-AAA Spread"){
        var = baa_aaa_ts
      }
      if (input_string == "Treasury Spread"){
        var = tspread_ts
      }
      if (input_string == "Housing Starts"){
        var = fred_hstarts_ts
      }
      if (input_string == "Consumer Sentiment"){
        var = consent_ts
      }
      if (input_string == "NASDAQ Composite Index"){
        var = nasdaq_ts
      }
      return(var)
    }
    
    X_dataframe = rename_variable(input$select_ADL) 
    
    ADL_preds <- function(GDPGrowth_ts, X_dataframe, h) {
      preds = numeric(h)
      rmsfe = numeric(h)
      for(i in 1:h){
        #test_AR <- as.matrix(check$growth_rate)
        adl_predicting = ADL_predict_all(GDPGrowth_ts, X_dataframe, i, covid_dummy_ts)
        preds[i] = adl_predicting$prediction
        rmsfe[i] = adl_predicting$rmsfe
        ##2 is placeholder for input$lags
      }
      return(list("preds" = preds, "rmsfe" = rmsfe))
    }
    
    ### graph plotting
    
    start_plot = GDPGrowth_ts_df$Time[end_rownum - 10]
    
    
    training <- check %>%
      #mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(example_startq)) %>%
      filter(Time <= as.yearqtr(example_endq)) %>% 
      tail(10) %>% 
      select(Time, growth_rate) %>%
      mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 1) 

    joining_value <- check %>%
      #mutate(Time = as.yearqtr(Time)) %>%
      filter(Time == as.yearqtr(example_endq)) %>% 
      select(Time, growth_rate) %>%
      mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 3) 
    
    training_t <- bind_rows(training, joining_value) %>%
      mutate(category = 1) 
    
    training_p <- bind_rows(training, joining_value)
    
    predictions <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
      head(n = h) %>%
      mutate(new_growth_rate = c(ADL_preds(GDPGrowth_ts, X_dataframe, h)$preds))
    
    # Separate predictions into actual and predicted dataframes for plotting
    actual_test_values <- predictions %>% 
      select(Time, growth_rate) %>%
      mutate(category = 2)
    
    predicted_test_values <- predictions %>%
      select(Time, new_growth_rate) %>% 
      rename("growth_rate" = "new_growth_rate") %>% 
      #l_join(joining_value, by = "growth_rate") %>%
      mutate(category = 3) 
    
    
    original_data <- rbind(training_t, actual_test_values)
    predicted_data <- rbind(training_p, predicted_test_values)
    
    # creating data for fanplot
    predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
    
    fanplot_rmsfe <- function(X_dataframe, predictions, h) {
      predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                      upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
      predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
      
      for(i in 2:(h+1)){
        rmsfe = ADL_preds(GDPGrowth_ts, X_dataframe, i)$rmsfe
        predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate + 1.28*rmsfe
        predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate - 1.28*rmsfe
        predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate + 0.67*rmsfe
        predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate - 0.67*rmsfe
      }
      return(predictions_rmsfe)
    }
    
    ##create fanplot dataframe with time column
    time_data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time >= as.yearqtr(example_endq)) %>%
      select(Time) %>%
      head(h+1)
    
    data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time < as.yearqtr(example_endq)) %>%
      select(Time) %>% 
      mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
    
    rmsfe_data <- cbind(time_data, fanplot_rmsfe(X_dataframe, predictions, h)) 
    
    fanplot_data <- rbind(data, rmsfe_data) %>%
      filter(Time >= as.yearqtr(example_endq)) %>%
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
      filter(xmin >= start_plot & xmax <= as.yearqtr(example_endq)) #replace w range of lineplot
    
    model_3 <- ggplot() +
      geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
      geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
      scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FF0000"), 
                             limits = c(1, 3), guide = "none") +
      geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
      #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
      labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
      theme_minimal() +
      scale_x_yearqtr(breaks = seq(min(predicted_data$Time), max(predicted_data$Time), by = 1)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_blank(),
            panel.border = element_blank(),  # Remove panel border
            axis.line = element_line(color = "black"),
            plot.margin = margin(20,20,20,20))
    
    plot(model_3)
    
  })
})
  
  
  ##################
    ## COMBINED MODEL
  ##################
#ADL_comb_predict_all <- function(Y_dataframe, X_combined_dataframe, f_horizon, end_yq)
observeEvent(input$show_prediction, {
  example_startq = gsub(":", " ", input$year[1])
  example_endq = gsub(":", " ", input$year[2])
  example_startyq = as.yearqtr(gsub(":", " ", input$year[1]))
  example_endyq = as.yearqtr(gsub(":", " ", input$year[2]))
  start_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[1]))))
  start_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[1]))))
  end_y = as.numeric(year(as.yearqtr(gsub(":", " ", input$year[2]))))
  end_q = as.numeric(quarter(as.yearqtr(gsub(":", " ", input$year[2]))))
  
  
  
  
  output$model4 <- renderPlot({
    
    covid = c("2020 Q2", "2020 Q3")
    covid_start = as.yearqtr(covid[1])
    covid_end = as.yearqtr(covid[2])
    covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
    
    # Timeframe cannot start from during covid or after
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
    
    ##############
    # Predictor data
    ##############
    ADL_splice <- function(data, window_start, window_end){
      start_rownum = which(grepl(window_start, data$Date))
      end_rownum = which(grepl(window_end, data$Date))
      
      output <- data[start_rownum:end_rownum+1, ]
      
      return(output)
    }
    
    
    baa_aaa <- read_excel("FRED BAA-AAA Data.xls", 
                          col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)
    
    baa_aaa_ts <- ts(baa_aaa$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    tspread <- read_excel("FRED Treasury Spread.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    # NOTE: tspread is only from 1976 Q4 onward, so we can't accept forecast horizons earlier, at least not for this ADL model
    
    tspread <- ADL_splice(tspread, example_startyq, example_endyq)
    
    tspread_ts <- ts(tspread$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    fred_hstarts <- read_excel("FRED Hstarts.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    fred_hstarts <- ADL_splice(fred_hstarts, example_startyq, example_endyq)
    
    fred_hstarts_ts <- ts(fred_hstarts$Spread, 
                          start = c(start_y, start_q), 
                          end = c(end_y, end_q), 
                          frequency = 4)
    
    
    consent <- read_excel("FRED Consumer Sentiment.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    consent <- ADL_splice(consent, example_startyq, example_endyq)
    
    consent_ts <- ts(consent$Spread, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
    
    nasdaq <- read_excel("NASDAQCOM.xls", col_names = c("Date", "Spread")) %>% 
      mutate(Date = as.yearqtr(Date), 
             Spread = as.numeric(Spread))
    
    nasdaq <- ADL_splice(nasdaq, example_startyq, example_endyq)
    
    nasdaq_ts <- ts(nasdaq$Spread, 
                    start = c(start_y, start_q), 
                    end = c(end_y, end_q), 
                    frequency = 4)
    
    
    
    ##############
    # GDP prep
    ##############
    
    spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                               "2024 Q1", as.yearqtr(gsub(":", " ", input$year[1])), as.yearqtr(gsub(":", " ", input$year[2])), 3, 0)
    
    post_prep_gdp <- prep_func(spliced_GDP, 40)
    post_prep_gdp_df <- post_prep_gdp$df
    post_prep_gdp_delta = post_prep_gdp$delta
    
    # revise GDP values
    
    # note that the last input should be in a string format
    ###################
    
    sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                      "1965 Q4", "2024 Q1", 
                                      example_startq, example_endq, 2, 1)
    all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
                                  example_startq, example_endq)
    
    GDPGrowth_ts <- ts(all_GDP_data, 
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
    
    GDPGrowth_ts_df <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
      rename("Time" = "time") %>%
      rename("growth_rate" = "value")
    
    start_rownum = which(grepl(as.yearqtr(example_startq), GDPGrowth_ts_df$Time))
    end_rownum = which(grepl(as.yearqtr(example_endq), GDPGrowth_ts_df$Time))
    #indiv_ADL_input <- as.matrix(all_GDP_data)
    
    h = as.numeric(input$h)
    
    X_combined_dataframe <- ts.union(baa_aaa_ts, tspread_ts, fred_hstarts_ts, consent_ts, nasdaq_ts)
    
    ADL_preds <- function(GDPGrowth_ts, X_combined_dataframe, h) {
      preds = numeric(h)
      rmsfe = numeric(h)
      for(i in 1:h){
        #test_AR <- as.matrix(check$growth_rate)
        adl_predicting = ADL_comb_predict_all(GDPGrowth_ts, X_combined_dataframe, i, example_endq)
        preds[i] = adl_predicting$prediction
        rmsfe[i] = adl_predicting$rmsfe
        ##2 is placeholder for input$lags
      }
      return(list("preds" = preds, "rmsfe" = rmsfe))
    }
    
    ### graph plotting
    
    start_plot = GDPGrowth_ts_df$Time[end_rownum - 10]
    
    
    training <- check %>%
      #mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(example_startq)) %>%
      filter(Time <= as.yearqtr(example_endq)) %>% 
      tail(10) %>% 
      select(Time, growth_rate) %>%
      mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 1) 
    
    joining_value <- check %>%
      #mutate(Time = as.yearqtr(Time)) %>%
      filter(Time == as.yearqtr(example_endq)) %>% 
      select(Time, growth_rate) %>%
      mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 3) 
    
    training_t <- bind_rows(training, joining_value) %>%
      mutate(category = 1) 
    
    training_p <- bind_rows(training, joining_value)
    
    predictions <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
      head(n = h) %>%
      mutate(new_growth_rate = c(ADL_preds(GDPGrowth_ts, X_combined_dataframe, h)$preds))
    
    # Separate predictions into actual and predicted dataframes for plotting
    actual_test_values <- predictions %>% 
      select(Time, growth_rate) %>%
      mutate(category = 2)
    
    predicted_test_values <- predictions %>%
      select(Time, new_growth_rate) %>% 
      rename("growth_rate" = "new_growth_rate") %>% 
      #l_join(joining_value, by = "growth_rate") %>%
      mutate(category = 3) 
    
    
    original_data <- rbind(training_t, actual_test_values)
    predicted_data <- rbind(training_p, predicted_test_values)
    
    # creating data for fanplot
    predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
    
    fanplot_rmsfe <- function(X_combined_dataframe, predictions, h) {
      predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                      upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
      predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
      predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
      predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
      
      for(i in 2:h+1){
        rmsfe = ADL_preds(GDPGrowth_ts, X_combined_dataframe, i)$rmsfe
        predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate + 1.28*rmsfe
        predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate - 1.28*rmsfe
        predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate + 0.67*rmsfe
        predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate - 0.67*rmsfe
      }
      return(predictions_rmsfe)
    }
    
    ##create fanplot dataframe with time column
    time_data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time >= as.yearqtr(example_endq)) %>%
      select(Time) %>%
      head(h+1)
    
    data <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time < as.yearqtr(example_endq)) %>%
      select(Time) %>% 
      mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)
    
    rmsfe_data <- cbind(time_data, fanplot_rmsfe(X_dataframe, predictions, h)) 
    
    fanplot_data <- rbind(data, rmsfe_data) %>%
      filter(Time >= as.yearqtr(example_endq)) %>%
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
      filter(xmin >= start_plot & xmax <= as.yearqtr(example_endq)) #replace w range of lineplot
    
    model_4 <- ggplot() +
      geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
      geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
      scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FF0000"), 
                             limits = c(1, 3), guide = "none") +
      geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
      geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
      #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
      labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
      theme_minimal() +
      scale_x_yearqtr(breaks = seq(min(predicted_data$Time), max(predicted_data$Time), by = 1)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_blank(),
            panel.border = element_blank(),  # Remove panel border
            axis.line = element_line(color = "black"),
            plot.margin = margin(20,20,20,20))
    
    plot(model_4)
    
  })
  
  
  ##################
  ## AGGREGATE MODEL
  ##################

})
  
}
  

shinyApp(ui = ui, server = server)