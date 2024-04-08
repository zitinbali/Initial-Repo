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
library(fanplot)

RGDP_Data <- read_excel("Data/RGDP Data.xlsx")

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
    ")
  ),
           wellPanel("", value = "models", icon = NULL,
                     fluidPage(
                       chooseSliderSkin("Shiny"),
                       wellPanel(
                         sliderTextInput('year', 'Input time period', 
                                         choices = RGDP_Data$DATE[120:length(RGDP_Data$DATE)], #starting from 1976 Q4, the earliest start date all datasets have in common
                                         selected = c(RGDP_Data$DATE[140], RGDP_Data$DATE[200])),
                         #add in from_max to indicate start of test window
                         selectInput('h', 'Select Forecast Horizon (Number of Quarters ahead)', 
                                     choices = c("2", "3", "4"), 
                                     selected = "2", width = '50%'),
                         actionButton("show_prediction", "Show Prediction")
                       ),
                       mainPanel(
                         width = 14,
                         tabsetPanel(
                           tabPanel("Comparing Revised Values",
                                    icon = icon("calculator"),
                           wellPanel(
                             tabsetPanel(
                               tabPanel("Basic AR Model", plotOutput("model1"),
                                        textOutput("desc1")),
                               tabPanel("AR Model with Revised Values", plotOutput("model2"),
                                        textOutput("desc2"))
                           )
                           )
                         ),
                         tabPanel("Analysing Predictive Ability",
                                  icon = icon("chart-line"),
                                  wellPanel(
                                    tabsetPanel(
                                      tabPanel("ADL Model", plotOutput("model3"),
                                               textOutput("desc3")),
                                      tabPanel("Combined Model", plotOutput("model4"),
                                               textOutput("desc4")),
                                      tabPanel("ADL + Combined Model", plotOutput("model5"),
                                               textOutput("desc5"))
                                    )
                                  )
                         )
                       )
                     )
           )
  ),
)
  

server <- function(input, output, session) {
  
### AR Function & Model 
  
  # Using the basic AR model from lecture 
  # Inputs: Y - predicted variable, p - AR order, h - forecast horizon
  
  
  ##fitAR function for calculating predictions and models for aic
  fitAR=function(Y,p,h){
    
    # create p lags + forecast horizon shift (=h option)
    aux = embed(Y, p+h)
    
    #  Y variable aligned/adjusted for missing data due to lags
    y = aux[,1] 
    
    # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)
    X = as.matrix(aux[,-c(1:(ncol(Y)*h))])
    
    # retrieve last p observations
    X.out = tail(aux,1)[1:ncol(X)] 
    
    # estimate direct h-step AR(p) by OLS 
    model = lm(y~X) 
    
    # extract coefficients
    coef = coef(model) 
    
    #make a forecast using the last few observations: a direct h-step forecast.
    pred = c(1,X.out)%*%coef 
    
    # Get unadjusted RMSFE (ignoring estimation uncertainty)
    rmsfe = sqrt(sum(model$residuals^2) / nrow(X)) 
    
    # Save estimated AR regression, predictions, and estimated coefficients
    return(list("model" = model, "pred" = pred, "coefs" = coef)) 
  }
  
  test <- as.matrix(check$growth_rate)
  
  ##fitAR_preds for predictions at each step along forecast horizon h for line graph
  fitAR_preds <- function(Y, p, h) {
    preds = numeric(h)
    rmsfe = numeric(h)
    for(i in 1:h){
      #test_AR <- as.matrix(check$growth_rate)
      preds[i] = fitAR(test, 2, i)$pred  ##2 is placeholder for input$lags
      #rmsfe[i] = fitAR(test, 2, i)$residuals
    }
    return(list("preds" = preds))
  }
  
  #################
  ## MODEL 1 OUTPUT
  #################
  
  observeEvent(input$show_prediction, {output$model1 <- renderPlot({
  # formatting the data variable in terms of year and quarters
    
  training <- check %>%
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[1]))) %>%
    filter(Time <= as.yearqtr(gsub(":", " ", input$year[2]))) %>% #change to start year and end year inputs
    select(Time, growth_rate) %>%
    mutate(growth_rate = as.numeric(growth_rate)) %>%
    mutate(category = 1) 
    
  #training_xts <- xts(training$growth_rate, training$Time) 
  #check_xts <- xts(check$growth_rate, check$Time) 
  #test <- as.matrix(check$growth_rate)
  #print(c(fitAR_preds(test, 2, 2)))
  
  predictions <- check %>% 
    mutate(Time = as.yearqtr(Dates)) %>%
    filter(Time > as.yearqtr(gsub(":", " ", input$year[2]))) %>% 
    head(n = as.numeric(input$h)) %>%
    mutate(new_growth_rate = c(fitAR_preds(test, 3, as.numeric(input$h))$preds))
    #mutate(rmsfe = c(fitAR(test, 3, 2)$residuals))

    # Separate predictions into actual and predicted dataframes for plotting
    actual_test_values <- predictions %>% 
      select(Time, growth_rate) %>%
      mutate(category = 2)

    #actual_test_values_xts <- xts(actual_test_values$growth_rate, actual_test_values$Time)
    
    predicted_test_values <- predictions %>% 
      select(Time, new_growth_rate) %>% 
      mutate(category = 3) %>% 
      rename("growth_rate" = "new_growth_rate")
    
    original_data <- rbind(training, actual_test_values)
    predicted_data <- rbind(training, predicted_test_values)
    
    # creating data for fanplot
    predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
    
    fanplot_data <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(gsub(":", " ", input$year[2])))
      
    fanplot_rmsfe <- fitAR(test, 3, as.numeric(input$h))$model$residuals # replace w p and h
    data <- check[-c(1:(3+as.numeric(input$h)-1)),] # replace w p and h
    rmsfe <- sqrt(abs(fanplot_rmsfe))
    fanplot_data <- cbind(as.data.frame(rmsfe), data)
  
    ## creating dataframe for bounds 80% = 1.28, 50% = 0.67
    bound_data <- fanplot_data %>%
      mutate(upper_bound_80 = growth_rate + 1.28*rmsfe) %>%
      mutate(lower_bound_80 = growth_rate - 1.28*rmsfe) %>%
      mutate(upper_bound_50 = growth_rate + 0.67*rmsfe) %>%
      mutate(lower_bound_50 = growth_rate - 0.67*rmsfe) %>%
      filter(Time > as.yearqtr(gsub(":", " ", input$year[1]))) %>% #replace w start time
      filter(Time <= as.yearqtr(gsub(":", " ", input$year[2]))) #replace w end time
    
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
      filter(xmin >= as.yearqtr("1976 Q4") & xmax <= as.yearqtr(gsub(":", " ", input$year[2]))) #replace w start and end of lineplot
      
    model_1 <- ggplot() +
      geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
      geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
      scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FB5917"), 
                             limits = c(1, 3), guide = "none") +
      geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
      geom_ribbon(data = bound_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "yellow",  colour = "steelblue", alpha = 0.3) +
      geom_ribbon(data = bound_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "yellow3", colour = "steelblue", alpha = 0.3) +
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
  
  spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                             "2024 Q1", example_startyq, example_endyq, 3, 0)
  
  post_prep_gdp <- prep_func(spliced_GDP, 40)
  post_prep_gdp_df <- post_prep_gdp$df
  post_prep_gdp_delta = post_prep_gdp$delta

  sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                    "1965 Q4", "2024 Q1", 
                                    example_startq, example_endq, 2, 1)
  all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
                                example_startq, example_endq)
  
  advanced_AR_input <- as.matrix(all_GDP_data)
  
  
  advanced_AR_output <- fitAR(advanced_AR_input, 3, example_fhorizon) #p lags hardcoded, dummy covid removed for now

  ar2_prediction = advanced_AR_output$pred
  
  #################
  ## MODEL 2 OUTPUT
  #################
  
  observeEvent(input$show_prediction, {output$model2 <- renderPlot({
    
    # formatting the data variable in terms of year and quarters
    training <- check %>%
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time <= as.yearqtr(input$year[2])) %>% #change to start year and end year inputs
      select(Time, growth_rate) %>%
      mutate(growth_rate = as.numeric(growth_rate)) %>%
      mutate(category = 1) 
    
    #training_xts <- xts(training$growth_rate, training$Time) 
    #check_xts <- xts(check$growth_rate, check$Time) 
    #test <- as.matrix(check$growth_rate)
    #print(c(fitAR_preds(test, 2, 2)))
    
    predictions <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr(input$year[1])) %>% 
      #filter(Time > gsub(":", " ", input$year)) %>% 
      head(n = as.numeric(input$h)) %>%
      mutate(new_growth_rate = c(fitAR(advanced_AR_input, 3, as.numeric(input$h))$preds))
    #mutate(rmsfe = c(fitAR(test, 3, 2)$residuals))
    
    # Separate predictions into actual and predicted dataframes for plotting
    actual_test_values <- predictions %>% 
      select(Time, growth_rate) %>%
      mutate(category = 2)
    
    #actual_test_values_xts <- xts(actual_test_values$growth_rate, actual_test_values$Time)
    
    predicted_test_values <- predictions %>% 
      select(Time, new_growth_rate) %>% 
      mutate(category = 3) %>% 
      rename("growth_rate" = "new_growth_rate")
    
    original_data <- rbind(training, actual_test_values)
    predicted_data <- rbind(training, predicted_test_values)
    
    # creating data for fanplot
    predictions_actual_values_only <- predictions %>% select(Time, growth_rate)
    fanplot_data <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>%
      filter(Time > as.yearqtr("1970 Q1"))
    
    fanplot_rmsfe <- fitAR(test, 3, 2)$model$residuals # replace w p and h
    data <- check[-c(1:(3+as.numeric(input$h)-1)),] # replace w p and h
    rmsfe <- sqrt(abs(fanplot_rmsfe))
    fanplot_data <- cbind(as.data.frame(rmsfe), data)
    
    ## creating dataframe for bounds 80% = 1.28, 50% = 0.67
    bound_data <- fanplot_data %>%
      mutate(upper_bound_80 = growth_rate + 1.28*rmsfe) %>%
      mutate(lower_bound_80 = growth_rate - 1.28*rmsfe) %>%
      mutate(upper_bound_50 = growth_rate + 0.67*rmsfe) %>%
      mutate(lower_bound_50 = growth_rate - 0.67*rmsfe) %>%
      filter(Time > as.yearqtr("1970 Q1")) %>% #replace w start time
      filter(Time <= as.yearqtr("1970 Q3")) #replace w end time
    
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
      filter(xmin >= as.yearqtr("1950 Q1") & xmax <= as.yearqtr("2010 Q4")) #replace w start and end of lineplot
    
    model_2 <- ggplot() +
      geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
      geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
      scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FB5917"), 
                             limits = c(1, 3), guide = "none") +
      geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
      geom_ribbon(data = bound_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "yellow",  colour = "steelblue", alpha = 0.3) +
      geom_ribbon(data = bound_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "yellow3", colour = "steelblue", alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
      #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
      labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_blank(),
            panel.border = element_blank(),  # Remove panel border
            axis.line = element_line(color = "black"),
            plot.margin = margin(20,20,20,20))
    plot(model_2)
  })
  })
  
  
  output$model3 <- renderPlot({
    latest_data <- RGDP_Data$ROUTPUT24Q1
    
    # creating a lag for all quarters
    lag(latest_data)
    
    # check is a dataset to validate whether the data is stationary 
    check <- data.frame(RGDP_Data$DATE, (latest_data), lag(latest_data))
    
    check <- check %>% 
      rename(c("Date" = "RGDP_Data.DATE",
               "Raw Data" = "X.latest_data.",
               "First Lag" = "lag.latest_data."))
    
    # calculating growth rate of GDP from one quarter to the next
    check <- check[-1,] %>% 
      mutate(growth_rate = (`Raw Data` - `First Lag`)/(`First Lag`) * 100)
    
    # formatting the data variable in terms of year and quarters
    Dates <- gsub(":", " ", check$Date) 
    check <- check %>% 
      mutate(Time = as.yearqtr(Dates)) %>% 
      select(c(Time, growth_rate)) %>% 
      mutate(growth_rate = as.numeric(growth_rate))
    
    check_xts <- xts(check$growth_rate, check$Time) 
    
    plot(as.zoo(check_xts), 
         plot.type = "single", 
         col = c("darkred"),
         lwd = 1,
         xlab = "Date",
         ylab = "Growth Rate",
         main = "Quarterly Growth Rate of GDP")
    
    # function that transform years to class 'yearqtr'
    YToYQTR <- function(years){
      return(
        sort(as.yearqtr(sapply(years, paste, c("Q1", "Q2", "Q3", "Q4"))))
      )
    }
    
    # recessions
    recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991,
                            2001, 2007:2008))
    # the COVID recession ended in April 2020 according to the Fed
    recessions_covid <- append(recessions, 
                               c(as.yearqtr("2020 Q1"), 
                                 as.yearqtr("2020 Q2")))
    
    # colour shading for recessions
    xblocks(time(as.zoo(check_xts)), 
            c(time(check_xts) %in% recessions_covid), 
            col = alpha("steelblue", alpha = 0.3))
  })
  
  output$model2 <- renderPlot({
    plot(as.zoo(check_xts), 
         plot.type = "single", 
         col = c("darkred"),
         lwd = 1,
         xlab = "Date",
         ylab = "Growth Rate",
         main = "Quarterly Growth Rate of log(GDP)")
    
    # recessions
    recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991,
                            2001, 2007:2008))
    # the COVID recession ended in April 2020 according to the Fed
    recessions_covid <- append(recessions, 
                               c(as.yearqtr("2020 Q1"), 
                                 as.yearqtr("2020 Q2")))
    
    # colour shading for recessions
    xblocks(time(as.zoo(check_xts)), 
            c(time(check_xts) %in% recessions_covid), 
            col = alpha("steelblue", alpha = 0.3))
  })
  
  
}
  

shinyApp(ui = ui, server = server)