library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readxl)
#install.packages("AICcmodavg")
library(AICcmodavg)


RGDP_Data <- read_excel("Data/RGDP Data.xlsx")

ui <- navbarPage(
  theme = shinythemes::shinytheme('flatly'),
  title = "Comparing Models",
  fluid = TRUE,
  tags$style(
    HTML("
      .navbar-nav > li > a {
        padding-top: 15px;
        padding-bottom: 15px;
      }
    ")
  ),
  tabPanel("", value = "models", icon = NULL,
           fluidPage(
             titlePanel("Comparing Models"),
             chooseSliderSkin("Flat"),
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput('input_year', 'Input Year', 
                                 choices = RGDP_Data$DATE,
                                 selected = RGDP_Data$DATE[4]),
                 selectInput('lags', 'Select number of lags', 
                             choices = c("1", "2", "3", "4"), 
                             selected = "2"),
                 selectInput('forecast_horizon', 'Select Forecast Horizon (Number of Quarters ahead)', 
                             choices = c("2", "3", "4"), 
                             selected = "2"),
                 checkboxGroupInput("combined_forecast", "Combined Forecast", 
                                    choices = list("AR Model" = 1, 
                                                   "Advance AR Model" = 2, 
                                                   "Model 3" = 3),
                                    selected = 1),
                 actionButton("show_prediction", "Show Prediction")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Model 1", plotOutput("model1"),
                            tableOutput("aic_table1")),
                   tabPanel("Model 2", plotOutput("model2"),
                            tableOutput("aic_table2")),
                   tabPanel("Model 3", plotOutput("model3"),
                            tableOutput("aic_table3"),
                            textOutput("best_model"))
               )
             )
           )
           )
  )
)

server <- function(input, output, session) {
  
  ### Basic Cleaning & Processing
  
  # extracting the most revised values/recent data (2024 Q1) 
  latest_data <- RGDP_Data$ROUTPUT24Q1
  
  # creating a lag for all quarters
  lag(latest_data)
  
  # check is a dataset to validate whether the data is stationary 
  check <- data.frame(RGDP_Data$DATE, log(latest_data), lag(log(latest_data)))
  
  check <- check %>% 
    rename(c("Date" = "RGDP_Data.DATE",
             "Raw Data" = "log.latest_data.",
             "First Lag" = "lag.log.latest_data.."))
  
  # calculating growth rate of log(GDP) from one quarter to the next
  check <- check[-1,] %>% 
    mutate(growth_rate = (log(`Raw Data`) - log(`First Lag`))/log(`First Lag`) * 100)
  
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
       main = "Quarterly Growth Rate of log(GDP)")
  
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
  
  ### AR Function & Model 
  
  # Using the basic AR model from lecture 
  # Inputs: Y - predicted variable, p - AR order, h - forecast horizon
  
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
    
    #note the addition of a constant to the test observation vector
    
    #get unadjusted rmsfe (ignoring estimation uncertainty)
    rmsfe = sqrt(sum(model$residuals^2)/nrow(X)) 
    
    #save estimated AR regression, prediction, and estimated coefficients
    #return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) 
    return(model)
  }
  
  ## prepping data for model 1
  test <- as.matrix(check$growth_rate)
  
  models <- lapply(1:4, function(p) fitAR(test, p, 2))
  
  aic <- aictab(cand.set = models, modnames = paste0('p=', 1:4))
  
  output$model1 <- renderPlot({
    plot(fitAR(test, 2, 2))
  })

  output$aic_table1 <- renderTable({
    aic[,c(1, 3)]
  })
  
  output$model2 <- renderPlot({
    plot(fitAR(test, 2, 2))
  })
  
  output$aic_table2 <- renderTable({
    aic[,c(1, 3)]
  })
  
  output$model3 <- renderPlot({
    plot(fitAR(test, 2, 2))
  })
  
  output$aic_table3 <- renderTable({
    aic[,c(1, 3)]
  })
  
  # Render the best model
  output$best_model <- renderText({
    paste("The best model given this data is")
  })
}

shinyApp(ui = ui, server = server)