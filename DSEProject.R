library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(readxl)

RGDP_Data <- read_excel("RGDP Data.xlsx")

library(shiny)
library(shinythemes)

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
                   tabPanel("Model 1", plotOutput("model1")),
                   tabPanel("Model 2", plotOutput("model2")),
                   tabPanel("Model 3", plotOutput("model3"),
                          textOutput("best_model"))
               )
             )
           )
           )
  )
)


server <- function(input, output, session) {
  # Server logic goes here
}

shinyApp(ui = ui, server = server)


server <- function(input, output, session) {
  # Server logic goes here
}

shinyApp(ui = ui, server = server)


server <- function(input, output, session) {
  
  fitAR=function(Y,p,h){
    
    #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
    aux=embed(Y,p+h) #create p lags + forecast horizon shift (=h option)
    y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
    X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
    
    X.out=tail(aux,1)[1:ncol(X)] #retrieve last p observations 
    model=lm(y~X) #estimate direct h-step AR(p) by OLS 
    coef=coef(model) #extract coefficients
    pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
    #note the addition of a constant to the test observation vector
    
    rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
    
    return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
  }
  
  ## prepping data for model 1
  latest_data <- RGDP_Data$ROUTPUT24Q1
  # creating a lag for all quarters
  lag(latest_data)
  check <- data.frame(RGDP_Data$DATE, log(latest_data), lag(log(latest_data)))
  check <- check %>% 
    rename(c("Date" = "RGDP_Data.DATE",
             "Raw Data" = "log.latest_data.",
             "First Lag" = "lag.log.latest_data.."))
  check <- check[-1,] %>% 
    mutate(growth_rate = (`Raw Data` - `First Lag`)/`First Lag` * 100)
  test <- as.matrix(log(check$growth_rate))
  
  output$model1 <- renderPlot({
    fitAR(test, 2, 2) 
  })
  output$model2 <- renderPlot({
    ggplot(cars, aes(x = speed, y = dist)) +
      geom_point()
  })
  output$model3 <- renderPlot({
    ggplot(cars, aes(x = speed, y = dist)) +
      geom_point()
  })
  output$best_model <- renderText({
    paste("The best model given this data is")
  })
}

shinyApp(ui = ui, server = server)