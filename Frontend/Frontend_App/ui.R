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
library(plotly)

RGDP_Data <- read_excel("Data/RGDP Data.xlsx")

navbarPage(
  theme = shinythemes::shinytheme('flatly'),
  title = "GDP Growth Rate Predictor",
  fluid = TRUE,
  tags$style(
    type="text/css", ".dataTables_filter {display: none;    }",
    HTML("
      .navbar-nav > li > a {
        padding-bottom: 15px;
      }
      .tabbable > .nav > li > a[data-value='AR Models'] {background-color: #9ba7a8;   color:white}
      .tabbable > .nav > li > a[data-value='ADL Models'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Aggregate Model'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Rolling Test Window'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Help'] {background-color: #9ba7a8;  color:white}
      .tabbable > .nav > li > a[data-value='Basic AR Model'] {background-color: transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Revised AR Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Individual ADL Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Combined ADL Model'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Add A Predictor'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Revised AR'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='ADL'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li > a[data-value='Combined ADL'] {background-color: #transparent;  color:#79818c; border: 1px solid #79818c}
      .tabbable > .nav > li[class=active]    > a {background-color: #5092cf; color: white; border: transparent}
    ")
  ),

  wellPanel("", value = "models", icon = NULL,
            fluidPage(
              chooseSliderSkin("Flat", color = "#787D99"),
              wellPanel(
                sliderTextInput('year', 'Select Time Period for Training', 
                                choices = RGDP_Data$DATE[120:length(RGDP_Data$DATE)], #starting from 1976 Q4, the earliest start date all datasets have in common
                                selected = c(RGDP_Data$DATE[140], RGDP_Data$DATE[200])),
                #add in from_max to indicate start of test window
                selectInput('h', 'Select Forecast Horizon (Number of Quarters ahead)', 
                            choices = c("1","2", "3", "4"), 
                            selected = "2", width = '50%'),
              ),
              mainPanel(
                width = 14,
                tabsetPanel(
                  tabPanel("AR Models",
                           icon = icon("calculator"),
                           helpText("Our Basic AR model serves as a benchmark. For the Revised AR model, we revise some of the data points before putting it through the model to improve accuracy. Compare the two models below!"),
                           wellPanel(
                             style = "background-color: #f8f9fa",
                             tabsetPanel(
                               type = "pills",
                               tabPanel("Basic AR Model",
                                        headerPanel(""),
                                        actionButton("button1", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        plotlyOutput("model1"),
                                        textOutput("desc1"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table1"),
                                        headerPanel(""), # adds space btwn text and inputs
                               ),
                               tabPanel("Revised AR Model",
                                        headerPanel(""),
                                        actionButton("button2", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        plotlyOutput("model2"),
                                        textOutput("desc2"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table2"),
                                        headerPanel(""), # adds space btwn text and inputs
                                        helpText("This model can be updated with new values every year, input values to add to the current dataset to simulate model predictions for 2024."), 
                                        div(style="display:inline-block", textInput("data1" ,"2024 Q1:")),
                                        div(style="display:inline-block", textInput("data2" ,"2024 Q2:")),
                                        div(style="display:inline-block", textInput("data3" ,"2024 Q3:")),
                                        div(style="display:inline-block", textInput("data4" ,"2024 Q4:")),
                                        actionButton("add_data", "Add Data and Make Prediction", style="background-color: #79818c"),
                                        plotlyOutput("model2a"),
                                        DT::dataTableOutput("table2a")
                               )
                             )
                           )
                  ),
                  tabPanel("ADL Models",
                           icon = icon("chart-line"),
                           helpText("We use a number of different indicators, as well as a combined model that leverages all of these indicators. You can also add your own indicator to see how it performs!"),
                           wellPanel(
                             style = "background-color: #f8f9fa",
                             tabsetPanel(
                               type = "pills", 
                               tabPanel("Individual ADL Model",
                                        headerPanel(""),
                                        selectInput("select_ADL", "Select ADL Predictors",
                                                    choices = c("BAA-AAA Spread", "Treasury Spread", "Housing Starts", "Consumer Sentiment", "NASDAQ Composite Index"),
                                                    selected = "BAA-AAA Spread"),
                                        actionButton("button3", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""), # adds space btwn text and inputs
                                        plotlyOutput("model3"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table3"),
                                        textOutput("desc3")
                               ),
                               
                               tabPanel("Combined ADL Model", 
                                        headerPanel(""),
                                        actionButton("button4", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        helpText("Please note that this model takes up to 1 minute to run. The model works, promise."),
                                        headerPanel(""),
                                        plotlyOutput("model4"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table4"),
                                        textOutput("desc4")
                               ),
                               
                               tabPanel("Add A Predictor", 
                                        headerPanel(""), # adds space btwn text and inputs
                                        fileInput("excel_data", "Upload a .xlsx or .xls file following the format detailed below.",
                                                  multiple = FALSE,
                                                  accept = c(".xlsx", ".xls")),
                                        helpText("The data should only have two columns, with the left being quarters formatted as “YYYY QQ” and the left being the GDP growth rates. Please ensure that there are NO column names. Data values should start from the first row."),
                                        actionButton("button5", "Generate ADL Model",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        plotlyOutput("model5"),
                                        DT::dataTableOutput("table5")
                               )
                             )
                           )
                  ),
                  tabPanel("Aggregate Model",
                           icon = icon("table"),
                           helpText("The aggregate model combines the advanced AR model, as well as the individual ADL models to deliver the most optimal forecast. We also check for anomalous market behaviour in this model."),
                           wellPanel(
                             style = "background-color: #f8f9fa",
                             tabsetPanel(
                               tabPanel("Optimised Forecast",
                                        headerPanel(""),
                               actionButton("button6", "Show Prediction",
                                            style="background-color: #79818c"),
                               headerPanel(""),
                               helpText("Please note that this model takes up to 3 minutes to run. The model works, promise."),
                               headerPanel(""),
                               plotlyOutput("model6"),
                               headerPanel(""),
                               DT::dataTableOutput("table6"),
                               HTML("<br>"),
                               #headerPanel(""), # adds space btwn text and inputs
                               #headerPanel(""), # adds space btwn text and inputs
                               textOutput("outlook_indicators"), 
                               HTML("<br>"), 
                               textOutput("poor_outlook"),
                               HTML("<br>"), 
                               #headerPanel(""), # adds space btwn text and inputs
                               textOutput("abnormal_high_indicators"),
                               textOutput("abnormal_med_indicators"),
                               HTML("<br>"), 
                               textOutput("abnormal_message")
                               )
                             )
                           )
                  ),
                  tabPanel("Rolling Test Window", 
                           icon = icon("plus"),
                           headerPanel(" "),
                           helpText("You can choose any value from the drop down as the end of the test window. Ensure that the end of your test window is after the end of the training window above. Please note that forecast horizon is not a relevant input for this feature, so the value does not matter. Additionally, a Diebold-Mariano test value will be generated at the bottom of the table for test windows of 4 or more quarters. Please refer to the Help tab for more."),
                           uiOutput("rolling_input"),
                           wellPanel(
                             style = "background-color: #f8f9fa",
                             tabsetPanel(
                               type = "pills",
                               tabPanel("Revised AR",
                                        headerPanel(""),
                                        actionButton("button7", "Show Prediction",
                                              style="background-color: #79818c"),
                                        headerPanel(""),
                                        plotlyOutput("model7"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table7"), 
                                        htmlOutput("desc7")
                               ),
                               tabPanel("ADL",
                                        headerPanel(""),
                                        selectInput("select_rolling_ADL", "Select ADL Predictors",
                                                    choices = c("BAA-AAA Spread", "Treasury Spread", "Housing Starts", "Consumer Sentiment", "NASDAQ Composite Index"),
                                                    selected = "BAA-AAA Spread"),
                                        actionButton("button8", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        plotlyOutput("model8"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table8"),
                                        htmlOutput("desc8")
                               ),
                               tabPanel("Combined ADL",
                                        headerPanel(""),
                                        actionButton("button9", "Show Prediction",
                                                     style="background-color: #79818c"),
                                        headerPanel(""),
                                        helpText("Please note that this model takes up to 1 minute to run. The model works, promise."),
                                        headerPanel(""),
                                        plotlyOutput("model9"),
                                        headerPanel(""),
                                        DT::dataTableOutput("table9"),
                                        htmlOutput("desc9")
                               )
                             )
                             
                           )
                  ),
                  
                  tabPanel("Help",
                           icon = icon("info"),
                           includeMarkdown("../../Backend/Help Page.Rmd")
                  )
                  
                )
              )
            )
  ),
)
