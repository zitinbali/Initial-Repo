
source("inputs.R")

#####################
#### BASIC ANALYSIS
#####################

# load libraries

library(tidyverse)
library(ggplot2)
library(zoo)
library(forecast)
library(xts)
library(readxl)
library(dynlm)
library(lmtest)
library(stringr)
library(broom)
library(flexmix)

# reading the GDP data
RGDP_Data <- read_excel("../Data/RGDP Data.xlsx")

# extracting the most revised values/recent data (data as of 2024 Q1) 
latest_data <- RGDP_Data[, ncol(RGDP_Data)]

# creating a lag for all quarters
lag(latest_data)

# check is a dataset to first have a look at the data
check <- data.frame(RGDP_Data$DATE, (latest_data), lag(latest_data))

# renaming columns
colnames(check) <- c("Date", "Raw Data", "First Lag")

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

# Variance is higher in the earlier years prior to the GDP calculation methodology being changed 



#####################
#### BASIC CLEANING
#####################


# Formatting dataframe of quarters and their estimated values
working_df = RGDP_Data[, -c(1)] %>%
  mutate_all(as.numeric)

colnames(working_df) <- sapply(seq(as.Date(as.yearqtr("1965 Q4", format = "%Y Q%q")), by="quarter", length.out = ncol(working_df)), function(x) format(as.yearqtr(x), format = "%Y Q%q"))

rownames(working_df) <- sapply(seq(as.Date(as.yearqtr("1947 Q1", format = "%Y Q%q")), by="quarter", length.out = nrow(working_df)), function(x) format(as.yearqtr(x), format = "%Y Q%q"))

# Calculating growth rate of each quarter
lagged_working_df = lag(working_df)
perc_change_df = (100 * (working_df - lagged_working_df) / lagged_working_df)[-c(1),]



#####################
#### DATA SPLICING
#### FUNC
#####################

data_splice = function(data, row_start, row_end, col_start, col_end, 
                       window_start, window_end, n, m){
  
  ncol_data = ncol(data)
  nrow_data = nrow(data)
  
  row_start = as.yearqtr(row_start)
  row_end = as.yearqtr(row_end)
  col_start = as.yearqtr(col_start)
  col_end = as.yearqtr(col_end)
  window_start = as.yearqtr(window_start)
  window_end = as.yearqtr(window_end)
  
  row_start_slice = (window_start - row_start)*4
  row_last_slice = nrow_data - (row_end - window_end)*4
  
  col_start_slice = (window_start - col_start)*4
  col_last_slice = ncol_data - (col_end - window_end)*4
  
  output <- data[(row_start_slice + m):row_last_slice, 
                 (col_start_slice+n):(col_last_slice + 1)]
  
  return (output)
}




#####################
#### REVISING 
#### VALUES FUNC
#####################

row_start = "1947 Q1"
row_end = "2023 Q4"
col_start = "1965 Q4"
col_end = "2024 Q1"
window_start = "2004 Q3"
window_end = "2014 Q4"

revise_values = function(data, delta, window_start, window_end){

  window_start_yq = as.yearqtr(window_start)
  window_end_yq = as.yearqtr(window_end)
  
  # Split dataframe into 10 years before target date and current value of growth for quarters more than 10 years ago
  ten_year_mark = (window_end_yq - 10 - window_start_yq)*4 + 1
  end_of_row_mark = (window_end_yq - window_start_yq)*4 + 1
  
  end_qtr = as.character(window_end_yq + 1/4)
  
  # Most recent values for start of time to 10 years before target date
  ancient_values <- data[1:ten_year_mark,][[`end_qtr`]]
  
  # All other values
  recent_values <- data[(ten_year_mark + 1):end_of_row_mark,][[`end_qtr`]]
  
  # Applying approximation of final growth numbers on recent values
  forecast_growth = recent_values 
  for (i in 1:length(recent_values)){
    if (forecast_growth[i] < 0){
      for (j in 1:i){
        forecast_growth[i] = forecast_growth[i] + (1 / (1 + exp(-abs(forecast_growth[i]))) - 0.5) * delta[41 - j]
      }
    }
  }
  
  revised_data = c(ancient_values, forecast_growth)
  
  return (revised_data)
  
}


##############
# GDP prep
##############

spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                           "2024 Q1", example_startyq, example_endyq, 3, 0)

# revise GDP values

# note that the last input should be in a string format
sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                  "1965 Q4", "2024 Q1", 
                                  example_startq, example_endq, 2, 1)

#all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
#example_startq, example_endq)

all_GDP_data <- sliced_perc_change[, ncol(sliced_perc_change)]

GDPGrowth_ts <- ts(all_GDP_data, 
                   start = c(start_y, start_q), 
                   end = c(end_y, end_q), 
                   frequency = 4)



