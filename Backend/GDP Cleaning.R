
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

# reading the GDP data
RGDP_Data <- read_excel("../Data/RGDP Data.xlsx")

# extracting the most revised values/recent data (2024 Q1) 
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
#### ADVANCED AR
#### MODEL FUNC
#####################

# n refers to the number of periods that sum up to 10 years for a given dataset. For instance, n = 40 for a dataset arranged by quarters, and n = 120 for a dataset arranged by months.

prep_func = function(dataset, n){
  
  nrow_dataset = nrow(dataset)
  
  # cut dataset into relevant ranges
  data_across_q <- dataset %>%
    mutate_all(as.numeric)
  
  colnames(data_across_q) <- NULL
  
  data_across_q <- as.matrix(data_across_q) 
  
  lagged_data_across_q <- lag(data_across_q)
  
  df <- 100 * (data_across_q - lagged_data_across_q)/(lagged_data_across_q)
  
  # Removing the first and last row since we had added those to compute the lag.
  df <- df[-c(1, nrow_dataset),]
  
  # Revised values is a data frame that shows the growth rate of each quarter as compared to the previous quarter, across revisions. For instance, revision0 is the initial growth rate for each quarter, and revision1 shows the growth rate after values have been revised. 
  
  revised_growth <- data.frame(matrix(NA, nrow = nrow_dataset - 2, ncol = 0))
  
  for (i in 0:n){
    na_vector = c(replicate(nrow_dataset - 2, NA)) 
    na_vector = as.numeric(na_vector)
    
    values <- diag(df)
    
    col_name = paste0("revision", i)
    
    na_vector[1:(nrow_dataset - 2 -i)] = values[1:(nrow_dataset - 2 -i)]
    revised_growth[[`col_name`]] = na_vector
    
    df <- df[,-c(1)]
  }
  
  revised_growth_df <- revised_growth
  
  # Now, we are looking into how the growth rates change due to revision
  lagged_growth <- revised_growth[, -c(ncol(revised_growth))]
  revised_growth <- revised_growth[,-c(1)]
  
  # We're taking the average of the change in growth per revision 
  change_in_growth <- 100*((revised_growth - lagged_growth)/lagged_growth)
  
  change_in_growth[sapply(change_in_growth, is.infinite)] <- NA
  
  change_in_growth <- change_in_growth %>% apply(2, mean, na.rm = TRUE)
  
  return(list("df" = revised_growth_df, "delta" = change_in_growth))
}





#####################
#### DATA SPLICING
#### FUNC
#####################

data_splice = function(data, row_start, row_end, col_start, col_end, 
                       window_start, window_end){
  
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
  
  output <- data[row_start_slice:row_last_slice, 
                 (col_start_slice+3):(col_last_slice + 1)]
  
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
window_start = "1970 Q4"
window_end = "2014 Q4"

revise_values = function(data, delta, window_end){

  window_end_yq = as.yearqtr(window_end)
  
  # Split dataframe into 10 years before target date and current value of growth for quarters more than 10 years ago
  ten_year_mark = (window_end_yq - 10 - as.yearqtr("1947 Q1", format = "%Y Q%q"))* 4
  end_of_row_mark = (window_end_yq - as.yearqtr("1947 Q1", format = "%Y Q%q")) * 4 - 1
  
  # Most recent values for start of time to 10 years before target date
  ancient_values <- data[1:ten_year_mark,][[`window_end`]]
  
  # All other values
  recent_values <- data[(ten_year_mark + 1):end_of_row_mark,][[`window_end`]]
  
  # Applying approximation of final growth numbers on recent values
  forecast_growth = recent_values 
  for (i in 1:length(recent_values)){
    for (j in 1:i){
      forecast_growth[i] = forecast_growth[i] * (1 + (delta[40 - j] / 100) )
    }
  }
  
  revised_data = c(ancient_values, forecast_growth)
  
  return (revised_data)
  
}




