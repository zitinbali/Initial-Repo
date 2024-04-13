source("GDP Cleaning.R")
source("inputs.R")
source("ADL Data.R")
source("AR_Model_Functions.R")
source("ADL Functions.R")
source("Combined ADL Functions.R")

##########################
# Poor Outlook Function
##########################

# This function analyses how many of our ADL predictions until and including 
# the forecast horizon are less than 0. i.e. Predicting a negative GDP Growth Rate. 


# X variables is a vector; input would be ADL variables
poor_outlook <- function(Y_ts, X_variables, start, end, f_horizon, dum){
  
  m <- length(X_variables)
  
  forecast_output <- c() 
  
  for (i in 1:m){
    X_temp <- get(X_variables[i])
    forecast_h <- (ADL_predict_all(Y_ts, X_temp, start, end, f_horizon, dum))$predictions
    for (j in 1:f_horizon){
      forecast <- forecast_h[j]
      forecast_output <- append(forecast_output, forecast)
    }
  }
  
  # what is the number of forecasts that are less than 0
  num_poor_forecast <- sum(forecast_output < 0)
  
  # identifying which indicators show a poor forecast
  
  # indexes of the indicators predicting GDP growth rate < 0
  less_than_zero <- which(forecast_output < 0)
  less_than_zero <- ifelse(less_than_zero > 5, less_than_zero %% 5, less_than_zero)
  less_than_zero[less_than_zero == 0] <- 5
  
  # names of unique indicators which predict GDP growth rate < 0
  less_than_zero <- unique(less_than_zero)
  indicators_poor <- X_variables[less_than_zero]
  
  if (is_empty(unique(indicators_poor))){
    output_message = "None of the ADL predictors forecast a negative GDP growth rate."
    indicators_output = NULL
  } else {
    output_message = "Some of the ADL predictors forecast a negative GDP growth rate."
    indicators_output = unique(indicators_poor)
  }
  
  return(list("message" = output_message, "indicators" = indicators_output))
}


##########################
# Abnormalities Function
##########################

# How many of the indicators have been behaving weirdly in the last 4 quarters? 
# This would indicate an imminent crisis. 
# 
# The function returns a list comprising a message and the indicators that 
# are behaving abnormally (if any). If the indicators are behaving normally, 
# the output for indicators will be NULL.


abnormal <- function(X_variables){
  
  m <- length(X_variables)
  high_dev_indicators <- c()
  medium_dev_indicators <- c()
  
  for (i in 1:m){
    X_temp <- get(X_variables[i])
    name_indicator <- X_variables[i]
    
    # median of the X variable
    median_value <- median(X_temp)
    
    # calculate the median absolute deviation (MAD)
    mad_value <- mad(X_temp)
    
    # calculate the absolute deviations from the median
    absolute_deviations <- abs(X_temp - median_value)
    
    # subset last 4 entries
    last_4_entries <- tail(X_temp, 4)
    
    # set the high threshold for outlier detection (3 times the MAD)
    threshold_high <- 3 * mad_value
    
    # identify major outliers 
    outliers_high <- X_temp[absolute_deviations > threshold_high]
    
    # set the medium threshold for outlier detection (2 times the MAD)
    threshold_medium <- 2 * mad_value
    
    # identify medium level outliers 
    outliers_medium <- X_temp[absolute_deviations > threshold_medium]
    
    # check if any of the outliers correspond to the last 4 entries
    if (any(last_4_entries %in% outliers_high)){
      high_dev_indicators <- append(high_dev_indicators, name_indicator)
    } 
    else if (any(last_4_entries %in% outliers_medium)){
      medium_dev_indicators <- append(medium_dev_indicators, name_indicator)
    }
  }
  
  if (length(high_dev_indicators) == 0 && length(medium_dev_indicators) == 0){
    output_message = "All good!"
  } 
  else {
    output_message = "Markets have been deviating from the norm significantly in the last year. If a recession or rapid economic boom is not already taking place, one might occur soon. A medium level deviation means the deviation is less severe but still significant, whereas a high level deviation is extremely serious. Conduct deeper analysis into the deviating predictors to arrive at more conclusive results."
  }
  
  return(list("message" = output_message, 
              "high_dev_indicators" = high_dev_indicators,
              "medium_dev_indicators" = medium_dev_indicators))
}


###############################
# Aggregate Forecast Function
###############################

# Aggregates the forecasts


# X_variables refers to a vector comprising the string names of all X variables.
# The input for X_variable is ADL_variables
aggregate_output <- function(Y_dataframe, X_variables, start, end, f_horizon, dum){
  
  forecast_output <- c() 
  
  ################################
  ### INSERT AGGREGATION FUNCTION
  ################################
  
  # placeholder
  mean_output = 1.0
  
  # call poor outlook function 
  poor_outlook <- poor_outlook(Y_dataframe, X_variables, start, end,
                               f_horizon, covid_dummy)
  
  # call abnormalities function 
  abnormal <- abnormal(X_variables)
  
  return(list("prediction" = mean_output, 
              "outlook" = poor_outlook, 
              "abnormal" = abnormal))
}
