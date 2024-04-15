
##########################
# Poor Outlook Function
##########################

# This function analyses how many of our ADL predictions until and including 
# the forecast horizon are less than 0. i.e. Predicting a negative GDP Growth Rate. 


# X variables is a vector; input would be ADL variables
poor_outlook <- function(Y_ts, ADL_var, start, end, f_horizon, dum){
  
  m <- length(ADL_var)
  
  forecast_output <- c() 
  empty_list <- list() 
  
  for (i in 1:m){
    X_temp <- get(ADL_var[i])
    forecast_h <- (ADL_predict_all(Y_ts, X_temp, start, end, f_horizon, dum))$predictions
    curr_indicator <- c()
    for (j in 1:f_horizon){
      forecast <- forecast_h[j]
      forecast_output <- append(forecast_output, forecast)
      curr_indicator <- append(curr_indicator, forecast)
    }
    empty_list[[i]] <- curr_indicator
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
  indicators_poor <- ADL_var[less_than_zero]
  
  if (is_empty(unique(indicators_poor))){
    output_message = "None of the ADL predictors forecast a negative GDP growth rate."
    indicators_output = NULL
  } else {
    output_message = "Some of the ADL predictors forecast a negative GDP growth rate."
    indicators_output = unique(indicators_poor)
  }
  
  return(list("message" = output_message, "indicators" = indicators_output,
              "ADL_predictions" = empty_list))
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


abnormal <- function(ADL_var){
  
  m <- length(ADL_var)
  high_dev_indicators <- c()
  medium_dev_indicators <- c()
  
  for (i in 1:m){
    X_temp <- get(ADL_var[i])
    name_indicator <- ADL_var[i]
    
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
aggregate_output <- function(Y_ts, X_comb_df, start, end, f_horizon, dum){
  
  # call poor outlook function 
  ADL_var <- as.vector(colnames(X_comb_df))
  
  poor_outlook <- poor_outlook(Y_ts, ADL_var, start, end,
                               f_horizon, covid_dummy)
  
  #########################################################
  # forecasting using GR result from another file 
  #########################################################
  
  ADL_forecasts <- poor_outlook$ADL_predictions 
  
  p_constant <- rep(gru2[1], f_horizon)
  #p_rev_AR
  p_baa <- ADL_forecasts[[1]]
  p_tsp <- ADL_forecasts[[2]]
  p_hstarts <- ADL_forecasts[[3]]
  p_consent <- ADL_forecasts[[4]]
  p_nasdaq <- ADL_forecasts[[5]]
  
  #currently missing revised AR predictor (it should be gru2[2])
  forecasts <- p_constant + (gru2[2]*p_baa) + (gru2[3]*p_tsp) + (gru2[4]*p_hstarts) + (gru2[5]*p_consent) + (gru2[6]*p_nasdaq) + (gru2[7]*p_comb)
  
  ####################
  # calculating rmsfe 
  ####################
  
  # append new prediction values to the existing dataframes 
  new_rev_AR <- append(advanced_AR_input, p_rev_AR)
  new_baa <- append(as.matrix(X_comb_df[,1]), p_baa)
  new_tsp <- append(as.matrix(X_comb_df[,2]), p_tsp)
  new_hstarts <- append(as.matrix(X_comb_df[,3]), p_hstarts)
  new_consent <- append(as.matrix(X_comb_df[,4]), p_consent)
  new_nasdaq <- append(as.matrix(X_comb_df[,5]), p_nasdaq)
  new_comb <- append(as.matrix(comb_original), p_comb)
  
  # combine them then find rmsfe 
  # rmsfe should divide by diff rows to get diff horizons 
  
  
  # call abnormalities function 
  abnormal <- abnormal(ADL_var)
  
  return(list("predictions" = forecasts, 
              "outlook_message" = poor_outlook$message,
              "outlook_indicators" = poor_outlook$indicators,
              "abnormal" = abnormal))
}

aggregate_output(GDPGrowth_ts, ADL_variables, example_startq, 
                 example_endq, 4, covid_dummy)
  