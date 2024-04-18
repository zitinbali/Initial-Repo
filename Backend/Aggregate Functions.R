
##########################
# Poor Outlook Function
##########################

# This function analyses how many of our ADL predictions until and including 
# the forecast horizon are less than 0. i.e. Predicting a negative GDP Growth Rate. 


# X variables is a vector; input would be ADL variables
poor_outlook <- function(Y_ts, ADL_var, start, end, f_horizon, dum,
                         baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts){
  
  baa_aaa_ts = baa_aaa_ts
  tspread_ts = tspread_ts
  hstarts_ts = hstarts_ts
  consent_ts = consent_ts
  nasdaq_ts = nasdaq_ts
  
  m <- length(ADL_var)
  
  forecast_output <- c() 
  empty_list <- list() 
  fitted_values_list <- list() 
  
  for (i in 1:m){
    X_temp <- get(ADL_var[i])
    curr_model <- ADL_predict_all(Y_ts, X_temp, start, end, f_horizon, dum)
    forecast_h <- curr_model$predictions
    
    # curr_fitted_values is a list of dataframes
    curr_fitted_values <- curr_model$fitted_values
    
    curr_indicator <- c()
    for (j in 1:f_horizon){
      forecast <- forecast_h[j]
      forecast_output <- append(forecast_output, forecast)
      curr_indicator <- append(curr_indicator, forecast)
    }
    empty_list[[i]] <- curr_indicator
    fitted_values_list[[i]] <- curr_fitted_values
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
              "ADL_predictions" = empty_list, "fitted_values" = fitted_values_list))
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


abnormal <- function(ADL_var, baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts){
  
  baa_aaa_ts = baa_aaa_ts
  tspread_ts = tspread_ts
  hstarts_ts = hstarts_ts
  consent_ts = consent_ts
  nasdaq_ts = nasdaq_ts
  
  m <- length(ADL_var)
  high_dev_indicators <- c()
  medium_dev_indicators <- c()
  temp <- c() 
  
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
      temp <- append(temp, name_indicator)
      matches <- last_4_entries %in% outliers_medium
      # only count medium if at least 2 out of the last 4 entries have been above the threshold
      if (sum(matches) > 1){
        medium_dev_indicators <- append(medium_dev_indicators, name_indicator)
      }
    }
  }
  
  # additionally, count medium if >1 variables are crossing the medium threshold
  if (length(temp) > 1){
    for (i in (1:length(temp))){
      if (temp[i] %in% medium_dev_indicators){
        medium_dev_indicators = medium_dev_indicators
      } else{
        medium_dev_indicators <- append(medium_dev_indicators, temp[i])
      }
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


####################################
# Granger Ramanathan Test Function
####################################


GRtest <- function(RGDP_Data, perc_change_df, perc_change_df_spliced, start_yq, end_yq, 
                   real_values, dum,
                   baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts, X_comb_df){
  
  window_start_p = as.yearqtr(start_yq) + 15
  ADL_variables <- c("baa_aaa_ts", "tspread_ts", "hstarts_ts", "consent_ts", 
                     "nasdaq_ts")
  
  rw_revised_AR = rolling_window_adv(RGDP_Data, perc_change_df, 
                                     window_start_p, dum, real_values, start_yq, end_yq)
  rw_baa = rolling_window_adl(perc_change_df_spliced, baa_aaa_ts, window_start_p, dum, real_values, start_yq, end_yq, h = 1)
  rw_tsp = rolling_window_adl(perc_change_df_spliced, tspread_ts, window_start_p, dum, real_values, start_yq, end_yq)
  rw_hstarts = rolling_window_adl(perc_change_df_spliced, hstarts_ts, window_start_p, dum, real_values, start_yq, end_yq)
  rw_consent = rolling_window_adl(perc_change_df_spliced, consent_ts, window_start_p, dum, real_values, start_yq, end_yq)
  rw_nasdaq = rolling_window_adl(perc_change_df_spliced, nasdaq_ts, window_start_p, dum, real_values, start_yq, end_yq)
  rw_comb = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, ADL_variables, window_start_p, dum, real_values, start_yq, end_yq, h = 1, 
                                    baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
  
  X=cbind(rw_revised_AR$pred, rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
          rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)
  
  # number of out-of-sample observations (test window)
  noos = (end_yq - window_start_p) * 4 + 1 
  #true values for the validation set
  oosy2 = as.matrix(tail(real_values, noos))
  
  #GR weights, constant, all restrictions in place:
  X2=cbind(rep(1,nrow(oosy2)),X)
  
  temp=diag(8)
  temp[1,1]=0
  
  gru2=lsei(X2, oosy2, c=c(0,rep(1,7)), d=1, e=temp, f=rep(0,8))
  
  
  return(list("weights" = gru2))
}


###############################
# Aggregate Forecast Function
###############################

# Aggregates the forecasts

# X_variables refers to a vector comprising the string names of all X variables.
# The input for X_variable is ADL_variables
aggregate_output <- function(Y_ts, X_comb_df, RGDP_Data, 
                             perc_change_df, perc_change_df_spliced, real_values,
                             ADL_var, start, end, f_horizon, dum,
                             baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts){
  
  start_yq = as.yearqtr(start)
  end_yq = as.yearqtr(end)
  
  # call granger ramanathan test
  gru2 <- GRtest(RGDP_Data, perc_change_df, perc_change_df_spliced, start_yq, end_yq, 
                 real_values, dum,
                 baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts, X_comb_df)$weights
  
  # call poor outlook function 
  poor_outlook <- poor_outlook(Y_ts, ADL_var, start, end,
                               f_horizon, dum,
                               baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
  
  # call combined ADL function to get output 
  comb_output <- ADL_comb_predict_all(Y_ts, X_comb_df, ADL_var, 
                                      start, end, f_horizon, dum)
  
  # call revised AR function to get output 
  rev_AR_input = adv_ar_input(RGDP_Data, perc_change_df, start, end)
  rev_AR_output <- AR_predict_all(rev_AR_input, f_horizon, dum)
  
  #########################################################
  # forecasting using GR result from another file 
  #########################################################
  
  ADL_forecasts <- poor_outlook$ADL_predictions 
  
  p_constant <- rep(gru2[1], f_horizon)
  p_rev_AR <- rev_AR_output$predictions
  p_baa <- ADL_forecasts[[1]]
  p_tsp <- ADL_forecasts[[2]]
  p_hstarts <- ADL_forecasts[[3]]
  p_consent <- ADL_forecasts[[4]]
  p_nasdaq <- ADL_forecasts[[5]]
  p_comb <- comb_output$predictions
  
  # calculating forecasts based on the GR weights
  forecasts <- p_constant + (gru2[2]*p_rev_AR) + (gru2[3]*p_baa) + (gru2[4]*p_tsp) + (gru2[5]*p_hstarts) + (gru2[6]*p_consent) + (gru2[7]*p_nasdaq) + (gru2[8]*p_comb)
  
  ####################
  # calculating rmsfe 
  ####################
  
  # ADL_fitted_values is a list (of each variable) comprising lists 
  # (dataframes of fitted values) for each horizon)
  ADL_fitted_values <- poor_outlook$fitted_values
  
  # fitted values for each model 
  # each list comprises n dataframes of fitted values where n = f_horizon
  rev_AR <-rev_AR_output$fitted_values
  baa <- ADL_fitted_values[[1]]
  tsp <- ADL_fitted_values[[2]]
  hstarts <- ADL_fitted_values[[3]]
  consent <- ADL_fitted_values[[4]]
  nasdaq <- ADL_fitted_values[[5]]
  comb <- comb_output$fitted_values
  
  rmsfe <- c() 
  
  for (i in 1:f_horizon){
    
    # fitted values for the current horizon
    curr_rev_AR <- rev_AR[[i]]
    curr_baa <- baa[[i]]
    curr_tsp <- tsp[[i]]
    curr_hstarts <- hstarts[[i]]
    curr_consent <- consent[[i]]
    curr_nasdaq <- nasdaq[[i]]
    curr_comb <- comb[[i]]
    
    # since all of them use lags, they might not have the same number of rows
    # the objective here is to find the common starting point
    min_row <- min(length(curr_rev_AR), length(curr_baa), length(curr_tsp), 
                   length(curr_hstarts), length(curr_consent), length(curr_nasdaq),
                   length(curr_comb))
    
    # ensure that they are all the same size
    new_rev_AR <- as.matrix(tail(as.matrix(curr_rev_AR), min_row))
    new_baa <- as.matrix(tail(as.matrix(curr_baa), min_row))
    new_tsp <- as.matrix(tail(as.matrix(curr_tsp), min_row))
    new_hstarts <- as.matrix(tail(as.matrix(curr_hstarts), min_row))
    new_consent <- as.matrix(tail(as.matrix(curr_consent), min_row))
    new_nasdaq <- as.matrix(tail(as.matrix(curr_nasdaq), min_row))
    new_comb <- as.matrix(tail(as.matrix(curr_comb), min_row))
    
    true_values <- as.matrix(tail(real_values, min_row))
    
    # creating the constants and coefficients 
    
    all_constant <- as.matrix(rep(gru2[1], min_row))
    
    second_term <- as.matrix(rep(gru2[2], min_row))
    third_term <- as.matrix(rep(gru2[3], min_row))
    fourth_term <- as.matrix(rep(gru2[4], min_row))
    fifth_term <- as.matrix(rep(gru2[5], min_row))
    sixth_term <- as.matrix(rep(gru2[6], min_row))
    seventh_term <- as.matrix(rep(gru2[7], min_row))
    eighth_term <- as.matrix(rep(gru2[8], min_row))
    
    # calculate all predictions
    all_predictions <- as.vector(all_constant) + as.vector(second_term * new_rev_AR) + 
      as.vector(third_term * new_baa) + as.vector(fourth_term * new_tsp) + 
      as.vector(fifth_term * new_hstarts) + as.vector(sixth_term * new_consent) + 
      as.vector(seventh_term * new_nasdaq) + as.vector(eighth_term * new_comb)
    
    all_predictions <- as.matrix(all_predictions)
    
    resid <- all_predictions - true_values
    curr_rmsfe <- sqrt(sum(resid^2)/min_row)
    
    rmsfe <- append(rmsfe, curr_rmsfe)
  }
  
  # call abnormalities function 
  abnormal <- abnormal(ADL_var, baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
  
  return(list("predictions" = forecasts, 
              "rmsfe" = rmsfe,
              "outlook_message" = poor_outlook$message,
              "outlook_indicators" = poor_outlook$indicators,
              "abnormal" = abnormal))
}
  

# rev_AR_input = adv_ar_input(RGDP_Data, perc_change_df, example_startq, example_endq)
# rev_AR_output <- AR_predict_all(rev_AR_input, example_fhorizon, covid_dummy)
# rev_AR_fitted <-rev_AR_output$fitted_values
# 
# rev_AR_f1 <- rev_AR_fitted[[1]]
# 
# 
# new_rev_AR <- as.matrix(tail(as.matrix(rev_AR_f1), 75))
# 
# poor_outlook <- poor_outlook(GDPGrowth_ts, ADL_variables, example_startq, example_endq,
#                              example_fhorizon, covid_dummy,
#                              baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts)
# 
# ADL_fitted_values <- poor_outlook$fitted_values
# 
# ADL_baa_fit_1 <- ADL_fitted_values[[1]][[1]]
# 
# old_baa <- as.matrix(ADL_baa_fit_1)
# 
# new_baa <- as.matrix(tail(as.matrix(old_baa), 75))
# 
# start_yq = as.yearqtr(example_startq)
# end_yq = as.yearqtr(example_endq)
# 
# gru2 <- GRtest(RGDP_Data,perc_change_df, perc_change_df_spliced, start_yq, end_yq, real_values, covid_dummy,
#                baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts, X_comb_df)$weights
# 
# 
# all_constant <- as.matrix(rep(gru2[1], 75))
# 
# second_term <- as.matrix(rep(gru2[2], 75))
# third_term <- as.matrix(rep(gru2[3], 75))
# 
# 
# test <- (second_term[,1] * new_rev_AR[,1])
# test2 <- (third_term[,1] * new_baa[,1])
# 
# 
# as.vector(third_term * new_baa) + as.vector(second_term*new_rev_AR)
# 
# test + test2
