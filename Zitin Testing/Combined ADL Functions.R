
#source("inputs.R")
#source("GDP Cleaning.R")
#source("AR_Model_Functions.R")
#source("ADL Data.R")


#########################################
## Combined: Selecting Best Model - AIC 
#########################################


# start is the string of the start period e.g. "1980 Q1"
# end is the string of the end period e.g. "2000 Q1"

comb_AICselector <- function(Y_ts, X_combined_ts, ADL_var, start, end, curr_horizon, dum){
  
  startyq = as.yearqtr(start)
  endyq = as.yearqtr(end)
  
  start_y = as.numeric(year(startyq))
  end_y = as.numeric(year(endyq))
  
  start_q = as.numeric(quarter(startyq))
  end_q = as.numeric(quarter(endyq))
  
  # options is a vector that comprises all the lags of Y and X. 
  # These are the options for permutations and combinations
  options <- c()
  
  Y_name <- as.character(substitute(Y_ts))
  dum_name <- as.character(substitute(dum))
  
  num_X <- length(ADL_var)
  
  assign(Y_name, Y_ts)
  assign(dum_name, dum)
  
  # e.g. if your horizon is 2, the earliest lag you can take is the second lag
  start_lag = curr_horizon 
  end_lag = curr_horizon + 1
  
  # j refers to the number of lags of each variable that we will include.
  for (j in start_lag:end_lag){
    Y_string = paste("L(", Y_name, ",", j, ")", sep = "")
    options <- append(options, Y_string)
    
    for (i in 1:num_X){
      var_name = ADL_var[i]
      X_string = paste("L(", var_name, ",", j, ")", sep = "")
      options <- append(options, X_string)
    }
  }
  
  # creates the "GDPGrowth_ts ~ dum + " part
  start_string = paste(Y_name, " ~ ", sep = "")
  start_string = paste(start_string, dum_name, " + ", sep = "")
  
  # just a really large value
  min_AIC = Inf
  min_AIC_string = ""
  
  # creating the X dataframes 
  for (i in 1:num_X){
    name <- ADL_var[i]
    # assign name to the variable
    assign(name, X_combined_ts[,i])
  }
  
  for (i in 1:4){
    store = combn(options, i)
    length_store = ncol(store)
    
    for (m in 1:length_store){
      elements <- store[,m]
      body_string = ""
      
      for (n in 1:i){
        body_string = paste(body_string, elements[n], sep = " + ")
        # remove the first plus sign
        body_string_up <- substring(body_string, 4)
        
        model_string = paste(start_string, body_string_up, sep = "")
        model_formula = as.formula(model_string)
        
        # create dynlm model 
        model_local <- dynlm(model_formula,
                             start = c(start_y, start_q), 
                             end = c(end_y, end_q))
        
        # AIC of model 
        AIC_local <- AIC(model_local)
        
        # if AIC_local < min_AIC, replace the value of min_AIC. 
        # replace min_AIC_string too
        if(AIC_local < min_AIC){
          min_AIC = AIC_local
          min_AIC_string = model_string
        }
      }
    }
  }
  # output is the string format of the optimal model formula
  return(model_string)
}


##################################################################
# Function for calculating next quarter prediction (combined)
##################################################################


ADL_comb_predict <- function(Y_ts, X_combined_ts, ADL_var, curr_horizon,
                             dum, coefficients){
  
  Y_name <- as.character(substitute(Y_ts))
  dum_name <- as.character(substitute(dum))
  
  assign(Y_name, Y_ts)
  assign(dum_name, dum)
  
  num_X <- length(ADL_var)
  
  # creating the X dataframes 
  for (i in 1:num_X){
    name <- ADL_var[i]
    # assign name to the variable
    assign(name, X_combined_ts[,i])
  }
  
  coef_df <- as.data.frame(coefficients)
  coef_row_name <- c(rownames(coef_df))
  
  coef_df <- coef_df %>% 
    mutate(lag_name = gsub("^.*?\\(([^,]+).*", "\\1", coef_row_name), 
           lag_num = str_sub(coef_row_name,-2,-2))
  
  # intercept value 
  pred = coefficients[1]

  
  # if covid_dummy is one of the chosen variables
  # start looking through the table from row 3
  if (dum_name %in% coef_row_name){
    start_row = 3
  } else {
    start_row = 2 
  }
  
  num_coeff <- length(coefficients)
  
  for (i in start_row:num_coeff){
    
    # if it's GDPGrowth_ts, read from the Y_dataframe instead 
    if (coef_df[i, 2] == Y_name){
      df <- Y_ts
    } else {
      df <- get(coef_df[i, 2])
    }
    coef_value <- coef_df[i, 1]
    tail_num <- as.numeric(coef_df[i, 3])
    
    # tail_num is based on the first horizon perspective 
    # so tail_num should change with each horizon prediction
    # e.g. if tail_num = 4, this is only valid for the first horizon
    # if curr_horizon = 2, then the tail based on the same dataframe is now 3
    tail_num_upd <- tail_num - (curr_horizon - 1)
    
    temp = (tail(df, n = tail_num_upd)[1]) * coef_value
    pred = pred + temp
  }
  
  return(pred[[1]])
}

##################################################################
# Function for calculating predictions for all time horizons
##################################################################


ADL_comb_predict_all <- function(Y_ts, X_combined_ts, ADL_var, start, end, 
                                 f_horizon, dum){
  
  # formatting dates
  startyq = as.yearqtr(start)
  endyq = as.yearqtr(end)
  
  start_y = as.numeric(year(startyq))
  end_y = as.numeric(year(endyq))
  
  start_q = as.numeric(quarter(startyq))
  end_q = as.numeric(quarter(endyq))
  
  # vectors + list of values to return at the end 
  predictions <- c()
  rmsfe_values <- c()
  fitted_values <- list()
  
  covid_dummy_ts <- ts(dum,
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q), 
                       frequency = 4)
  
  num_X <- length(ADL_var)
  # creating the X dataframes 
  for (i in 1:num_X){
    name <- ADL_var[i]
    # assign name to the variable
    assign(name, X_combined_ts[,i])
  }
  
  for (j in 1:f_horizon){
    selectors_AIC <- comb_AICselector(Y_ts, X_combined_ts, ADL_var,
                                      start, end, j, dum)
    selectors_AIC <- gsub("dum", "covid_dummy_ts", selectors_AIC)
    
    print(selectors_AIC)
    
    model_formula = as.formula(selectors_AIC)
    
    model_AIC <- dynlm(model_formula,
                       start = c(start_y, start_q), 
                       end = c(end_y, end_q))
    
    output_model <- model_AIC$residuals
    
    pred <- ADL_comb_predict(Y_ts, X_combined_ts, ADL_var, j,
                             covid_dummy_ts, model_AIC$coefficients)
    resid <- as.matrix(output_model)
    rmsfe <- sqrt(sum(resid^2)/nrow(resid))
    curr_fitted_values <- model_AIC$fitted.values
    
    predictions <- append(predictions, pred)
    rmsfe_values <- append(rmsfe_values, rmsfe)
    fitted_values[[j]] <- curr_fitted_values
    
  }
  
  return (list("predictions" = predictions, "rmsfe" = rmsfe_values,
               "fitted_values" = fitted_values))
}
