
#################################
# Selecting Best Model - AIC 
#################################

AICselector <- function(Y_df, X_df, end_year, end_quarter, dum){

  # options is a vector that comprises all the lags of Y and X. 
  # These are the options for permutations and combinations
  options <- c()
  for (j in 1:4){
    Y_string = paste("L(", "Y_df", ",", j, ")", sep = "")
    X_string = paste("L(", "X_df", ",", j, ")", sep = "")
    options <- append(options, Y_string)
    options <- append(options, X_string)
  }
  
  dum_string = as.character(substitute(dum))
  # creates the "GDPGrowth_ts ~ " part
  #start_string = paste("Y_df", " ~ ", sep = "")
  start_string = "Y_df ~ dum + "
  
  # just a really large value
  min_AIC = Inf
  min_AIC_string = ""
  
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
                             end = c(end_year, end_quarter))

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
  final_string = gsub("dum", dum_string, min_AIC_string)
  # output is the string format of the optimal model formula
  return(final_string)
}




###############################################################
# Function for calculating next quarter prediction
###############################################################


ADL_predict_1 <- function(Y_dataframe, X_dataframe, Y_string, X_string,
                          selectors, coefficients){
  
  coef_df <- as.data.frame(coefficients)
  coef_row_name <- c(rownames(coef_df))
  
  input_string <- as.character(selectors)
  
  Y_lags <- str_count(input_string, Y_string) - 1
  X_lags <- str_count(input_string, X_string) 
  # error proof: number of lags is accurate 
  
  Y_lag_names <- c()
  for (x in (1:length(coef_row_name))){
    if (grepl(Y_string, coef_row_name[x], fixed = TRUE)){
      # string of lag 
      str <- coef_row_name[x]
      Y_lag_names <- append(Y_lag_names, str)
    }
  }
  
  X_lag_names <- c()
  for (y in (1:length(coef_row_name))){
    if (grepl(X_string, coef_row_name[y], fixed = TRUE)){
      # string of lag 
      str <- coef_row_name[y]
      X_lag_names <- append(X_lag_names, str)
    }
  }
  
  pred = coefficients[[1]]
  
  
  if (Y_lags == 0){
    if (X_lags == 0){
      # if Y_lags = 0 and X_lags = 0
      pred = pred
    } 
    else {
      # if Y_lags = 0 and X_lags != 0
      for (i in 1:X_lags){
        # create lag string 
        lag_string <- X_lag_names[i]
        coeff_value = coef_df[lag_string,]
        
        tail_num_i = as.numeric(str_sub(lag_string,-2,-2))
        
        temp = (tail(X_dataframe, n = tail_num_i)[1]) * coeff_value
        pred = pred + temp
      }
    }
  } 
  else if (X_lags == 0){
    # if Y_lags != 0 and X_lags = 0
    
    for (j in 1:Y_lags){
      # create lag string 
      lag_string <- Y_lag_names[j]
      coeff_value = coef_df[lag_string,]
      
      tail_num_j = as.numeric(str_sub(lag_string,-2,-2))
      
      temp = (tail(Y_dataframe, n = tail_num_j)[1]) * coeff_value
      pred = pred + temp
    }
  } 
  else {
    # if Y_lags != 0 and X_lags != 0 
    for (p in 1:Y_lags){
      # create lag string 
      lag_string <- Y_lag_names[p]
      coeff_value = coef_df[lag_string,]
      
      tail_num_p = as.numeric(str_sub(lag_string,-2,-2))
      
      temp = (tail(Y_dataframe, n = tail_num_p)[1]) * coeff_value
      
      pred = pred + temp
      
    }
    for (q in 1:X_lags){
      # create lag string 
      lag_string <- X_lag_names[q]
      coeff_value = coef_df[lag_string,]
      
      tail_num_q = as.numeric(str_sub(lag_string,-2,-2))
      
      temp = (tail(X_dataframe, n = tail_num_q)[1]) * coeff_value
      pred = pred + temp
    }
  }
  
  return(pred)
}




###############################################################
# Function for calculating predictions for all time horizons
###############################################################

# note that Y_dataframe and X_dataframe are in ts form
ADL_predict_all <- function(Y_dataframe, X_dataframe, f_horizon, covid_dummy = covid_dummy){
  selectors_AIC <- AICselector(Y_dataframe, X_dataframe, end_y, end_q, covid_dummy)
  
  Y_string <- as.character(substitute(Y_dataframe))
  X_string <- as.character(substitute(X_dataframe))
  
  assign(X_string, X_dataframe)
  
  selectors_AIC <- gsub("Y_df", Y_string, selectors_AIC) 
  selectors_AIC <- gsub("X_df", X_string, selectors_AIC)

  
  model_formula = as.formula(selectors_AIC)
  
  model_AIC <- dynlm(model_formula,
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q))
  
  output_model <- model_AIC$residuals
  
  
  pred <- ADL_predict_1(Y_dataframe, X_dataframe, Y_string, X_string,
                        selectors_AIC, model_AIC$coefficients)
  
  X_df <- as.matrix(X_dataframe)
  GDP_df <- as.matrix(Y_dataframe)
  
  # X pred 
  AR_output <- fitAR(X_df, 1, covid_dummy)
  X_pred <- AR_output$pred
  
  if (f_horizon == 1){
    pred = pred
    resid <- as.matrix(output_model)
    rmsfe <- sqrt(sum(resid^2))/nrow(resid)
  }
  
  else{
    for (i in 2:f_horizon){
      
      # updating end year & quarter 
      updated_end_yq <- example_endyq + (i - 1)/4
      upd_end_y <- as.numeric(year(updated_end_yq))
      upd_end_q <- as.numeric(quarter(updated_end_yq))
      
      # replace the NA in the last row with the prediction above
      GDP_df <- rbind(GDP_df, pred)
      
      gdp_ts <- ts(GDP_df, 
                   start = c(start_y, start_q), 
                   end = c(upd_end_y, upd_end_q), 
                   frequency = 4)
      
      # X pred
      X_df <- rbind(X_df, X_pred)
      
      X_ts <- ts(X_df, 
                 start = c(start_y, start_q), 
                 end = c(upd_end_y, upd_end_q), 
                 frequency = 4)
      
      # Updating COVID dummy
      upd_covid_dummy = c(covid_dummy, rep(0, i - 1))
      
      # Formula recommended by AIC selector
      selectors_AIC_local <- gsub(Y_string, "gdp_ts", selectors_AIC) 
      selectors_AIC_local <- gsub(X_string, "X_ts", selectors_AIC_local) 
      selectors_AIC_local <- gsub("covid_dummy", "upd_covid_dummy", selectors_AIC_local)
      
      
      
      # model based on AIC selection
      model_AIC_local <- dynlm(as.formula(selectors_AIC_local),
                               start = c(start_y, start_q), 
                               end = c(upd_end_y, upd_end_q))
      
      
      output_model <- model_AIC_local$residuals
      resid <- as.matrix(output_model)
      rmsfe <- sqrt(sum(resid^2))/nrow(resid)
      
      pred <- ADL_predict_1(gdp_ts, X_ts, "gdp_ts", "X_ts",
                            selectors_AIC_local, model_AIC_local$coefficients)
      
      AR_output_new <- fitAR(X_df, i, covid_dummy)
      X_pred <- AR_output_new$pred
      
      
    }
  }
  
  return (list("rmsfe" = rmsfe, "prediction" = pred))
}


