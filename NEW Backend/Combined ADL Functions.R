
source("inputs.R")
source("GDP Cleaning.R")
source("AR_Model_Functions.R")
source("ADL Data.R")


#########################################
## Creating combined dataframe
#########################################

# NEED TO FORM THE COMBINED DATASET FIRST
X_comb_df <- ts.union(baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts) 
# set colnames 
colnames(X_comb_df) <- ADL_variables

#########################################
## Combined: Selecting Best Model - AIC 
#########################################


# start is the string of the start period e.g. "1980 Q1"
# end is the string of the end period e.g. "2000 Q1"

comb_AICselector <- function(Y_ts, X_combined_ts, ADL_var, start, end, dum){
  
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
  
  # j refers to the number of lags of each variable that we will include.
  for (j in 1:2){
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


ADL_comb_predict <- function(Y_ts, X_combined_ts, ADL_var, 
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
    
    temp = (tail(df, n = tail_num)[1]) * coef_value
    pred = pred + temp
  }
  
  return(pred)
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
  
  # vectors of values to return at the end 
  predictions <- c()
  rmsfe_values <- c()
  
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
  
  selectors_AIC <- comb_AICselector(Y_ts, X_combined_ts, ADL_var,
                                    start, end, dum)
  selectors_AIC <- gsub("dum", "covid_dummy_ts", selectors_AIC) 

  # because of high computation, use this model_formula for all predictions
  model_formula = as.formula(selectors_AIC)
  
  model_AIC <- dynlm(model_formula,
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q))
  
  output_model <- model_AIC$residuals
  
  pred <- ADL_comb_predict(Y_ts, X_combined_ts, ADL_var, 
                           covid_dummy_ts, model_AIC$coefficients)
  resid <- as.matrix(output_model)
  rmsfe <- sqrt(sum(resid^2)/nrow(as.matrix(Y_ts)))
  
  predictions <- append(predictions, pred[[1]])
  rmsfe_values <- append(rmsfe_values, rmsfe)
  
  GDP_df <- as.matrix(Y_ts)
  
  # predictions for all X variables
  for (i in 1:num_X){
    # e.g. name is baa_aaa
    name <- substr(ADL_var[i], 1, nchar(ADL_var[i]) - 3)
    # e.g. name_df is "baa_aaa_df"
    name_df <- paste(name, "_df", sep = "")
    X_ts <- X_combined_ts[,i]
    X_df <- as.matrix(X_ts)
    
    assign(name_df, X_df)
    
    AR_output <- fitAR(get(name_df), 1, dum)
    pred_name = paste("X", i,"_pred", sep = "")
    
    assign(pred_name, AR_output$pred)
    # to call the new X value, you have to call get(paste(X, i,"_pred", sep = ""))
  }
  
  if (f_horizon == 1){
    pred = pred
  }
  
  else{
    for (i in 2:f_horizon){
      # updating end year & quarter 
      updated_end_yq <- endyq + (i - 1)/4
      upd_end_y <- as.numeric(year(updated_end_yq))
      upd_end_q <- as.numeric(quarter(updated_end_yq))
      
      # replace the NA in the last row with the prediction above
      GDP_df <- rbind(GDP_df, pred)
      
      gdp_ts <- ts(GDP_df, 
                   start = c(start_y, start_q), 
                   end = c(upd_end_y, upd_end_q), 
                   frequency = 4)
      
      
      # X pred
      for (j in 1:num_X){
        name <- substr(ADL_var[j], 1, nchar(ADL_var[j]) - 3)
        name_df <- paste(name, "_df", sep = "")
        name_ts <- ADL_var[j]
        
        X_pred <- get(paste("X", j,"_pred", sep = ""))
        
        curr_df <- get(name_df)
        curr_df <- rbind(curr_df, X_pred)
        
        assign(name_df, curr_df)
        
        curr_ts <- ts(get(name_df),
                      start = c(start_y, start_q), 
                      end = c(upd_end_y, upd_end_q), 
                      frequency = 4)
        
        assign(name_ts, curr_ts)
        # get(name_ts) summons the time series
        
      }
      
      
      # create the combined dataframe with the updated X dataframes 
      
      # this makes X_comb_upd the first ts 
      name_ts_first <- ADL_var[1]
      X_comb_upd <- get(name_ts_first)
      
      # this loop then updates X_comb_upd
      for (m in 2:num_X){
        name_ts <- ADL_var[m]
        X_new <- get(name_ts)
        X_comb_upd <- ts.union(X_comb_upd, X_new)
      }
      
      # correct the col names of the combined dataframe 
      colnames(X_comb_upd) <- ADL_var
      
      # Updating COVID dummy
      upd_covid_dummy = c(dum, rep(0, i - 1))
      
      upd_covid_dummy_ts <- ts(upd_covid_dummy,
                               start = c(start_y, start_q), 
                               end = c(upd_end_y, upd_end_q), 
                               frequency = 4)
      
      selectors_AIC_local <- gsub("GDPGrowth_ts", "gdp_ts", selectors_AIC) 
      selectors_AIC_local <- gsub("covid_dummy_ts", "upd_covid_dummy_ts", selectors_AIC_local) 
      
      
      # model based on AIC selection
      model_AIC_local <- dynlm(as.formula(selectors_AIC_local),
                               start = c(start_y, start_q), 
                               end = c(upd_end_y, upd_end_q))
      
      output_model <- model_AIC_local$residuals
      resid <- as.matrix(output_model)
      rmsfe <- sqrt(sum(resid^2)/nrow(as.matrix(gdp_ts)))
      
      
      pred <- ADL_comb_predict(gdp_ts, X_comb_upd, ADL_var, 
                               upd_covid_dummy_ts, model_AIC_local$coefficients)
      
      
      predictions <- append(predictions, pred[[1]])
      rmsfe_values <- append(rmsfe_values, rmsfe)
      
      # predictions for all X variables
      for (n in 1:num_X){
        # e.g. name is baa_aaa
        name <- substr(ADL_var[n], 1, nchar(ADL_var[n]) - 3)
        # e.g. name_df is "baa_aaa_df"
        name_df <- paste(name, "_df", sep = "")
        X_ts <- X_comb_upd[,n]
        X_df <- as.matrix(X_ts)
        
        assign(name_df, X_df)
        
        AR_output <- fitAR(get(name_df), 1, upd_covid_dummy)
        pred_name = paste("X", n,"_pred", sep = "")
        
        assign(pred_name, AR_output$pred)
      }
    }
  }
  return (list("predictions" = predictions, "rmsfe" = rmsfe_values))
}

