# Using the basic AR model from lecture 
# Inputs: Y - predicted variable, h - forecast horizon
# Iterates through p from 1 to 4 to find lowest rmsfe, uses that p to build model

fitAR=function(Y,h, dum){
  
  minimum = Inf
  fitted_values <- list()
  
  for (p in 1:4){
    # create p lags + forecast horizon shift (=h option)
    aux = embed(Y, p+h)
    
    #  Y variable aligned/adjusted for missing data due to lags
    y = aux[,1] 
    
    # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)
    X = as.matrix(aux[,-c(1:(ncol(Y)*h))])
    
    # retrieve last p observations
    X.out = tail(aux,1)[1:ncol(X)] 
    
    # cutting dummy to shape
    dum = tail(dum, length(y))
    
    # estimate direct h-step AR(p) by OLS 
    model = lm(y~X+dum) 
    
    # fitted_values 
    curr_fitted_values <- model$fitted.values
    fitted_values[[p]] <- curr_fitted_values
    
    # extract coefficients
    coef = coef(model)[1:(ncol(X)+1)]
    
    #make a forecast using the last few observations: a direct h-step forecast.
    pred = c(1,X.out)%*%coef 
    
    #note the addition of a constant to the test observation vector
    
    #get unadjusted rmsfe (ignoring estimation uncertainty)
    rmsfe = sqrt(sum(model$residuals^2)/nrow(X))
    aic = AIC(model)
    
    if(aic < minimum){
      minimum = aic
      best_rmsfe = rmsfe
      best_p = p
      best_model = model
      best_pred = pred
      best_coef = coef
    }
  }
  
  
  #save estimated AR regression, prediction, and estimated coefficients
  return(list("model"=best_model,"pred"=best_pred,"coef"=best_coef, "rmsfe" = best_rmsfe, "aic"=minimum, "p" = best_p,
              "fitted_values"=fitted_values)) 
}

##########################
# AR Predictions
##########################

# This function outputs a vector of all predictions and rmsfe values for the 
# forecast horizon. e.g. if f_horizon = 2, there will be 2 predictions and 2 rmsfe 
# values 

# Y_matrix would be as.matrix(GDPGrowth_ts)
# h refers to the forecast horizon, and dum is the covid_dummy 
AR_predict_all <- function(Y_matrix, h, dum){
  pred = c()
  rmsfe = c()
  h_1 = fitAR(Y_matrix, 1, dum) 
  fitted_values <- h_1$fitted_values
  
  for (i in 1:h){
    curr_output = fitAR(Y_matrix, i, dum) 
    curr_pred = curr_output$pred
    curr_rmsfe = curr_output$rmsfe
    pred <- append(pred, curr_pred)
    rmsfe <- append(rmsfe, curr_rmsfe)
  }
  return(list("predictions" = pred, "rmsfe" = rmsfe, "fitted_values"=fitted_values))
}




adv_ar_input = function(RGDP_Data, perc_change_df, example_startq, example_endq){
  
  example_startyq = as.yearqtr(example_startq)
  example_endyq = as.yearqtr(example_endq)
  
  spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                             "2024 Q1", example_startyq, example_endyq, 3, 0)
  
  post_prep_gdp <- prep_func(spliced_GDP, 40)
  post_prep_gdp_df <- post_prep_gdp$df
  post_prep_gdp_delta = post_prep_gdp$delta
  
  #####################
  ## revise GDP values
  #####################
  
  # note that the last input should be in a string format
  sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                    "1965 Q4", "2024 Q1", 
                                    example_startq, example_endq, 2, 1)
  
  all_GDP_data <- revise_values(sliced_perc_change, post_prep_gdp_delta, 
                                example_startq, example_endq)
  
  advanced_AR_input <- as.matrix(all_GDP_data)
  
  return(advanced_AR_input)
}