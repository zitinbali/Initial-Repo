# Using the basic AR model from lecture 
# Inputs: Y - predicted variable, h - forecast horizon
# Iterates through p from 1 to 4 to find lowest rmsfe, uses that p to build model

fitAR=function(Y,h, dum){
  
  minimum = Inf
  
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
  return(list("model"=best_model,"pred"=best_pred,"coef"=best_coef, "rmsfe" = best_rmsfe, "aic"=minimum, "p" = best_p)) 
}

fitAR_preds <- function(Y, h, dum) {
  preds = numeric(h)
  for(i in 1:h){
    #test_AR <- as.matrix(check$growth_rate)
    preds[i] = fitAR(Y, i, dum)$pred  ##2 is placeholder for input$lags
  }
  return(preds)
}

adv_ar_input = function(RGDP_Data, example_startq, example_endq){
  
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