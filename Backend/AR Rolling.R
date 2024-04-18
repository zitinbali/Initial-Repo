library(sandwich)
#source("GDP Cleaning.R")
#source("inputs.R")
#source("AR_Model_Functions.R")

###########################
#### ROLLING WINDOW BASIC
###########################

# E.g. test window from 2010 to 2020
# E.g. Y or train window from 2000 to 2010
# Trains on data from 2000 to 2010 to produce 2010 data, then 2001 to 2011 for 2011, etc.

rolling_window = function(df, window_start, dummy, real, start, end, h = 1){
  test_length = (end - window_start) * 4 + 1
  
  save.coef = matrix(NA,test_length,5)
  save.pred = matrix(NA, test_length, 1)
  temp = window_start

  for(i in test_length:1){
    window_start_str = format(window_start, "%Y Q%q")
    Y = df[[`window_start_str`]]
    
    Y.window = Y[(1+test_length-i):(length(Y)-i)] 
    Y.window = as.matrix(Y.window)
    
    dummy.window = dummy[(1+test_length-i):(length(dummy)-i)] 
    dummy.window = as.matrix(dummy.window)
    
    winfit = fitAR(Y.window,h,dummy.window)
    save.coef[(1+test_length-i),] = c(winfit$coef, rep(0 , 5 - length(winfit$coef))) 
    save.pred[(1+test_length-i),] = winfit$pred
    
    window_start = window_start + 1/4
  }
  
  real_ts = ts(real, start, end, freq = 4)
  plot.ts(real_ts, main = "Real values against predicted values", cex.axis = 1.8)
  lines(ts(save.pred, temp, end, freq = 4),col="red") 
  
  
  rmse = sqrt(mean((tail(real,test_length)-save.pred)^2)) 
  mae = mean(abs(tail(real,test_length)-save.pred))
  errors = c("rmse"=rmse,"mae"=mae) 
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}

#############################
#### ROLLING WINDOW ADVANCED
#############################

rolling_window_adv = function(df, perc_change_df, window_start, dummy, real, start, end, h = 1){
  test_length = (end - window_start) * 4 + 1
  
  save.coef = matrix(NA,test_length,5)
  save.pred = matrix(NA, test_length, 1)
  rmse = rep(NA, test_length)
  
  start_str = format(start, "%Y Q%q")
  temp = window_start
  
  for(i in test_length:1){
    window_start_str = format(window_start - 1/4, "%Y Q%q")
    
    Y = adv_ar_input(df, perc_change_df, start_str, window_start_str)
    
    Y.window = Y[(1+test_length-i):(length(Y))] 
    Y.window = as.matrix(Y.window)
    
    dummy.window = dummy[(1+test_length-i):(length(dummy)-i)] 
    dummy.window = as.matrix(dummy.window)
    
    winfit = fitAR(Y.window,h,dummy.window)
    save.coef[(1+test_length-i),] = c(winfit$coef, rep(0 , 5 - length(winfit$coef))) 
    save.pred[(1+test_length-i),] = winfit$pred
    rmse[1+test_length-i] = winfit$rmsfe
    
    window_start = window_start + 1/4
  }
  
  real_ts = ts(real, start, end, freq = 4)
  plot.ts(real_ts, main = "Real values against predicted values", cex.axis = 1.8)
  lines(ts(save.pred, temp, end, freq = 4),col="red") 
  
  mae = mean(abs(tail(real,test_length)-save.pred))
  
  return(list("pred"=save.pred,"coef"=save.coef,"rmse"=rmse, "mae" = mae))
}