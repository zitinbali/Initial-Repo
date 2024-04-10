library(sandwich)

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

rolling_window_adv = function(df, window_start, dummy, real, start, end, h = 1){
  test_length = (end - window_start) * 4 + 1
  
  save.coef = matrix(NA,test_length,5)
  save.pred = matrix(NA, test_length, 1)
  
  start_str = format(start, "%Y Q%q")
  temp = window_start
  
  for(i in test_length:1){
    window_start_str = format(window_start - 1/4, "%Y Q%q")
    
    Y = adv_ar_input(df, start_str, window_start_str)
    
    Y.window = Y[(1+test_length-i):(length(Y))] 
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
#####################
#### DIEBOLD-MARINO TEST
#####################

dm_test = function(real_values, pred1, pred2, start, end){
  loss1 = abs(real_values - pred1)
  loss2 = abs(real_values - pred2)
  
  loss_diff = loss1 - loss2
  
  if (as.yearqtr("2020 Q2") >= start){
    year_diff = (as.yearqtr("2020 Q2") - start) * 4 + 1
    if (as.yearqtr("2020 Q3") <= end){
      loss_diff[year_diff + 1] = 0
    } 
    if (as.yearqtr("2020 Q2") <= end){
      loss_diff[year_diff] = 0
    }
  }
  
  loss_diff_ts = ts(loss_diff, start, end, freq = 4)
  
  plot.ts(loss_diff_ts, main = "Loss differential", cex.axis = 1.8)
  
  dmreg = lm(loss_diff ~ 1)
  
  return(dmreg$coefficients / sqrt(NeweyWest(dmreg, lag = 4)))
}

start_rownum = which(grepl(example_startyq, check$Time))
end_rownum = which(grepl(example_endyq, check$Time))

row_start_slice = (example_startyq - as.yearqtr("1947 Q2"))*4 + 1
row_last_slice = nrow(check) - (as.yearqtr("2023 Q4") - example_endyq)*4

real_values = as.matrix(check[row_start_slice:row_last_slice, ncol(check)])
perc_change_df_spliced = perc_change_df[start_rownum:end_rownum,]

pred1 = rolling_window(perc_change_df_spliced, window_start, covid_dummy, real_values, example_startyq, example_endyq)$pred
