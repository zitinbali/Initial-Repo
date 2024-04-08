library(sandwich)

#####################
#### ROLLING WINDOW
#####################

# E.g. test window from 2010 to 2020
# E.g. Y or train window from 2000 to 2010
# Trains on data from 2000 to 2010 to produce 2010 data, then 2001 to 2011 for 2011, etc.

rolling_window = function(Y, test_length, dummy, p = 1, h = 1){
  save.coef = matrix(NA,test_length,p + 1)
  save.pred = matrix(NA, test_length, 1) 
  for(i in test_length:1){
    
    Y.window = Y[(1+test_length-i):(nrow(Y)-i),] 
    Y.window = as.matrix(Y.window)
    winfit = fitAR(Y.window,p,h,dummy)
    save.coef[(1+test_length-i),] = winfit$coef 
    save.pred[(1+test_length-i),] = winfit$pred 
  }
  
  #Some useful post-prediction misc stuff:
  real = Y #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-test_length),save.pred),col="red") 
  
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
  
  loss_diff_ts = ts(loss_diff, start, end, freq = 4)
  
  plot.ts(loss_diff_ts, main = "Loss differential", cex.axis = 1.8)
  
  dmreg = lm(loss_diff ~ 1)
  
  return(dmreg$coefficients / sqrt(NeweyWest(dmreg, lag = 4)))
}
