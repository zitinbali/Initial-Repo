library(sandwich)
source("GDP Cleaning.R")

# Rolling window prediction

fitAR=function(Y,p,h, dum){
  
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
  
  #save estimated AR regression, prediction, and estimated coefficients
  return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) 
}

#####################
#### ROLLING WINDOW
#####################

# E.g. test window from 2010 to 2020
# E.g. Y or train window from 2000 to 2010
# Trains on data from 2000 to 2010 to produce 2010 data, then 2001 to 2011 for 2011, etc.

rolling_window = function(Y, test_length, p = 1, h = 1){
  save.coef = matrix(NA,test_length,p + 1)
  save.pred = matrix(NA, test_length, 1) 
  for(i in test_length:1){
    
    Y.window = Y[(1+test_length-i):(nrow(Y)-i),] 
    Y.window = as.matrix(Y.window)
    print(Y.window)
    winfit = fitAR(Y.window,p,h, rep(0, length(Y.window)))
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
Y = as.matrix(check[,2])
test_length = 100

rolling_window(Y, test_length)

#####################
#### DIEBOLD-MARINO TEST
#####################





