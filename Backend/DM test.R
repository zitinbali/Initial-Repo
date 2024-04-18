library(sandwich)
#source("GDP Cleaning.R")
#source("inputs.R")
#source("AR_Model_Functions.R")

#####################
#### DIEBOLD-MARINO TEST
#####################

dm_test = function(real_values, pred1, pred2, start, end){
  loss1 = abs(real_values - pred1)
  loss2 = abs(real_values - pred2)
  
  loss_diff = loss1 - loss2
  
  if (as.yearqtr("2020 Q2") >= start){
    year_diff = (as.yearqtr("2020 Q2") - start) * 4 + 1
    if (as.yearqtr("2020 Q4") <= end){
      loss_diff[year_diff + 2] = 0
    } 
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