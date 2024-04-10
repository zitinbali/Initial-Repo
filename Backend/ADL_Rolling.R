
rolling_window_adl = function(Y_df, X_df, window_start, dummy, real, start, end, h = 1){

  test_length = (end - window_start) * 4 + 1

  save.pred = matrix(NA, test_length, 1)
  
  start_str = format(start, "%Y Q%q")
  temp = window_start
  

  for (i in test_length:1){
    window_start_str = format(window_start, "%Y Q%q")

    Y = adv_ar_input(Y_df, start_str, window_start_str)

    Y.window = Y[(1+test_length-i):(length(Y) - 1)] 

    #Y.window = as.matrix(Y.window)

    GDPGrowth_ts <- ts(Y.window, 
                       start = start, 
                       end = window_start, 
                       frequency = 4)

    dummy.window = dummy[(1+test_length-i):(length(dummy)-i)] 
    dummy.window = as.matrix(dummy.window)
    
    X.window = X_df[(1+test_length-i):(length(X_df) - i)]

    if (i == test_length){
      selection = AICselector(Y.window, X.window, year(window_start), quarter(window_start), dummy.window)
    }
    
    save.pred[(1+test_length-i),] = winfit$pred
    
    window_start = window_start + 1/4
  }

  real_ts = ts(real, start, end, freq = 4)
  plot.ts(real_ts, main = "Real values against predicted values", cex.axis = 1.8)
  lines(ts(save.pred, temp, end, freq = 4),col="red") 
  
  rmse = sqrt(mean((tail(real,test_length)-save.pred)^2)) 
  mae = mean(abs(tail(real,test_length)-save.pred))
  errors = c("rmse"=rmse,"mae"=mae) 
  
  return(list("pred"=save.pred,"errors"=errors))
  
}