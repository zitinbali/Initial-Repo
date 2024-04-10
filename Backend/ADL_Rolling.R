
rolling_window_adl = function(Y_df, X_df, window_start, dummy, real, start, end, h = 1){

  test_length = (end - window_start) * 4 + 1

  save.pred = matrix(NA, test_length, 1)
  
  start_str = format(start, "%Y Q%q")
  temp = window_start
  
  #window_start = window_start - 1/4
  
  for (i in test_length:1){
    window_start_str = format(window_start, "%Y Q%q")

    #Y = adv_ar_input(Y_df, start_str, window_start_str)
    Y = Y_df[[`window_start_str`]]

    Y.window = Y[(1+test_length-i):(length(Y) - i)] 

    Y.window = as.matrix(Y.window)

    GDPGrowth_ts <- ts(Y.window, 
                       start = start + (test_length - i) * 1/4, 
                       end = window_start - 1/4, 
                       frequency = 4)

    dummy.window = dummy[(1+test_length-i):(length(dummy)-i)] 
    dummy.window = as.matrix(dummy.window)
    dummy.window <- ts(dummy.window, 
                       start = start + (test_length - i) * 1/4, 
                       end = window_start - 1/4, 
                       frequency = 4)
    
    X.window = X_df[(1+test_length-i):(length(X_df) - i)]
    X.window <- ts(X.window, 
                       start = start + (test_length - i) * 1/4, 
                       end = window_start - 1/4, 
                       frequency = 4)

    if (i == test_length){
      selection = AICselector(GDPGrowth_ts, X.window, year(window_start), quarter(window_start), dummy.window)
    }
    
    Y_string = as.character(substitute(GDPGrowth_ts))
    X_string = as.character(substitute(X.window))
    
    formula = gsub("Y_df", "GDPGrowth_ts", selection)
    
    model_temp = dynlm(as.formula(formula), start = start, end = window_start)

    winfit = ADL_predict_1(GDPGrowth_ts, X.window, "GDPGrowth_ts", "X.window", formula, model_temp$coefficients)
    
    save.pred[(1+test_length-i),] = winfit
    
    window_start = window_start + 1/4
  }

  real_ts = ts(real, start, end, freq = 4)
  plot.ts(real_ts, main = "Real values against predicted values", cex.axis = 1.8)
  lines(ts(save.pred, temp, end, freq = 4),col="red") 
  
  rmse = sqrt(mean((tail(real,test_length)-save.pred)^2)) 
  mae = mean(abs(tail(real,test_length)-save.pred))
  errors = c("rmse"=rmse,"mae"=mae) 
  
  return(list("pred"=save.pred,"errors"=errors, "formula" = formula))
  
}