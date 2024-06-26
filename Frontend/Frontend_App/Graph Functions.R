
#Graph fn
actual_values_graph <- function(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, h){
   edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(NA,NA,NA,NA)) %>%
    mutate(Time = as.yearqtr(Time)) %>%
    mutate(growth_rate = as.numeric(growth_rate))
  
  
  all_GDP_ts <- ts(all_GDP_data, 
                   start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                   end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                   frequency = 4)
  
  all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
  
  GDPGrowth_ts_df_sliced <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
 
  GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
  
  training <- GDPGrowth_ts_df_sliced %>%
    filter(Time > example_startyq) %>%
    filter(Time < example_endyq) %>% 
    tail(n = 9) %>%
    select(Time, growth_rate) %>%
    mutate(category = 1) 
  
  joining_value <- GDPGrowth_ts_df_sliced %>%
    filter(Time == example_endyq) %>% 
    select(Time, growth_rate) %>%
    mutate(category = 3) 
  
  #training_t <- bind_rows(training, joining_value) 
  
  training_p <- bind_rows(training, joining_value)
  
  original_test_values <- full_GDP_growth %>%
    filter(Time > example_endyq) %>% 
    select(Time, growth_rate) %>%
    head(h) %>%
    mutate(category = 1) 
  
  original_data <- bind_rows(training_p, original_test_values)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  
  return(list("original_data" = original_data, "training_p" = training_p, "joining_value" = joining_value)) 
  #original data returns true value graph, training_p returns values of training data before prediction values
}


actual_values_graph_rolling <- function(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, window_start, window_length){
  edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"), growth_rate = c(NA,NA,NA,NA)) %>%
    mutate(Time = as.yearqtr(Time)) %>%
    mutate(growth_rate = as.numeric(growth_rate))
  
  
  all_GDP_ts <- ts(all_GDP_data, 
                   start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                   end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                   frequency = 4)
  
  all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
  
  GDPGrowth_ts_df_sliced <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  
  GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
  
  training <- GDPGrowth_ts_df_sliced %>%
    filter(Time > example_startyq) %>%
    filter(Time < example_endyq) %>% 
    tail(n = 9) %>%
    select(Time, growth_rate) %>%
    mutate(category = 1) 
  
  joining_value <- GDPGrowth_ts_df_sliced %>%
    filter(Time == example_endyq) %>% 
    select(Time, growth_rate) %>%
    mutate(category = 3) 

  training_p <- bind_rows(training, joining_value)
  
  original_test_values <- full_GDP_growth %>%
    filter(Time >= window_start) %>% 
    select(Time, growth_rate) %>%
    head(window_length) %>%
    mutate(category = 1) 
  
  original_data <- bind_rows(training_p, original_test_values)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  return(list("original_data" = original_data, "training_p" = training_p, "joining_value" = joining_value)) 
  #original data returns true value graph, training_p returns values of training data before prediction values
}



# graphing function for adding data
# has extra add_data input to add in input data to plot "original graph"
actual_values_graph_add <- function(all_GDP_data, GDPGrowth_ts, full_GDP_growth, example_startyq, example_endyq, add_data_time, add_data_inputs, h){
  edge <- data.frame(Time = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4"), 
                     growth_rate = c(NA,NA,NA,NA,NA,NA,NA,NA)) %>%
    mutate(Time = as.yearqtr(Time)) %>%
    mutate(growth_rate = as.numeric(growth_rate))
  
  input_data_df = data.frame(Time = as.yearqtr(add_data_time), growth_rate = add_data_inputs) #%>%
    #mutate(category = 2)
  
  edge = rbind(input_data_df, tail(edge, n=8-nrow(input_data_df)))
  
  all_GDP_ts <- ts(all_GDP_data, 
                   start = c(as.numeric(year(as.yearqtr("1976 Q1"))), as.numeric(quarter(as.yearqtr("1976 Q1")))),
                   end = c(as.numeric(year(as.yearqtr("2023 Q4"))), as.numeric(quarter(as.yearqtr("2023 Q4")))),
                   frequency = 4)
  
  all_GDP_ts_df <- data.frame(time = as.yearqtr(time(all_GDP_ts)), value = as.numeric(all_GDP_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  all_GDP_ts_df <- rbind(all_GDP_ts_df, edge)
  
  GDPGrowth_ts_df_sliced <- data.frame(time = as.yearqtr(time(GDPGrowth_ts)), value = as.numeric(GDPGrowth_ts)) %>% 
    rename("Time" = "time") %>%
    rename("growth_rate" = "value")
  
  
 GDPGrowth_ts_df_sliced <- rbind(GDPGrowth_ts_df_sliced, edge)
 
 #example_endyq = as.yearqtr(tail(add_data_time, n=1))
  
  training <- GDPGrowth_ts_df_sliced %>%
    filter(Time > example_startyq) %>%
    filter(Time < example_endyq) %>% 
    tail(n = 9) %>%
    select(Time, growth_rate) %>%
    mutate(category = 1) 
  
  joining_value <- GDPGrowth_ts_df_sliced %>%
    filter(Time == example_endyq) %>% 
    select(Time, growth_rate) %>%
    mutate(category = 3) 
  
  original_test_values <- full_GDP_growth %>%
    filter(Time > example_endyq) %>% 
    select(Time, growth_rate) %>%
    head(h) %>%
    mutate(category = 1) 

  training_p <- bind_rows(training, joining_value)
    
  original_data <- bind_rows(training_p, original_test_values)
  #original_data <- bind_rows(original_data, input_data_df)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  return(list("original_data" = original_data, "training_p" = training_p, "joining_value" = joining_value)) 
  #original data returns true value graph, training_p returns values of training data before prediction values
}

# fanplot rmsfe generating fn
# rmsfe_df from fn that generates rmsfe for h horizons

rmsfe_df_test = c(0.2, 0.4)

fanplot_rmsfe <- function(rmsfe_df, joining_value, predictions, h) {
  predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                  upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
  predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
  
  for(i in 2:(h+1)){
    rmsfe = rmsfe_df
    predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate[i-1] + 1.28*rmsfe[i-1]
    predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate[i-1] - 1.28*rmsfe[i-1]
    predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate[i-1] + 0.67*rmsfe[i-1]
    predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate[i-1] - 0.67*rmsfe[i-1]
  }
  return(predictions_rmsfe)
}

fanplot_rmsfe_rolling <- function(rmsfe_df, joining_value, predictions, window_length) {
  predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,window_length+1), lower_bound_80 = rep(0,window_length+1), 
                                  upper_bound_50 = rep(0,window_length+1), lower_bound_50 = rep(0,window_length+1))
  predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
  
  for(i in 2:(window_length+1)){
    rmsfe = rmsfe_df
    predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate[i-1] + 1.28*rmsfe[i-1]
    predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate[i-1] - 1.28*rmsfe[i-1]
    predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate[i-1] + 0.67*rmsfe[i-1]
    predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate[i-1] - 0.67*rmsfe[i-1]
  }
  return(predictions_rmsfe)
}



#rename variable for indiv ADL model
rename_variable <- function(input_string) { 
  var = NULL
  if (input_string == "BAA-AAA Spread"){
    var = "baa_aaa_ts"
  }
  if (input_string == "Treasury Spread"){
    var = "tspread_ts"
  }
  if (input_string == "Housing Starts"){
    var = "hstarts_ts"
  }
  if (input_string == "Consumer Sentiment"){
    var = "consent_ts"
  }
  if (input_string == "NASDAQ Composite Index"){
    var = "nasdaq_ts"
  }
  return(var)
}

#create vector for adding data
add_data <- function(data1, data2, data3, data4){
  vector = c()
  if (!is.na(data1)){
    vector = c(vector, as.numeric(data1))
  }
  else{
    return(na.omit(vector))
  }
  
  if (!is.na(data2)){
    vector = c(vector, as.numeric(data2))
  }
  else{
    return(na.omit(vector))
  }
  
  if (!is.na(data3)){
    vector = c(vector, as.numeric(data3))
  }
  else{
    return(na.omit(vector))
  }
  
  if (!is.na(data4)){
    vector = c(vector, as.numeric(data4))
  }
  else{
    return(na.omit(vector))
  }
  #vector = vector[1:h]
  length = length(vector)
  return(list("vector" = vector, "length" = length))
}





