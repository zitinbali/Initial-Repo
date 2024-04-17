#covid dummy fn


covid_dummy_fn <- function(example_startyq, example_endyq){
  
  covid = c("2020 Q2", "2020 Q3")
  covid_start = as.yearqtr(covid[1])
  covid_end = as.yearqtr(covid[2])
  covid_dummy = rep(0, (example_endyq - example_startyq) * 4 + 1)
  
  
  # Timeframe cannot start from during covid or after
  # Dummy if timeframe ends on 2020 Q2, start of covid
  if (example_startyq <= covid_start & example_endyq == covid_start){
    index = (covid_start - example_startyq) * 4 + 1
    covid_dummy[index] = -1
  }
  
  # Dummy if timeframe includes all of covid
  if (example_startyq <= covid_start & example_endyq >= covid_end){
    index = (covid_start - example_startyq) * 4 + 1
    covid_dummy[index] = -1
    covid_dummy[index + 1] = 1
  }
  return(covid_dummy)
}


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
  
  training_t <- bind_rows(training, joining_value) 
  
  original_test_values <- full_GDP_growth %>%
    filter(Time > example_endyq) %>% 
    select(Time, growth_rate) %>%
    head(h) %>%
    mutate(category = 1) 
  
  original_data <- bind_rows(training_t, original_test_values)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  training_p <- bind_rows(training, joining_value)
  
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
    filter(Time < window_start) %>% 
    tail(n = 9) %>%
    select(Time, growth_rate) %>%
    mutate(category = 1) 
  
  joining_value <- GDPGrowth_ts_df_sliced %>%
    filter(Time == example_endyq) %>% 
    select(Time, growth_rate) %>%
    mutate(category = 3) 
  
  training_t <- bind_rows(training, joining_value) 
  
  original_test_values <- full_GDP_growth %>%
    filter(Time >= window_start) %>% 
    select(Time, growth_rate) %>%
    head(window_length) %>%
    mutate(category = 1) 
  
  original_data <- bind_rows(training_t, original_test_values)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  training_p <- bind_rows(training, joining_value)
  
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
 
 #input_data_df = data.frame(Time = as.yearqtr(add_data_time), growth_rate = add_data_inputs) %>%
   #mutate(category = 2)

 #GDPGrowth_ts_df_sliced <- left_join(GDPGrowth_ts_df_sliced, input_data_df)
  
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
  
  training_t <- bind_rows(training, joining_value) 
  
  original_test_values <- full_GDP_growth %>%
    filter(Time > example_endyq) %>% 
    select(Time, growth_rate) %>%
    head(h) %>%
    mutate(category = 1) 
  
  #input_data_df = data.frame(Time = as.yearqtr(add_data_time), growth_rate = add_data_inputs) %>%
    #mutate(category = 2)
  
  original_data <- bind_rows(training_t, original_test_values)
  #original_data <- bind_rows(original_data, input_data_df)
  
  original_data <- original_data %>%
    filter(Time <= as.yearqtr("2023 Q4"))
  
  training_p <- bind_rows(training, joining_value)
  
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

fanplot_rmsfe_rolling <- function(rmsfe_df, joining_value, predictions, h) {
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
add_data <- function(data1, data2, data3, data4, h){
  vector = c()
  if (data1 != ""){
    vector = c(vector, data1)
  }
  else{
    return(vector)
  }
  
  if (data2 != ""){
    vector = c(vector, data2)
  }
  else{
    return(vector)
  }
  
  if (data3 != ""){
    vector = c(vector, data3)
  }
  else{
    return(vector)
  }
  
  if (data4 != ""){
    vector = c(vector, data4)
  }
  else{
    return(vector)
  }
  #vector = vector[1:h]
  length = length(vector)
  return(list("vector" = vector, "length" = length))
}





