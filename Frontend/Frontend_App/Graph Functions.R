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
actual_values_graph <- function(example_startyq, example_endyq, h){
  
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
  
  original_test_values <- all_GDP_ts_df %>%
    filter(Time > example_endyq) %>% 
    select(Time, growth_rate) %>%
    head(h) %>%
    mutate(category = 1) 
  
  original_data <- bind_rows(training_t, original_test_values)
  
  training_p <- bind_rows(training, joining_value)
  
  return(list("original_data" = original_data, "training_p" = training_p)) 
  #original data returns true value graph, training_p returns values of training data before prediction values
}





# fanplot rmsfe generating fn
# rmsfe_df from fn that generates rmsfe for h horizons

#rmsfe_df_test = c(0.2, 0.4)

fanplot_rmsfe <- function(rmsfe_df, predictions, h) {
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