
basic_cleaning <- function(RGDP_Data){
  # extracting the most revised values/recent data (data as of 2024 Q1) 
  latest_data <- RGDP_Data[, ncol(RGDP_Data)]
  
  # creating a lag for all quarters
  lag(latest_data)
  
  # check is a dataset to first have a look at the data
  check <- data.frame(RGDP_Data$DATE, (latest_data), lag(latest_data))
  
  # renaming columns
  colnames(check) <- c("Date", "Raw Data", "First Lag")
  
  # calculating growth rate of GDP from one quarter to the next
  check <- check[-1,] %>% 
    mutate(growth_rate = (`Raw Data` - `First Lag`)/(`First Lag`) * 100)
  
  # formatting the data variable in terms of year and quarters
  Dates <- gsub(":", " ", check$Date) 
  check <- check %>% 
    mutate(Time = as.yearqtr(Dates)) %>% 
    select(c(Time, growth_rate)) %>% 
    mutate(growth_rate = as.numeric(growth_rate))
  
  # first output: check
  
  working_df = RGDP_Data[, -c(1)]
  
  working_df[] <- lapply(working_df, function(x) {
    if (is.character(x) || is.factor(x)) {
      x[x == "#N/A"] <- NA
    }
    x
  })
  
  working_df <- working_df %>%
    mutate_all(as.numeric)
  
  colnames(working_df) <- sapply(seq(as.Date(as.yearqtr("1965 Q4", format = "%Y Q%q")), by="quarter", length.out = ncol(working_df)), function(x) format(as.yearqtr(x), format = "%Y Q%q"))
  
  rownames(working_df) <- sapply(seq(as.Date(as.yearqtr("1947 Q1", format = "%Y Q%q")), by="quarter", length.out = nrow(working_df)), function(x) format(as.yearqtr(x), format = "%Y Q%q"))
  
  # Calculating growth rate of each quarter
  lagged_working_df = lag(working_df)
  perc_change_df = (100 * (working_df - lagged_working_df) / lagged_working_df)[-c(1),]
  
  # second output: perc_change_df
  
  return (list("check" = check, "perc_change_df" = perc_change_df))
  
}


#####################
#### ADVANCED AR
#### MODEL FUNC
#####################


prep_func = function(dataset, n){
  
  nrow_dataset = nrow(dataset)
  
  data_across_q <- dataset 
  
  data_across_q[] <- lapply(data_across_q, function(x) {
    if (is.character(x) || is.factor(x)) {
      x[x == "#N/A"] <- NA
    }
    x
  })
  
  data_across_q[, -1] <- lapply(data_across_q[, -1], as.numeric)
  
  data_across_q[, 1] <- NA
  
  colnames(data_across_q) <- NULL
  
  data_across_q <- as.matrix(data_across_q) 
  
  lagged_data_across_q <- lag(data_across_q)
  
  df <- 100 * (data_across_q - lagged_data_across_q)/(lagged_data_across_q)
  
  # Removing the first and last row since we had added those to compute the lag.
  df <- df[-c(1, nrow_dataset),]
  
  # Revised values is a data frame that shows the growth rate of each quarter as compared to the previous quarter, across revisions. For instance, revision0 is the initial growth rate for each quarter, and revision1 shows the growth rate after values have been revised. 
  
  revised_growth <- data.frame(matrix(NA, nrow = nrow_dataset - 2, ncol = 0))
  
  for (i in 0:n){
    na_vector = c(replicate(nrow_dataset - 2, NA)) 
    na_vector = as.numeric(na_vector)
    
    values <- diag(df)
    col_name = paste0("revision", i)
    na_vector[1:(nrow_dataset - 2 -i)] = values[1:(nrow_dataset - 2 -i)]
    revised_growth[[`col_name`]] = na_vector
    
    df <- df[,-c(1)]
  }
  
  revised_growth_df <- revised_growth
  
  # Now, we are looking into how the growth rates change due to revision
  lagged_growth <- revised_growth[, -c(ncol(revised_growth))]
  revised_growth <- revised_growth[,-c(1)]
  
  # We're taking the average of the change in growth per revision 
  change_in_growth <- (revised_growth - lagged_growth)
  
  #change_in_growth[sapply(change_in_growth, is.infinite)] <- NA
  
  change_in_growth <- change_in_growth %>% 
    apply(2, mean, na.rm = TRUE)
  
  
  return(list("df" = revised_growth_df, "delta" = change_in_growth))
}

#####################
#### DATA SPLICING
#### FUNC
#####################

data_splice = function(data, row_start, row_end, col_start, col_end, 
                       window_start, window_end, n, m){
  
  ncol_data = ncol(data)
  nrow_data = nrow(data)
  
  row_start = as.yearqtr(row_start)
  row_end = as.yearqtr(row_end)
  col_start = as.yearqtr(col_start)
  col_end = as.yearqtr(col_end)
  window_start = as.yearqtr(window_start)
  window_end = as.yearqtr(window_end)
  
  row_start_slice = (window_start - row_start)*4
  row_last_slice = nrow_data - (row_end - window_end)*4
  
  col_start_slice = (window_start - col_start)*4
  col_last_slice = ncol_data - (col_end - window_end)*4
  
  output <- data[(row_start_slice + m):row_last_slice, 
                 (col_start_slice+n):(col_last_slice + 1)]
  
  return (output)
}




#####################
#### REVISING 
#### VALUES FUNC
#####################

revise_values = function(data, delta, window_start, window_end){
  
  window_start_yq = as.yearqtr(window_start)
  window_end_yq = as.yearqtr(window_end)
  
  # Split dataframe into 10 years before target date and current value of growth for quarters more than 10 years ago
  ten_year_mark = (window_end_yq - 10 - window_start_yq)*4 + 1
  end_of_row_mark = (window_end_yq - window_start_yq)*4 + 1
  
  end_qtr = as.character(window_end_yq + 1/4)
  
  # Most recent values for start of time to 10 years before target date
  ancient_values <- data[1:ten_year_mark,][[`end_qtr`]]
  
  # All other values
  recent_values <- data[(ten_year_mark + 1):end_of_row_mark,][[`end_qtr`]]
  
  # Applying approximation of final growth numbers on recent values
  forecast_growth = recent_values
  for (i in 1:length(recent_values)){
    if (forecast_growth[i] < 0){
      for (j in 1:i){
        forecast_growth[i] = forecast_growth[i] + (1 / (1 + exp(-abs(forecast_growth[i]))) - 0.5) * delta[41 - j]
      }
    }
  }
  
  revised_data = c(ancient_values, forecast_growth)
  
  return (revised_data)
  
}


##############
# GDP prep
##############

GDP_prep <- function(RGDP_Data, example_startq, example_endq){
  
  example_startyq = as.yearqtr(example_startq)
  example_endyq = as.yearqtr(example_endq)
  
  start_y = as.numeric(year(example_startyq))
  end_y = as.numeric(year(example_endyq))
  
  start_q = as.numeric(quarter(example_startyq))
  end_q = as.numeric(quarter(example_endyq))
  
  spliced_GDP <- data_splice(RGDP_Data, "1947 Q1", "2023 Q4", "1965 Q4", 
                             "2024 Q1", example_startyq, example_endyq, 3, 0)
  
  
  # first output is spliced_GDP
  
  perc_change_df <- basic_cleaning(RGDP_Data)$perc_change_df
  
  # note that the last input should be in a string format
  sliced_perc_change <- data_splice(perc_change_df, "1947 Q2", "2023 Q4", 
                                    "1965 Q4", "2024 Q1", 
                                    example_startq, example_endq, 2, 1)
  
  # second output is sliced_perc_change
  
  all_GDP_data <- sliced_perc_change[, ncol(sliced_perc_change)]
  
  # third output is all_GDP_data
  
  GDPGrowth_ts <- ts(all_GDP_data, 
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)
  
  # fourth output is GDPGrowth_ts
  
  return (list("spliced_GDP" = spliced_GDP, "sliced_perc_change" = sliced_perc_change,
               "all_GDP_data" = all_GDP_data, "GDPGrowth_ts" = GDPGrowth_ts))
}
