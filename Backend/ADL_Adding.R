source("ADL Data.R")
source("inputs.R")
source("ADL Functions.R")
source("ADL_Rolling.R")
source("DM_test.R")

new_data <- read_excel("../Data/USSTHPI.xls", 
                      col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

new_data <- ADL_splice(new_data, example_startyq, example_endyq)

new_data_ts <- ts(new_data$Spread, 
                 start = c(start_y, start_q), 
                 end = c(end_y, end_q), 
                 frequency = 4)

plot(merge(as.zoo(GDPGrowth_ts), as.zoo(new_data_ts)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "New Data & GDP Growth over Time")

legend("topright", 
       legend = c("GDP Growth", "New Data"),
       col = c("darkred", "steelblue"),
       lwd = c(1, 1),
       cex = 0.5)

rw_new = rolling_window_adl(perc_change_df_spliced, new_data_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)

pred_new = rw_new$pred

dm_test_result = dm_test(tail(real_values, window_length), pred1, pred_new, window_start, example_endyq)[1]
# Basic vs Advanced values, Plots loss of basic - loss of advanced

p_value = pt(dm_test_result, df = window_length - 1) * 100

#dm_test_result
print(paste0("There is a probability of ", round(p_value,3), "% that this model outperforms the standard AR(p) model."))