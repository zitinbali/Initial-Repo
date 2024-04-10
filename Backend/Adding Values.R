
source("GDP Cleaning.R")
source("inputs.R")

##############
# GDP prep
##############


# check is a dataset to validate whether the data is stationary 
check <- data.frame(RGDP_Data$DATE, (latest_data), lag(latest_data))

# renaming columns
colnames(check) <- c("Date", "Raw Data", "First Lag")

# calculating growth rate of GDP from one quarter to the next
check <- check[-1,] %>% 
  mutate(growth_rate = (`Raw Data` - `First Lag`)/(`First Lag`) * 100)

# formatting the data variable in terms of year and quarters
Dates <- gsub(":", " ", check$Date) 
check <- check %>% 
  mutate(Time = Dates) %>% 
  select(c(Time, growth_rate)) %>% 
  mutate(growth_rate = as.numeric(growth_rate))


inputs <- c(0.350, 0.400, 0.540, 0.880)

check <- rbind(check, list("2024 Q1", 0.350))
check <- rbind(check, list("2024 Q2", 0.400))
check <- rbind(check, list("2024 Q3", 0.540))
check <- rbind(check, list("2024 Q4", 0.880))
check <- rbind(check, list("2025 Q1", 0))
check <- rbind(check, list("2025 Q2", 0))
check <- rbind(check, list("2025 Q3", 0))
check <- rbind(check, list("2025 Q4", 0))

all_GDP_data <- as.matrix(check[213:nrow(check), 2])

upd_covid_dummy <- append(covid_dummy, c(0, 0, 0, 0))

new_AR_output <- fitAR(all_GDP_data, example_fhorizon, upd_covid_dummy)

upd_end_y <- 2024
upd_end_q <- 4

example_endq <- "2024 Q4"

start_rownum = which(grepl(as.yearqtr("2000 Q1"), check$Time))
end_rownum = which(grepl(as.yearqtr(example_endq), check$Time))

basic_AR_input <- check[start_rownum:end_rownum, ] %>% 
  select(growth_rate) %>% 
  as.matrix()

start_plot = check$Time[end_rownum - 10]

h = 4

training <- check %>%
  mutate(Time = as.yearqtr(Time)) %>%
  filter(Time > as.yearqtr("1999 Q4")) %>%
  filter(Time < as.yearqtr("2024 Q4")) %>% 
  tail(n = 9) %>%
  select(Time, growth_rate) %>%
  mutate(growth_rate = as.numeric(growth_rate)) %>%
  mutate(category = 1) 

joining_value <- check %>%
  mutate(Time = as.yearqtr(Time)) %>%
  filter(Time == as.yearqtr("2024 Q4")) %>% 
  select(Time, growth_rate) %>%
  mutate(growth_rate = as.numeric(growth_rate)) %>%
  mutate(category = 3) 

training_t <- bind_rows(training, joining_value) %>%
  mutate(category = 1) 

training_p <- bind_rows(training, joining_value)

fitAR_preds <- function(Y, h, dum){
  preds = numeric(h)
  for(i in 1:h){
    #test_AR <- as.matrix(check$growth_rate)
    preds[i] = fitAR(Y, i, dum)$pred  
  }
  return(preds)
}

pred_store <- c(fitAR_preds(basic_AR_input, h, upd_covid_dummy))

predictions <- check %>% 
  mutate(Time = as.yearqtr(Time)) %>%
  filter(Time > as.yearqtr("2024 Q4")) %>% 
  head(n = h) %>%
  mutate(new_growth_rate = as.vector(pred_store))

# Separate predictions into actual and predicted dataframes for plotting
actual_test_values <- predictions %>% 
  
  select(Time, growth_rate) %>%
  mutate(category = 2)

predicted_test_values <- predictions %>%
  select(Time, new_growth_rate) %>% 
  rename("growth_rate" = "new_growth_rate") %>% 
  #l_join(joining_value, by = "growth_rate") %>%
  mutate(category = 3) 


original_data <- rbind(training_t, actual_test_values)
predicted_data <- rbind(training_p, predicted_test_values)

# creating data for fanplot
predictions_actual_values_only <- predictions %>% select(Time, growth_rate)


fanplot_rmsfe <- function(full_df, input_df, predictions, h) {
  predictions_rmsfe <- data.frame(upper_bound_80 = rep(0,h+1), lower_bound_80 = rep(0,h+1), 
                                  upper_bound_50 = rep(0,h+1), lower_bound_50 = rep(0,h+1))
  predictions_rmsfe$upper_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_80[1] = joining_value$growth_rate
  predictions_rmsfe$upper_bound_50[1] = joining_value$growth_rate
  predictions_rmsfe$lower_bound_50[1] = joining_value$growth_rate
  
  for(i in 2:(h+1)){
    rmsfe = fitAR(input_df, i, covid_dummy_ts)$rmsfe 
    predictions_rmsfe$upper_bound_80[i] = predictions$new_growth_rate + 1.28*rmsfe
    predictions_rmsfe$lower_bound_80[i] = predictions$new_growth_rate - 1.28*rmsfe
    predictions_rmsfe$upper_bound_50[i] = predictions$new_growth_rate + 0.67*rmsfe
    predictions_rmsfe$lower_bound_50[i] = predictions$new_growth_rate - 0.67*rmsfe
  }
  return(predictions_rmsfe)
}

##create fanplot dataframe with time column
time_data <- check %>%
  mutate(Time = as.yearqtr(Time)) %>%
  filter(Time >= as.yearqtr("2024 Q4")) %>%
  select(Time) 

data <- check %>%
  mutate(Time = as.yearqtr(Time)) %>%
  filter(Time < as.yearqtr("2024 Q4")) %>%
  select(Time) %>% 
  mutate(upper_bound_80 = 0, lower_bound_80 = 0, upper_bound_50 = 0, lower_bound_50 = 0)

rmsfe_data <- cbind(time_data, fanplot_rmsfe(check, basic_AR_input, predictions, h)) 

fanplot_data <- rbind(data, rmsfe_data) %>%
  filter(Time >= as.yearqtr("2024 Q4")) %>%
  head(h+1)


# recession blocks
recessions <- c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991,
                2001, 2007:2008)

rectangles <- data.frame(
  xmin = as.yearqtr(c("1961 Q1", "1970 Q1", "1974 Q1", "1980 Q1", "1990 Q1", "2001 Q1", "2007 Q1")),
  xmax = as.yearqtr(c("1962 Q4", "1970 Q4", "1975 Q4", "1982 Q4", "1991 Q4", "2001 Q4", "2008 Q4")),
  ymin = -Inf,
  ymax = Inf
)

recession_block = rectangles %>%
  filter(xmin >= start_plot & xmax <= as.yearqtr("2024 Q4")) #replace w start and end of lineplot

model_1 <- ggplot() +
  geom_line(data = predicted_data, aes(x = Time, y = growth_rate, color = category)) +
  geom_line(data = original_data, aes(x = Time, y = growth_rate, color = category)) +
  scale_colour_gradientn(colours = c("#465B84", "#1C5079", "#FF0000"), 
                         limits = c(1, 3), guide = "none") +
  geom_rect(data = recession_block, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) + 
  geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_80, ymax = upper_bound_80), fill = "#C1F4F7", alpha = 0.3) +
  geom_ribbon(data = fanplot_data, aes(x = Time, ymin = lower_bound_50, ymax = upper_bound_50), fill = "#6DDDFF", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", lwd = 0.5) +
  #geom_vline(xintercept = 1970-1, linetype = "solid", color = "blue") + #change x to end of input time horizon
  labs(x = "Time", y = "Growth Rate", title = "Quarterly Growth Rate of GDP") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line(color = "black"),
        plot.margin = margin(20,20,20,20))
plot(model_1)
})
})

