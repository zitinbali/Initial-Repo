source("inputs.R")
source("ADL Data.R")
source("ADL Functions.R")

#########################
# new indicator added
#########################

new_indicator <- read_excel("INSERT FILE NAME HERE", 
                            col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

new_indicator <- ADL_splice(new_indicator, example_startyq, example_endyq)

new_indicator_ts <- ts(new_indicator$Spread, 
               start = c(start_y, start_q), 
               end = c(end_y, end_q), 
               frequency = 4)

# after this, just call ADL_predict_all like you would for any other indicator

ADL_predict_all(GDPGrowth_ts, new_indicator_ts, example_startq,
                example_endq, example_fhorizon, covid_dummy)

