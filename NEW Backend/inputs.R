library(zoo)
library(lubridate)

<<<<<<< HEAD
=======

>>>>>>> 5fc77c0 (adding rolling window for adl)
example_startq = "1980 Q1"
example_endq = "2023 Q4"

example_startyq = as.yearqtr(example_startq)
example_endyq = as.yearqtr(example_endq)

window_start_str = "2000 Q1"
window_start = as.yearqtr(window_start_str)
window_length = (example_endyq - window_start) * 4 + 1

example_fhorizon = 4

start_y = as.numeric(year(example_startyq))
end_y = as.numeric(year(example_endyq))

start_q = as.numeric(quarter(example_startyq))
end_q = as.numeric(quarter(example_endyq))

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


covid_dummy_ts <- ts(covid_dummy,
                     start = c(start_y, start_q), 
                     end = c(end_y, end_q), 
                     frequency = 4)

