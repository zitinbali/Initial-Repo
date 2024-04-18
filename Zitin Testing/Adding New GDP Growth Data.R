source("inputs.R")
source("GDP Cleaning.R")
source("AR_Model_Functions.R")


# new_growth_inputs simulates the inputs from the website 
# if they don't enter a value, it must be read in as NULL or NA
new_growth_inputs <- c(1.0, 1.1, NA, NA)

# IF example_endq = "2023 Q4" IN THE INPUTS FILE, THEN 
# all_GDP_data is the GDP growth rates until 2023 Q4

original_endq = as.yearqtr("2023 Q4")
upd_covid_dummy <- covid_dummy

for (i in 1:4){
  curr = new_growth_inputs[i]
  if (is.na(curr) == FALSE){
    # adding new values to all_GDP_data
    all_GDP_data <- append(all_GDP_data, curr)
    
    # editing the end date  
    original_endq = original_endq + 1/4
    
    # adding to covid_dummy
    upd_covid_dummy <- append(upd_covid_dummy, 0)
  }
  new_endq = original_endq
}

upd_end_y = as.numeric(year(new_endq))
upd_end_q = as.numeric(quarter(new_endq))


# start_y and start_q remain the same, they are imported from inputs.R

upd_GDPGrowth_ts <- ts(all_GDP_data, 
                       start = c(start_y, start_q), 
                       end = c(upd_end_y, upd_end_q), 
                       frequency = 4)

# now use AR functions as normal

# note that the first and third inputs are different
AR_predict_all(as.matrix(upd_GDPGrowth_ts), example_fhorizon, upd_covid_dummy)

# revised AR model is pending 
