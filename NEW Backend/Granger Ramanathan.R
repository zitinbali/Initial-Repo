source("inputs.R")
source("GDP Cleaning.R")
source("AR_Model_Functions.R")
source("ADL Data.R")
source("ADL Functions.R")
source("ADL_Rolling.R")
source("Aggregate Functions.R")
source("DM_test.R")
source("Combined ADL Functions.R")

# not updated 
# rw_revised_AR = rolling_window_adv(RGDP_Data, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_baa = rolling_window_adl(perc_change_df_spliced, baa_aaa_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_tsp = rolling_window_adl(perc_change_df_spliced, tspread_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_hstarts = rolling_window_adl(perc_change_df_spliced, hstarts_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_consent = rolling_window_adl(perc_change_df_spliced, consent_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_nasdaq = rolling_window_adl(perc_change_df_spliced, nasdaq_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)

# not updated 
#rw_comb = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, window_start, covid_dummy, real_values, example_startyq, example_endyq)

# use this once updated:
# X=cbind(rw_revised_AR$pred, rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
#         rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)

X=cbind(rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
        rw_consent$pred, rw_nasdaq$pred)

# number of out-of-sample observations (test window)
noos = (example_endyq - window_start) * 4 + 1 
#true values for the validation set
oosy2 = as.matrix(tail(real_values, noos))

# once updated, 5 should actually be 7  
gru=lsei(X, oosy2, c=rep(1,5), d=1, e=diag(5), f=rep(0,5))

######################################
###Form OOS forecasts on the test set
######################################

# Y is supposed to be the true values for the test set 
# does that mean Y is perc_change_df_spliced[[`example_endq`]]
# if so, why is there an NA at the end?
# but then how do I put Y in the function?
Y = perc_change_df_spliced[[`example_endq`]]

oosy=tail(Y,noos) 


# need to choose what kind of OOS 



