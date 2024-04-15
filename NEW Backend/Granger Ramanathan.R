source("inputs.R")
source("GDP Cleaning.R")
source("AR_Model_Functions.R")
source("ADL Data.R")
source("ADL Functions.R")
source("ADL_Rolling.R")
source("DM_test.R")
source("Combined ADL Functions.R")

# not updated 
# rw_revised_AR = rolling_window_adv(RGDP_Data, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_baa = rolling_window_adl(perc_change_df_spliced, baa_aaa_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_tsp = rolling_window_adl(perc_change_df_spliced, tspread_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_hstarts = rolling_window_adl(perc_change_df_spliced, hstarts_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_consent = rolling_window_adl(perc_change_df_spliced, consent_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_nasdaq = rolling_window_adl(perc_change_df_spliced, nasdaq_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_comb = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, window_start, covid_dummy, real_values, example_startyq, example_endyq)

# use this once updated:
# X=cbind(rw_revised_AR$pred, rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
#         rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)


X=cbind(rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
        rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)


# number of out-of-sample observations (test window)
noos = (example_endyq - window_start) * 4 + 1 
#true values for the validation set
oosy2 = as.matrix(tail(real_values, noos))

#GR weights, constant, all restrictions in place:
X2=cbind(rep(1,nrow(oosy2)),X)

temp=diag(7)
temp[1,1]=0

gru2=lsei(X2, oosy2, c=c(0,rep(1,6)), d=1, e=temp, f=rep(0,7))

# Predictions from the individual models 
advanced_AR_input = adv_ar_input(RGDP_Data, example_startq, example_endq)
p_rev_AR <- AR_predict_all(advanced_AR_input, example_fhorizon, covid_dummy)$predictions
comb_output <- ADL_comb_predict_all(GDPGrowth_ts, X_comb_df, ADL_variables, 
                               example_startq, example_endq, example_fhorizon, covid_dummy)
p_comb <- comb_output$predictions
comb_original <- comb_output$fitted.values

# currently missing revised AR predictor (it should be gru2[2])
# gru2[1] + (gru2[2]*p_baa) + (gru2[3]*p_tsp) + (gru[4]*p_hstarts) + (gru[5]*p_consent) +
#   (gru[6]*p_nasdaq) + (gru[7]*p_comb)
