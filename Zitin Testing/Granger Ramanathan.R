source("inputs.R")
source("GDP Cleaning.R")
source("AR_Model_Functions.R")
source("ADL Data.R")
source("ADL Functions.R")
source("ADL_Rolling.R")
source("DM_test.R")
source("Combined ADL Functions.R")

start = example_startyq
end = example_endyq

GRtest <- function(RGDP_Data, perc_change_df_spliced, start_yq, end_yq, real_values, dum,
                   baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts, X_comb_df){
  
  window_start = as.yearqtr(start_yq) + 15
  
  rw_revised_AR = rolling_window_adv(RGDP_Data, window_start, dum, real_values, start_yq, end_yq)
  rw_baa = rolling_window_adl(perc_change_df_spliced, baa_aaa_ts, window_start, dum, real_values, start_yq, end_yq)
  rw_tsp = rolling_window_adl(perc_change_df_spliced, tspread_ts, window_start, dum, real_values, start_yq, end_yq)
  rw_hstarts = rolling_window_adl(perc_change_df_spliced, hstarts_ts, window_start, dum, real_values, start_yq, end_yq)
  rw_consent = rolling_window_adl(perc_change_df_spliced, consent_ts, window_start, dum, real_values, start_yq, end_yq)
  rw_nasdaq = rolling_window_adl(perc_change_df_spliced, nasdaq_ts, window_start, dum, real_values, start_yq, end_yq)
  rw_comb = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, window_start, dum, real_values, start_yq, end_yq)
  
  X=cbind(rw_revised_AR$pred, rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
          rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)
  
  # number of out-of-sample observations (test window)
  noos = (end_yq - window_start) * 4 + 1 
  #true values for the validation set
  oosy2 = as.matrix(tail(real_values, noos))
  
  #GR weights, constant, all restrictions in place:
  X2=cbind(rep(1,nrow(oosy2)),X)
  
  temp=diag(8)
  temp[1,1]=0
  
  gru2=lsei(X2, oosy2, c=c(0,rep(1,7)), d=1, e=temp, f=rep(0,8))
  
  
  return(list("weights" = gru2))
}


GRtest(RGDP_Data, perc_change_df_spliced, example_startyq, example_endyq, real_values, covid_dummy,
       baa_aaa_ts, tspread_ts, hstarts_ts, consent_ts, nasdaq_ts, X_comb_df)

rw_revised_AR = rolling_window_adv(RGDP_Data, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_baa = rolling_window_adl(perc_change_df_spliced, baa_aaa_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_tsp = rolling_window_adl(perc_change_df_spliced, tspread_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_hstarts = rolling_window_adl(perc_change_df_spliced, hstarts_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_consent = rolling_window_adl(perc_change_df_spliced, consent_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_nasdaq = rolling_window_adl(perc_change_df_spliced, nasdaq_ts, window_start, covid_dummy, real_values, example_startyq, example_endyq)
rw_comb = rolling_window_comb_adl(perc_change_df_spliced, X_comb_df, window_start, covid_dummy, real_values, example_startyq, example_endyq)

X=cbind(rw_revised_AR$pred, rw_baa$pred, rw_tsp$pred, rw_hstarts$pred,
        rw_consent$pred, rw_nasdaq$pred, rw_comb$pred)


# number of out-of-sample observations (test window)
noos = (example_endyq - window_start) * 4 + 1 
#true values for the validation set
oosy2 = as.matrix(tail(real_values, noos))

#GR weights, constant, all restrictions in place:
X2=cbind(rep(1,nrow(oosy2)),X)

temp=diag(8)
temp[1,1]=0

gru2=lsei(X2, oosy2, c=c(0,rep(1,7)), d=1, e=temp, f=rep(0,8))

# Predictions from the individual models 
rev_AR_input = adv_ar_input(RGDP_Data, example_startq, example_endq)
rev_AR_output <- AR_predict_all(rev_AR_input, example_fhorizon, covid_dummy)
p_rev_AR <- rev_AR_output$predictions
old_rev_AR <-rev_AR_output$fitted_values


comb_output <- ADL_comb_predict_all(GDPGrowth_ts, X_comb_df, ADL_variables, 
                               example_startq, example_endq, example_fhorizon, covid_dummy)
p_comb <- comb_output$predictions
old_comb <- comb_output$fitted_values

