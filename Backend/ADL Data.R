
##############
# BAA-AAA 
##############

baa_aaa <- read_excel("../Data/FRED BAA-AAA Data.xls", 
                      col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

baa_aaa <- ADL_splice(baa_aaa, example_startyq, example_endyq)

baa_aaa_ts <- ts(baa_aaa$Spread, 
                 start = c(start_y, start_q), 
                 end = c(end_y, end_q), 
                 frequency = 4)

plot(merge(as.zoo(GDPGrowth_ts), as.zoo(baa_aaa_ts)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "BAA-AAA Spread & GDP Growth over Time")

legend("topright", 
       legend = c("GDP Growth", "BAA-AAA Spread"),
       col = c("darkred", "steelblue"),
       lwd = c(1, 1),
       cex = 0.5)



##############
# tspread
##############



tspread <- read_excel("../Data/FRED Treasury Spread.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

# NOTE: tspread is only from 1976 Q4 onward, so we can't accept forecast horizons earlier, at least not for this ADL model

tspread <- ADL_splice(tspread, example_startyq, example_endyq)

tspread_ts <- ts(tspread$Spread, 
                 start = c(start_y, start_q), 
                 end = c(end_y, end_q), 
                 frequency = 4)

plot(merge(as.zoo(GDPGrowth_ts), as.zoo(tspread_ts)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Treasury Spread & GDP Growth over Time")
legend("topright", 
       legend = c("GDP Growth", "Treasury Spread"),
       col = c("darkred", "steelblue"),
       lwd = c(1, 1),
       cex = 0.5)


##############
# hstarts
##############


fred_hstarts <- read_excel("../Data/FRED Hstarts.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

fred_hstarts <- ADL_splice(fred_hstarts, example_startyq, example_endyq)

fred_hstarts_ts <- ts(fred_hstarts$Spread, 
                      start = c(start_y, start_q), 
                      end = c(end_y, end_q), 
                      frequency = 4)

plot(merge(as.zoo(GDPGrowth_ts), as.zoo(fred_hstarts_ts)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Housing Starts & GDP Growth over Time")
legend("topright", 
       legend = c("GDP Growth", "Housing Starts"),
       col = c("darkred", "steelblue"),
       lwd = c(1, 1),
       cex = 0.5)




##############
# consent
##############


consent <- read_excel("../Data/FRED Consumer Sentiment.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

consent <- ADL_splice(consent, example_startyq, example_endyq)

consent_ts <- ts(consent$Spread, 
                 start = c(start_y, start_q), 
                 end = c(end_y, end_q), 
                 frequency = 4)

plot(merge(as.zoo(GDPGrowth_ts), as.zoo(consent_ts)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Consumer Sentiment & GDP Growth over Time")
legend("topright", 
       legend = c("GDP Growth", "Consumer Sentiment"),
       col = c("darkred", "steelblue"),
       lwd = c(1, 1),
       cex = 0.5)



##############
# test
##############


consent2 <- read_excel("../Data/FRED Consumer Sentiment.xls", col_names = c("Date", "Spread")) %>% 
  mutate(Date = as.yearqtr(Date), 
         Spread = as.numeric(Spread))

consent2 <- ADL_splice(consent2, example_startyq, example_endyq)

consent2_ts <- ts(consent$Spread, 
                  start = c(start_y, start_q), 
                  end = c(end_y, end_q), 
                  frequency = 4)