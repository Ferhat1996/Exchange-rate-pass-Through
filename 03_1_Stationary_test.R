#****************************___03_1_Stationary_test__*************************#


# Check and find stationary of the variables with using parameters.


#*******************************___Currency_rate__*****************************#

# Currency rate
adf_t_log_Currency_rate <- ur.df(xts_log_list$log_Currency_rate, 
                             type = c("trend"), 
                             lags = 12,
                             selectlags = c("AIC")) 
summary(adf_t_log_Currency_rate)                                                # The series is not stationary and the trend is statistically insignificant.
                                                                                # -3.43 < -0.24 
adf_c_log_Currency_rate <- ur.df(xts_log_list$log_Currency_rate, 
                             type = c("drift"), 
                             lags = 12,
                             selectlags = c("AIC")) 
summary(adf_c_log_Currency_rate)                                                # The series is not stationary.
                                                                                # -2.88 < 3.12 


d1_log_Currency_rate <- diff(xts_log_list$log_Currency_rate, differences = 1)
d1_log_Currency_rate <- d1_log_Currency_rate[-1,]

adf_t_d1_log_Currency_rate <- ur.df(d1_log_Currency_rate, 
                                type = c("trend"), 
                                lags = 12,
                                selectlags = c("AIC"))

summary(adf_t_d1_log_Currency_rate)                                             # The series is stationary and the trend is significant in 99%.
                                                                                # -3.99 < -7.95 "adf_t_d1_log_Currency_rate" is significant in 99%.


mean_d1_log_Currency_rate <- matrix(mean(d1_log_Currency_rate), 
                                length(d1_log_Currency_rate), 
                                1)

mean_d1_log_Currency_rate <- as.xts(ts(mean_d1_log_Currency_rate, 
                                   start = c(2006,2), 
                                   frequency = 12))

plot(d1_log_Currency_rate)
lines(mean_d1_log_Currency_rate,
      col = "red",
      lty = 4,
      lwd = 2)


#************************************___CPI__**********************************#

adf_t_log_CPI <- ur.df(xts_log_list$log_CPI, 
                   type = c("trend"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_t_log_CPI)                                                          # The series is not stationary and the trend is significant in 90%.
                                                                                # Therefore we will use "trend"
                                                                                # -3.43 < 2.90


d1_log_CPI <- diff(xts_log_list$log_CPI, differences = 1)
d1_log_CPI <- d1_log_CPI[-1,]

adf_t_d1_log_CPI <- ur.df(d1_log_CPI, 
                      type = c("trend"), 
                      lags = 12,
                      selectlags = c("AIC"))
summary(adf_t_d1_log_CPI)                                                       # The series is stationary and the trend is significant in 99%.
                                                                                # -3.43 < -3.72 "adf_t_d1_log_CPI" is significant in 95%.
                                                                                


mean_d1_log_CPI <- matrix(mean(d1_log_CPI), 
                      length(d1_log_CPI), 
                      1)

mean_d1_log_CPI <- as.xts(ts(mean_d1_log_CPI, 
                         start = c(2006,2), 
                         frequency = 12))

plot(d1_log_CPI)
lines(mean_d1_log_CPI,
      col = "red",
      lty = 4,
      lwd = 2)

#***********************************___sa_CPI__********************************#

adf_t_log_sa_CPI <- ur.df(xts_log_list$log_sa_CPI, 
                         type = c("trend"), 
                         lags = 12,
                         selectlags = c("AIC")) 
summary(adf_t_log_sa_CPI)                                                       # The series is not stationary and the trend is insignificant.
                                                                                # -3.43 < 2.40

                                                                                

adf_c_log_sa_CPI <- ur.df(xts_log_list$log_sa_CPI, 
                      type = c("drift"), 
                      lags = 12,
                      selectlags = c("AIC")) 
summary(adf_c_log_sa_CPI)                                                       # The series is not stationary.
                                                                                # -3.43 < 3.37



d1_log_sa_CPI <- diff(xts_log_list$log_sa_CPI, differences = 1)
d1_log_sa_CPI <- d1_log_sa_CPI[-1,]

adf_t_d1_log_sa_CPI <- ur.df(d1_log_sa_CPI, 
                         type = c("trend"), 
                         lags = 12,
                         selectlags = c("AIC"))
summary(adf_t_d1_log_sa_CPI)                                                    # The series is stationary and the trend is significant in 95%.
                                                                                # -3.43 < -3.58 "adf_t_d1_log_sa_CPI" is significant in 95%.


mean_d1_log_sa_CPI <- matrix(mean(d1_log_sa_CPI), 
                         length(d1_log_sa_CPI), 
                         1)

mean_d1_log_sa_CPI <- as.xts(ts(mean_d1_log_sa_CPI, 
                            start = c(2006,2), 
                            frequency = 12))

plot(d1_log_sa_CPI)
lines(mean_d1_log_sa_CPI,
      col = "red",
      lty = 4,
      lwd = 2)

#*********************************___log_Core_CPI__********************************#

adf_t_log_Core_CPI <- ur.df(xts_log_list$log_Core_CPI, 
                             type = c("trend"), 
                             lags = 12,
                             selectlags = c("AIC")) 
summary(adf_t_log_Core_CPI)                                                     # The series is not stationary and the trend is significant in 99%.
                                                                                # Therefore we will use "trend"
                                                                                # -3.43 < 3.99


d1_log_Core_CPI <- diff(xts_log_list$log_Core_CPI, differences = 1)
d1_log_Core_CPI <- d1_log_Core_CPI[-1,]

adf_t_d1_log_Core_CPI <- ur.df(d1_log_Core_CPI, 
                           type = c("trend"), 
                           lags = 12,
                           selectlags = c("AIC"))
summary(adf_t_d1_log_Core_CPI)                                                  # The series is not stationary.
                                                                                # -3.43 < -1.11 


d2_log_Core_CPI <- diff(xts_log_list$log_Core_CPI, differences = 2)
d2_log_Core_CPI <- d2_log_Core_CPI[-c(1:2),]

adf_t_d2_log_Core_CPI <- ur.df(d2_log_Core_CPI, 
                           type = c("trend"), 
                           lags = 12,
                           selectlags = c("AIC"))

summary(adf_t_d2_log_Core_CPI)                                                  # The series is stationary and the trend is insignificant.
                                                                                # -3.43 < -5.45 "adf_t_d2_log_Core_CPI" is significant in 99%.



mean_d2_log_Core_CPI <- matrix(mean(d2_log_Core_CPI), 
                           length(d2_log_Core_CPI), 
                           1)

mean_d2_log_Core_CPI <- as.xts(ts(mean_d2_log_Core_CPI, 
                              start = c(2006,2), 
                              frequency = 12))

plot(d2_log_Core_CPI)
lines(mean_d2_log_Core_CPI,
      col = "red",
      lty = 4,
      lwd = 2)

#*********************************___log_sa_Core_CPI__********************************#

adf_t_log_sa_core_CPI <- ur.df(xts_log_list$log_sa_core_CPI, 
                        type = c("trend"), 
                        lags = 12,
                        selectlags = c("AIC")) 
summary(adf_t_log_sa_core_CPI)                                                  # The series is not stationary and the trend is significant in 99%.
# Therefore we will use "trend"
# -3.43 < 3.99


d1_log_sa_core_CPI <- diff(xts_log_list$log_sa_core_CPI, differences = 1)
d1_log_sa_core_CPI <- d1_log_sa_core_CPI[-1,]

adf_t_d1_log_sa_core_CPI <- ur.df(d1_log_sa_core_CPI, 
                           type = c("trend"), 
                           lags = 12,
                           selectlags = c("AIC"))
summary(adf_t_d1_log_sa_core_CPI)                                               # The series is stationary and the trend is insignificant.
                                                                                # -3.43 < -5.45 "adf_t_d2_log_sa_core_CPI" is significant in 99%.


mean_d1_log_sa_core_CPI <- matrix(mean(d1_log_sa_core_CPI), 
                           length(d1_log_sa_core_CPI), 
                           1)

mean_d1_log_sa_core_CPI <- as.xts(ts(mean_d1_log_sa_core_CPI, 
                              start = c(2006,2), 
                              frequency = 12))

plot(d1_log_sa_core_CPI)
lines(mean_d1_log_sa_core_CPI,
      col = "red",
      lty = 4,
      lwd = 2)

#************************************___IPI__**********************************#

adf_t_log_IPI <- ur.df(xts_log_list$log_IPI, 
                   type = c("trend"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_t_log_IPI)                                                          # The series is not stationary and the trend is insignificant.
                                                                                # -3.43 < -2.82

adf_c_log_IPI <- ur.df(xts_log_list$log_IPI, 
                   type = c("drift"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_c_log_IPI)                                                          # The series is not stationary.
                                                                                # -3.43 < -2.88 < -2.65


d1_log_IPI <- diff(xts_log_list$log_IPI, differences = 1)
d1_log_IPI <- d1_log_IPI[-1,]
 
adf_c_d1_log_IPI <- ur.df(d1_log_IPI, type = c("drift"), lags = 12,
                                 selectlags = c("AIC"))
 
summary(adf_c_d1_log_IPI)                                                       # The series is stationary.
                                                                                # -3.43 < -5.10 "adf_c_d1_log_IPI" is significant in 99%.
 
 
mean_d1_log_IPI <- matrix(mean(d1_log_IPI), 
                                length(d1_log_IPI), 
                                 1)
 
mean_d1_log_IPI <- as.xts(ts(mean_d1_log_IPI, 
                                    start = c(2006,2), 
                                    frequency = 12))
 
 plot(d1_log_IPI)
 lines(mean_d1_log_IPI,
       col = "red",
       lty = 4,
       lwd = 2)

#************************************___CCI__**********************************#

adf_t_log_CCI <- ur.df(xts_log_list$log_CCI, 
                   type = c("trend"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_t_log_CCI)                                                          # The series is not stationary in 95% but the trend is insignificant.
                                                                                # -3.99 < -3.43 < -3.22 

adf_c_log_CCI <- ur.df(xts_log_list$log_CCI, 
                   type = c("drift"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_c_log_CCI)                                                          # The series is not stationary.
                                                                                # -3.46 < -2.15 


d1_log_CCI <- diff(xts_log_list$log_CCI, differences = 1)
d1_log_CCI <- d1_log_CCI[-1,]

adf_c_d1_log_CCI <- ur.df(d1_log_CCI, 
                      type = c("drift"), 
                      lags = 12,
                      selectlags = c("AIC"))

summary(adf_c_d1_log_CCI)                                                       # The series is stationary.
                                                                                # -3.46 < -9.88 "adf_c_d1_log_CCI" is significant in 99%.



mean_d1_log_CCI <- matrix(mean(d1_log_CCI), 
                      length(d1_log_CCI), 
                      1)

mean_d1_log_CCI <- as.xts(ts(mean_d1_log_CCI, 
                         start = c(2006,2), 
                         frequency = 12))

plot(d1_log_CCI)
lines(mean_d1_log_CCI,
      col = "red",
      lty = 4,
      lwd = 2)

#*************************************___M3__**********************************#

adf_t_log_M3 <- ur.df(xts_log_list$log_M3, 
                  type = c("trend"), 
                  lags = 12,
                  selectlags = c("AIC")) 
summary(adf_t_log_M3)                                                           # The series is not stationary but the trend is insignificant.
                                                                                # Therefore we will use "trend"                                                                                
                                                                                # -3.43 < -3.39


d1_log_M3 <- diff(xts_log_list$log_M3, differences = 1)
d1_log_M3 <- d1_log_M3[-1,]

adf_c_d1_log_M3 <- ur.df(d1_log_M3, 
                     type = c("drift"), 
                     lags = 12,
                     selectlags = c("AIC"))

summary(adf_c_d1_log_M3)                                                        # The series is stationary.
                                                                                # -3.43 < -9.95 "adf_c_d1_log_M3" is significant in 99%.

mean_d1_log_M3 <- matrix(mean(d1_log_M3), 
                     length(d1_log_M3), 
                     1)

mean_d1_log_M3 <- as.xts(ts(mean_d1_log_M3, 
                        start = c(2006,2), 
                        frequency = 12))

plot(d1_log_M3)
lines(mean_d1_log_M3,
      col = "red",
      lty = 4,
      lwd = 2)

#********************___CBRT_Net_reserves_excluding_swaps__********************#
# log_CBRT_Net_reserves_excluding_swaps

adf_t_log_CBRT_Net_reserves_excluding_swaps <- ur.df(xts_log_list$log_CBRT_Net_reserves_excluding_swaps, 
                                                 type = c("trend"), 
                                                 lags = 12,
                                                 selectlags = c("AIC")) 
summary(adf_t_log_CBRT_Net_reserves_excluding_swaps)                            # The series is not stationary but the trend is significant in 95%.
                                                                                # Therefore we will use "trend"                                                                                
                                                                                # -3.43 < -0.41


d1_log_CBRT_Net_reserves_excluding_swaps <- diff(xts_log_list$log_CBRT_Net_reserves_excluding_swaps, differences = 1)
d1_log_CBRT_Net_reserves_excluding_swaps <- d1_log_CBRT_Net_reserves_excluding_swaps[-1,]

adf_t_d1_log_CBRT_Net_reserves_excluding_swaps <- ur.df(d1_log_CBRT_Net_reserves_excluding_swaps, 
                                                    type = c("trend"), 
                                                    lags = 12,
                                                    selectlags = c("AIC"))

summary(adf_t_d1_log_CBRT_Net_reserves_excluding_swaps)                         # The series is stationary and the trend is significant in 95%.
                                                                                # -3.99 < -6.34 "adf_t_d1_log_CBRT_Net_reserves_excluding_swaps" is significant in 99%.


mean_d1_log_CBRT_Net_reserves_excluding_swaps <- matrix(mean(d1_log_CBRT_Net_reserves_excluding_swaps), 
                                                    length(d1_log_CBRT_Net_reserves_excluding_swaps), 
                                                    1)

mean_d1_log_CBRT_Net_reserves_excluding_swaps <- as.xts(ts(mean_d1_log_CBRT_Net_reserves_excluding_swaps, 
                                                       start = c(2006,2), 
                                                       frequency = 12))

plot(d1_log_CBRT_Net_reserves_excluding_swaps)
lines(mean_d1_log_CBRT_Net_reserves_excluding_swaps,
      col = "red",
      lty = 4,
      lwd = 2)

#********************************___Policy_rate__******************************#

adf_t_log_policy_rate <- ur.df(xts_log_list$log_policy_rate, 
                           type = c("trend"), 
                           lags = 12,
                           selectlags = c("AIC")) 
summary(adf_t_log_policy_rate)                                                  # The series is not stationary but the trend is significant in 99%.
                                                                                # Therefore we will use "trend"                                                                                
                                                                                # -3.43 < -2.62

d1_log_policy_rate <- diff(xts_log_list$log_policy_rate, differences = 1)
d1_log_policy_rate <- d1_log_policy_rate[-1,]

adf_t_d1_log_policy_rate <- ur.df(d1_log_policy_rate, type = c("trend"), lags = 12,
                              selectlags = c("AIC"))

summary(adf_t_d1_log_policy_rate)                                               # The series is stationary but the trend is insignificant.
                                                                                # -3.13 < -5.64 "adf_t_d1_log_policy_rate" is significant in 99%.

mean_d1_log_policy_rate <- matrix(mean(d1_log_policy_rate), 
                              length(d1_log_policy_rate), 
                              1)

mean_d1_log_policy_rate <- as.xts(ts(mean_d1_log_policy_rate, 
                                 start = c(2006,2), 
                                 frequency = 12))

plot(d1_log_policy_rate)
lines(mean_d1_log_policy_rate,
      col = "red",
      lty = 4,
      lwd = 2)

#************************************___rir__**********************************#

adf_t_rir <- ur.df(xts_log_list$rir, 
                           type = c("trend"), 
                           lags = 12,
                           selectlags = c("AIC")) 
summary(adf_t_rir)                                                              # The series is not stationary but the trend is insignificant.
                                                                                # -3.43 < -0.70

adf_c_rir <- ur.df(xts_log_list$rir, 
                   type = c("drift"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_c_rir)                                                              # The series is not stationary.
                                                                                # -3.43 < -0.21

d1_rir <- diff(xts_log_list$rir, differences = 1)
d1_rir <- d1_rir[-1,]

adf_c_d1_rir <- ur.df(d1_rir, type = c("drift"), lags = 12,
                              selectlags = c("AIC"))

summary(adf_c_d1_rir)                                                           # The series is stationary.
                                                                                # -3.13 < -4.98 "adf_c_d1_rir" is significant in 99%.

mean_d1_rir <- matrix(mean(d1_rir), 
                              length(d1_rir), 
                              1)

mean_d1_rir <- as.xts(ts(mean_d1_rir, 
                                 start = c(2006,2), 
                                 frequency = 12))

plot(d1_rir)
lines(mean_d1_rir,
      col = "red",
      lty = 4,
      lwd = 2)


#**********************************___core_rir__*******************************#

adf_t_core_rir <- ur.df(xts_log_list$core_rir, 
                   type = c("trend"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_t_core_rir)                                                         # The series is not stationary and the trend is insignificant.
                                                                                # -3.43 < -1.12

adf_c_core_rir <- ur.df(xts_log_list$core_rir, 
                   type = c("drift"), 
                   lags = 12,
                   selectlags = c("AIC")) 
summary(adf_c_core_rir)                                                         # The series is not stationary.
                                                                                # -3.43 < -0.11

d1_core_rir <- diff(xts_log_list$core_rir, differences = 1)
d1_core_rir <- d1_core_rir[-1,]

adf_c_d1_core_rir <- ur.df(d1_core_rir, type = c("drift"), lags = 12,
                      selectlags = c("AIC"))

summary(adf_c_d1_core_rir)                                                      # The series is stationary.
                                                                                # -3.13 < -4.29 "adf_c_d1_core_rir" is significant in 99%.

mean_d1_core_rir <- matrix(mean(d1_core_rir), 
                      length(d1_core_rir), 
                      1)

mean_d1_core_rir <- as.xts(ts(mean_d1_core_rir, 
                         start = c(2006,2), 
                         frequency = 12))

plot(d1_core_rir)
lines(mean_d1_core_rir,
      col = "red",
      lty = 4,
      lwd = 2)


#***********************************___sd_CR__*********************************#

adf_t_sd_CR <- ur.df(xts_log_list$sd_CR, 
                        type = c("trend"), 
                        lags = 12,
                        selectlags = c("AIC")) 
summary(adf_t_sd_CR)                                                            # The series is stationary and the trend is significant.
                                                                                # -3.43 < 4.15

d1_sd_CR <- diff(xts_log_list$sd_CR, differences = 1)
d1_sd_CR <- d1_sd_CR[-1,]

adf_t_d1_sd_CR <- ur.df(d1_sd_CR, type = c("trend"), lags = 12,
                           selectlags = c("AIC"))

summary(adf_t_d1_sd_CR)                                                         # The series is stationary.
                                                                                # -3.13 < -6.98 "adf_c_d1_sd_CR" is significant in 99%.

mean_d1_sd_CR <- matrix(mean(d1_core_rir), 
                           length(d1_core_rir), 
                           1)

mean_d1_sd_CR <- as.xts(ts(mean_d1_sd_CR, 
                              start = c(2006,2), 
                              frequency = 12))

plot(d1_sd_CR)
lines(mean_d1_sd_CR,
      col = "red",
      lty = 4,
      lwd = 2)









