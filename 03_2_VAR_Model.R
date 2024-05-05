#*******************************___03_2_VAR_Model__****************************#


# Assign stationary variables


#*******************************___Currency_rate__*****************************#


summary(adf_t_d1_log_Currency_rate)                                                 # The series is stationary and the trend is significant in 95%.
length(d1_log_Currency_rate)                                                        # -3.99 < 4.70 "adf_t_d1_Currency_rate" is significant in 99%.

v_CR <- d1_log_Currency_rate

#************************************___CPI__**********************************#



summary(adf_t_d1_log_CPI)                                                           # The series is stationary and the trend is significant in 95%.
length(d1_log_CPI)                                                                  # -3.99 < -5.94 "adf_t_d2_CPI" is significant in 99%.

v_CPI <- d1_log_CPI

#***********************************___sa_CPI__********************************#

summary(adf_t_d1_log_sa_CPI)                                                        # The series is stationary and the trend is significant in 95%.
length(d1_log_sa_CPI)                                                               # -3.99 < -5.80 "adf_t_d2_sa_CPI" is significant in 99%.

v_sa_CPI <- d1_log_sa_CPI

#*********************************___Core_CPI__********************************#



summary(adf_t_d2_log_Core_CPI)                                                      # The series is stationary and the trend is significant in 95%.
length(d2_log_Core_CPI)                                                             # -3.43 < -3.90 "adf_t_d2_Core_CPI" is significant in 95%.

v_Core_CPI <- d2_log_Core_CPI

#*********************************___sa_Core_CPI__********************************#



summary(adf_t_d1_log_sa_core_CPI)                                                   # The series is stationary and the trend is significant in 95%.
length(d1_log_sa_core_CPI)                                                          # -3.43 < -3.90 "adf_t_d2_Core_CPI" is significant in 95%.

v_sa_Core_CPI <- d1_log_sa_core_CPI


#************************************___IPI__**********************************#

summary(adf_c_d1_log_IPI)                                                           # The series is not stationary and the trend is insignificant.
length(d1_log_IPI)                                                                  # -2.57 < -2.59 "adf_t_d2_IPI" is significant in 90%.

v_IPI <- d1_log_IPI

#************************************___CCI__**********************************#


summary(adf_c_d1_log_CCI)                                                           # The series is stationary.
length(d1_log_CCI)                                                                  # -3.46 < -9.76 "adf_t_d1_CCI" is significant in 99%.

v_CCI <- d1_log_CCI

#*************************************___M3__**********************************#

summary(adf_c_d1_log_M3)                                                            # The series is stationary and the trend is insignificant im 95%.
length(d1_log_M3)                                                                   # -3.99 < -4.08 "adf_t_d2_M3" is significant in 90%.

v_M3 <- d1_log_M3

#********************___CBRT_Net_reserves_excluding_swaps__********************#

summary(adf_t_d1_log_CBRT_Net_reserves_excluding_swaps)                             # The series is stationary.
length(d1_log_CBRT_Net_reserves_excluding_swaps)                                    # -3.99 < -6.87 "adf_t_d1_CBRT_Net_reserves_excluding_swaps" is significant in 99%.

v_NR <- d1_log_CBRT_Net_reserves_excluding_swaps


#********************************___Policy_rate__******************************#


summary(adf_t_d1_log_policy_rate)                                                   # The series is stationary and the trend is insignificant.
length(d1_log_policy_rate)                                                          # -2.88 < -3.08 "adf_c_d1_policy_rate" is significant in 95%.

v_PR <- d1_log_policy_rate


#************************************___rir__**********************************#


summary(adf_c_d1_rir)                                                              # The series is stationary and the trend is insignificant.
length(d1_rir)                                                                     # -2.88 < -4.98 "adf_c_d1_rir" is significant in 99%.

v_rir <- d1_rir


#**********************************___core_rir__*******************************#

summary(adf_c_d1_core_rir)                                                         # The series is stationary and the trend is insignificant.
length(d1_core_rir)                                                                # -2.88 < -4.29 "adf_c_d1_core_rir" is significant in 99%.

v_core_rir <- d1_core_rir


#***********************************___sd_CR__*********************************#

summary(xts_log_list$sd_CR)                                                        # The series is stationary and the trend is insignificant.
length(xts_log_list$sd_CR)                                                         # -2.88 < -4.76 "adf_c_d1_sd_CR" is significant in 99%.

v_sd_CR <- xts_log_list$sd_CR


#******************************************************************************#


v_CR <- as.xts(ts(v_CR, start = c(2006,2), frequency = 12))
v_CPI <- as.xts(ts(v_CPI, start = c(2006,2), frequency = 12))
v_sa_CPI <- as.xts(ts(v_CPI, start = c(2006,2), frequency = 12))
v_core_CPI <- as.xts(ts(v_CPI, start = c(2006,2), frequency = 12))
v_sa_core_CPI <- as.xts(ts(v_CPI, start = c(2006,2), frequency = 12))
v_IPI <- as.xts(ts(v_IPI, start = c(2006,2), frequency = 12))
v_CCI <- as.xts(ts(v_CCI, start = c(2006,2), frequency = 12))
v_M3 <- as.xts(ts(v_M3, start = c(2006,2), frequency = 12))
v_NR <- as.xts(ts(v_NR, start = c(2006,2), frequency = 12))
v_PR <- as.xts(ts(v_PR, start = c(2006,2), frequency = 12))
v_rir <- as.xts(ts(v_rir, start = c(2006,2), frequency = 12))
v_core_rir <- as.xts(ts(v_core_rir, start = c(2006,2), frequency = 12))
v_sd_CR <- as.xts(ts(v_sd_CR, start = c(2006,1), frequency = 12))
v_sd_CR <- v_sd_CR[-1,]


#******************************************************************************





























