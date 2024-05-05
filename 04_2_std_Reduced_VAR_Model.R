#**************************___04_2_std_Reduced_VAR_Model__*********************#

#****************************__Create_std_VAR_Model__**************************#

# Create VAR model matrix with adding v_sd_CR
var_model_s  <- cbind(v_sd_CR, v_NR, v_CCI,  v_IPI, v_CR, v_sa_core_CPI)
colnames(var_model_s ) <- c("v_sd_CR", "v_NR",  "v_CCI", "v_IPI", "v_CR", "v_sa_core_CPI")


#******************************************************************************#


# Select lag order for VAR model using data until 2019-12-31
delay_2019 <- VARselect(var_model_s["::2019-12-31"],
                        type ="cons",  # Constant term included
                        lag.max = 10,
                        exogen = NULL,
                        season = NULL)
summary(delay_2019)
delay_2019$selection
delay_2019$criteria

# Select lag order for VAR model using all available data
delay_2023 <- VARselect(var_model_s,
                        type ="cons",  # Constant term included
                        lag.max = 10,
                        exogen = NULL,
                        season = NULL)
summary(delay_2023)
delay_2023$selection
delay_2023$criteria


# Estimate VAR model using data until 2019-12-31
var_model_result_2019 <- VAR(var_model_s["::2019-12-31"], p = 3, type = "const")
# Estimate VAR model using all available data
var_model_result_2023 <- VAR(var_model_s, p = 3, type = "const")


# Compute Impulse Response Function for the period 2006-2019
IR <- irf(var_model_result_2019,
          impulse = "v_sd_CR",
          response = "v_sa_core_CPI",
          n.ahead = 36,
          boot = TRUE,
          ortho = FALSE,
          runs = 100)
plot(IR)
title(main = "2006-2019 Impulse Response", cex.main = 1.5, font.main = 2)

# Compute Impulse Response Function for the period 2006-2023
IR <- irf(var_model_result_2023,
          impulse = "v_sd_CR",
          response = "v_sa_core_CPI",
          n.ahead = 36,
          boot = TRUE,
          ortho = FALSE,
          runs = 100)
plot(IR)
title(main = "2006-2023 Impulse Response", cex.main = 1.5, font.main = 2)



#*******************************************************************************

# Create a list of VAR models for 2019 and 2023
filtered_var_model <- list(
  "2019-12-31" = var_model_s["::2019-12-31"],
  "2023-12-31" = var_model_s["::2023-12-31"]
)

#*******************************************************************************
#**********__CIR of 3 months rolling std of CR to sa_core_CPI_lag_3_***********#
#*******************************************************************************

# Loop over each VAR model in the list
for (date_filter in names(filtered_var_model)) {
  # Get the filtered VAR model based on the selected date
  var_model_results <- VAR(filtered_var_model[[date_filter]], p=3, type = "const")
  
  # Compute Impulse Response Function
  IR <- irf(var_model_results,
            impulse = "v_sd_CR",
            response = "v_sa_core_CPI",
            n.ahead = 36,
            boot = TRUE,
            ortho = FALSE,
            runs = 100)
  
  # Compute Cumulative Effects
  cumulative_irf <- cumsum(IR$irf[[1]])
  
  # Compute Standard Deviations
  std_dev <- sqrt(apply(IR$irf[[1]], 2, var))
  
  # Compute Upper and Lower Bands
  cumulative_lower_band <- cumulative_irf - 2 * std_dev
  cumulative_upper_band <- cumulative_irf + 2 * std_dev
  
  # Define time range (for illustration purpose)
  time_range <- 0:(length(cumulative_irf)-1)
  
  # Plot without default x-axis
  plot(time_range, cumulative_irf, type = "l", lwd = 2,
       ylim = range(c(cumulative_lower_band, cumulative_upper_band)),
       xlab = "Time", ylab = "Cumulative Effect", 
       main = paste("CIR of 3 months rolling std of CR to sa_core_CPI - ", date_filter, ", ", "lag = ", var_model_results$p),
       xaxt = "n",
       cex.main = 2) # Suppress default x-axis
  
  # Add Upper and Lower Bands
  lines(time_range, cumulative_lower_band, col = "red", lty = 2)
  lines(time_range, cumulative_upper_band, col = "blue", lty = 2)
  
  # Add shaded area
  polygon(c(time_range, rev(time_range)),
          c(cumulative_lower_band, rev(cumulative_upper_band)), 
          col = rgb(0.8,0.8,0.8,0.5), border = NA)
  
  # Add custom x-axis
  axis(1, at = seq(1, max(time_range), by = 2), labels = seq(1, max(time_range), by = 2))
  abline(h = seq(0, 1, by = 0.05), col = "lightgray", lty = "dotted")
}

#*******************************************************************************
#**********__CIR of 3 months rolling std of CR to sa_core_CPI_lag_5_***********#
#*******************************************************************************

# Loop over each VAR model in the list
for (date_filter in names(filtered_var_model)) {
  # Get the filtered VAR model based on the selected date
  var_model_results <- VAR(filtered_var_model[[date_filter]], p=5, type = "const")
  
  # Compute Impulse Response Function
  IR <- irf(var_model_results,
            impulse = "v_sd_CR",
            response = "v_sa_core_CPI",
            n.ahead = 36,
            boot = TRUE,
            ortho = FALSE,
            runs = 100)
  
  # Compute Cumulative Effects
  cumulative_irf <- cumsum(IR$irf[[1]])
  
  # Compute Standard Deviations
  std_dev <- sqrt(apply(IR$irf[[1]], 2, var))
  
  # Compute Upper and Lower Bands
  cumulative_lower_band <- cumulative_irf - 2 * std_dev
  cumulative_upper_band <- cumulative_irf + 2 * std_dev
  
  # Define time range (for illustration purpose)
  time_range <- 0:(length(cumulative_irf)-1)
  
  # Plot without default x-axis
  plot(time_range, cumulative_irf, type = "l", lwd = 2,
       ylim = range(c(cumulative_lower_band, cumulative_upper_band)),
       xlab = "Time", ylab = "Cumulative Effect", 
       main = paste("CIR of 3 months rolling std of CR to sa_core_CPI - ", date_filter, ", ", "lag = ", var_model_results$p),
       xaxt = "n",
       cex.main = 2) # Suppress default x-axis
  
  # Add Upper and Lower Bands
  lines(time_range, cumulative_lower_band, col = "red", lty = 2)
  lines(time_range, cumulative_upper_band, col = "blue", lty = 2)
  
  # Add shaded area
  polygon(c(time_range, rev(time_range)),
          c(cumulative_lower_band, rev(cumulative_upper_band)), 
          col = rgb(0.8,0.8,0.8,0.5), border = NA)
  
  # Add custom x-axis
  axis(1, at = seq(1, max(time_range), by = 2), labels = seq(1, max(time_range), by = 2))
  abline(h = seq(0, 1, by = 0.05), col = "lightgray", lty = "dotted")
}


#*******************************************************************************
#*****************__CIR of CR to sa_core_CPI (Sd_Model)_lag_3_*****************#
#*******************************************************************************

# Loop over each VAR model in the list
for (date_filter in names(filtered_var_model)) {
  # Get the filtered VAR model based on the selected date
  var_model_results <- VAR(filtered_var_model[[date_filter]], p=3, type = "const")
  
  # Compute Impulse Response Function
  IR <- irf(var_model_results,
            impulse = "v_CR",
            response = "v_sa_core_CPI",
            n.ahead = 36,
            boot = TRUE,
            ortho = FALSE,
            runs = 100)
  
  # Compute Cumulative Effects
  cumulative_irf <- cumsum(IR$irf[[1]])
  
  # Compute Standard Deviations
  std_dev <- sqrt(apply(IR$irf[[1]], 2, var))
  
  # Compute Upper and Lower Bands
  cumulative_lower_band <- cumulative_irf - 2 * std_dev
  cumulative_upper_band <- cumulative_irf + 2 * std_dev
  
  # Define time range (for illustration purpose)
  time_range <- 0:(length(cumulative_irf)-1)
  
  # Plot without default x-axis
  plot(time_range, cumulative_irf, type = "l", lwd = 2,
       ylim = range(c(cumulative_lower_band, cumulative_upper_band)),
       xlab = "Time", ylab = "Cumulative Effect", 
       main = paste("CIR of CR to sa_core_CPI (Sd_Model) - ", date_filter, ", ", "lag = ", var_model_results$p),
       xaxt = "n",
       cex.main = 2) # Suppress default x-axis
  
  # Add Upper and Lower Bands
  lines(time_range, cumulative_lower_band, col = "red", lty = 2)
  lines(time_range, cumulative_upper_band, col = "blue", lty = 2)
  
  # Add shaded area
  polygon(c(time_range, rev(time_range)),
          c(cumulative_lower_band, rev(cumulative_upper_band)), 
          col = rgb(0.8,0.8,0.8,0.5), border = NA)
  
  # Add custom x-axis
  axis(1, at = seq(1, max(time_range), by = 2), labels = seq(1, max(time_range), by = 2))
  abline(h = seq(0, 1, by = 0.05), col = "lightgray", lty = "dotted")
}


