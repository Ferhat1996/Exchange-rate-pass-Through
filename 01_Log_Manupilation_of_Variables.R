
#***********************___01_Log_Manupilation_of_Variables__*******************#

#install.packages("vars")
#install.packages("mFilter")
#install.packages("tseries")
#install.packages("TSstudio")
#install.packages("tidyverse")
#install.packages("xts")
#install.packages("lubridate")
#install.packages("uroot")
#install.packages("seasonal")


library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(xts)
library(lubridate)
library(uroot)
library(seasonal)


#******************************___Create_VAR_Data___***************************#

# Create a data set which includes all variables
data_list <- list(Currency_rate, CPI, Core_CPI, IPI, CCI, M3, CBRT_Net_reserves_excluding_swaps, policy_rate, rir, core_rir)
VAR_data_pre <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), data_list)

# Summary of the data
summary(VAR_data_pre)

# Filter data according to date
VAR_data <- VAR_data_pre %>% 
  filter(time >= "2006-01-15" & time <= "2023-11-15")

VAR_data$sd_CR <-rolling_sd_cr_xts


# Summary of the data
summary(VAR_data)


#**************************___plot_core_CPI_and_USD/TRY__**********************#

# Convert VAR_data$Core_CPI into a ts object starting from January 2006
ccpi_ts <- ts(VAR_data$Core_CPI, start=c(2006, 1), frequency=12)

# Convert VAR_data$USD_TRY into a ts object starting from January 2006
ER_ts <- ts(VAR_data$USD_TRY, start=c(2006, 1), frequency=12)

# Since you want the data up to November 2023, first determine how many months that is
end_year <- 2023
end_month <- 11
months_duration <- (end_year - 2006) * 12 + (end_month - 1)

# Truncate or subset the ts object to keep data up to November 2023
core_cpi_ts_subset <- window(ccpi_ts, start=c(2006, 1), end=c(2023, 11))

# Set up the plotting area with two plots side by side
par(mfrow=c(1,2))

# Plot Core CPI
plot(core_cpi_ts_subset, xlab="Year", ylab="Core CPI", col="red", lwd=2, main="Core CPI from 2006 to 2023")
grid(nx=NULL, ny=NULL, lty="solid", col="gray", lwd=par("lwd"), equilogs=TRUE)

# Plot USD to TRY exchange rate
plot(ER_ts, xlab="Year", ylab="USD to TRY", col="blue", lwd=2, main="USD to TRY Exchange Rate from 2006 to 2023")
grid(nx=NULL, ny=NULL, lty="solid", col="gray", lwd=par("lwd"), equilogs=TRUE)

# Reset the plotting area to default
par(mfrow=c(1,1))



#******************************___log_transformation__***************************#


# Create an empty list to store the log of each xts variable, copying the structure of VAR_data
log_VAR_data <- VAR_data

# Loop through each xts variable in VAR_data and take the logarithm
for (i in 1:length(VAR_data)) {
  # Check if the current variable is not 'CBRT_Net_reserves_excluding_swaps' and not 'rir', 'sa_rir', and 'core_rir'
  if (!(names(VAR_data)[i] %in% c("CBRT_Net_reserves_excluding_swaps", "rir", "sa_rir", "core_rir", "sd_CR"))) {
    # Check if the variable is numeric (excluding "Date" objects)
    if (is.numeric(VAR_data[[i]])) {
      log_VAR_data[[i]] <- log(VAR_data[[i]])
    }
  } else if (names(VAR_data)[i] == "CBRT_Net_reserves_excluding_swaps") {
    # Special manipulation for 'CBRT_Net_reserves_excluding_swaps' before logging
    min_value <- min(VAR_data[[i]], na.rm = TRUE)  # Find the minimum value
    add <- 1 - min_value  # Calculate the value to be added to shift all values to positive
    
    # Shift values to positive and log-transform
    log_VAR_data[[i]] <- log(VAR_data[[i]] + add)
  } else {
    # For 'rir', 'sa_rir', and 'core_rir', simply copy the original values without taking the logarithm
    log_VAR_data[[i]] <- VAR_data[[i]]
  }
}

# Add "log_" prefix to each name in xts_log_list
for (i in 1:(ncol(log_VAR_data)-4)) {
  names(log_VAR_data)[i] <- paste0("log_", names(log_VAR_data)[i])
}

# Print the resulting xts_log_list
log_VAR_data

names(log_VAR_data)

#****************************___Plot_the_Variables___**************************#

# Create an empty list to store plots
plot_list <- list()

# Loop through column names
for(column_name in colnames(log_VAR_data)[-which(colnames(log_VAR_data) == "log_time")]) {
  p <- ggplot(log_VAR_data, aes_string(x = "log_time", y = column_name)) +
    geom_line() + 
    theme_minimal() +
    ggtitle(paste("Time-Series:", column_name))
  
  plot_list[[column_name]] <- p
}

# Display all graphs in a single page.
do.call(grid.arrange, c(plot_list, ncol = 2))

# Please click the Export ---> copy the clipboard to see all graphs in a single page



#***************************___Convert_Variables_ts__**************************#

# In order to control seasonality convert the variables ts object.

# Remove time variable
log_VAR_data_ts <- log_VAR_data[,-1]

# Create an empty list
ts_log_list <- list()

# Converting each variable into a ts object with a for loop
for (i in 1:length(log_VAR_data_ts)) {
  ts_log_list[[i]] <- ts(log_VAR_data_ts[[i]], start = c(2006,1), frequency = 12)
}

# Assign names
names(ts_log_list) <- c("log_Currency_rate", "log_CPI", "log_Core_CPI", "log_IPI", "log_CCI", "log_M3", "log_CBRT_Net_reserves_excluding_swaps", "log_policy_rate", "rir", "core_rir", "sd_CR")

# Indicate results
ts_log_list



#*****************************___Plot_Variables__****************************#

# Display variables individualy
# Update graphics settings

par(cex.lab = 2, cex.axis = 2, cex.main = 4, mgp = c(2.7, 1, 0))

# old_par <- par()

ts.plot(ts_log_list$log_Currency_rate, 
        xlab = "year", 
        ylab = "log_USD_TRY", 
        col = "red", 
        lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "LOGARITHM OF USD/TRY", cex.main = 4)


#******************************************************************************#

ts.plot(ts_log_list$log_CPI, xlab = "year", ylab = "log_CPI", col = "red", lwd = 8)
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)
# Add title
title(main = "LOGARITHM OF CPI", cex.main = 4)

#******************************************************************************#

ts_log_list$log_sa_CPI <- log_sa_CPI
ts_log_list$log_sa_core_CPI <- log_sa_core_CPI

#******************************************************************************#

ts.plot(ts_log_list$log_sa_core_CPI, xlab = "year", ylab = "Log_sa_core_CPI", col = "red", lwd = 4)
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)
# Add title
title(main = "LOGARITHM OF SEASONALLY ADJUSTED CORE CPI", cex.main = 2)

#******************************************************************************#

# Plot time series data
# Plot time series data
ts.plot(ts_log_list$log_sa_core_CPI, 
        xlab = "Year", 
        ylab = "Log_sa_core_CPI", 
        col = "red", 
        lwd = 8)


# Define the shaded areas
rect(xleft = index(ts_log_list$log_sa_core_CPI)[151], 
     xright = index(ts_log_list$log_sa_core_CPI)[156], 
     ybottom = par("usr")[3], 
     ytop = par("usr")[4], 
     col = rgb(1, 1, 0, alpha = 0.3), border = NA)

# Define the shaded areas
rect(xleft = index(ts_log_list$log_sa_core_CPI)[190], 
     xright = index(ts_log_list$log_sa_core_CPI)[195], 
     ybottom = par("usr")[3], 
     ytop = par("usr")[4], 
     col = rgb(1, 1, 0, alpha = 0.3), border = NA)

# Add text for the shaded areas
text(x = (index(ts_log_list$log_sa_core_CPI)[151] + index(ts_log_list$log_sa_core_CPI)[156]) / 2, 
     y = max(ts_log_list$log_sa_core_CPI, na.rm = TRUE), 
     labels = "2018Q3-2018Q4", 
     pos = 3,
     font = 2)

text(x = (index(ts_log_list$log_sa_core_CPI)[190] + index(ts_log_list$log_sa_core_CPI)[195]) / 2, 
     y = max(ts_log_list$log_sa_core_CPI, na.rm = TRUE), 
     labels = "2022Q4-2023Q1", 
     pos = 3,
     font = 2)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"))

# Add title
title(main = "LOGARITHM OF SEASONALLY ADJUSTED CORE CPI", cex.main = 4)

#******************************************************************************#

ts.plot(ts_log_list$log_Core_CPI, xlab = "year", ylab = "Log_Core_CPI", col = "red", lwd = 8)
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)
# Add title
title(main = "LOGARITHM OF CORE CPI")

#******************************************************************************#

ts.plot(ts_log_list$log_IPI, xlab = "year", ylab = "log_IPI", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "LOGARITHM OF IMPORT PRICE INDEX", cex.main = 4)

#******************************************************************************#
ts.plot(ts_log_list$log_CCI, xlab = "year", ylab = "log_CCI", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "LOGARITHM OF CONSUMER CONFIDENCE INDEX", cex.main = 4)

#******************************************************************************#
ts.plot(ts_log_list$log_M3, xlab = "year", ylab = "log_M3", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "LOGARITHM OF MONEY SUPPLY", cex.main = 4)

#******************************************************************************#

#******************************************************************************#

ts.plot(ts_log_list$log_CBRT_Net_reserves_excluding_swaps, xlab = "year", ylab = "Log_CBRT_Net_reserves_excluding_swaps(billion dollar) ", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "LOGARITHM OF TR CENTRAL BANK NET USD RESERVES", cex.main = 2)

#******************************************************************************#

ts.plot(ts_log_list$log_policy_rate, xlab = "year", ylab = "Policy_Rate", col = "red", lwd = 4)

#******************************************************************************#

ts.plot(ts_log_list$rir, xlab = "year", ylab = "reel interest rate (%)", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "REEL INTEREST RATE (no core)", cex.main = 4)

#******************************************************************************#
ts.plot(ts_log_list$core_rir, xlab = "year", ylab = "core_rir", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "REEL INTEREST RATE", cex.main = 4)


ts.plot(ts_log_list$sd_CR, xlab = "year", ylab = "log_sd_CR", col = "red", lwd = 8)

# Add grid with squares
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)

# Add title
title(main = "Logarithm of 3- months rolling standard deviations of exchange rate", cex.main = 2)

#*****************************___Plot_Seasonality__****************************#

# Creating a graph for each time series using "ggseasonplot"
for (i in names(ts_log_list)) {
  print(ggseasonplot(ts_log_list[[i]], main = paste("Seasonal Plot of", i)))
}

# Creating a graph for each time series using "ggsubseriesplot"
for (i in names(ts_log_list)) {
  print(ggsubseriesplot(ts_log_list[[i]], main = paste("Seasonal Plot of", i)))
}

#*****************************___Check_Seasonality__***************************#


# Apply the seas function to each time series in ts_log_list and handle errors
models <- list()

for(name in names(ts_log_list)) {
  ts_data <- ts_log_list[[name]]
  tryCatch({
    # Apply SEATS model
    model <- seas(ts_data)
    # Add the model results to the list
    models[[name]] <- model
  }, error = function(e) {
    # Print the error message in case of an error
    cat("Error occurred for:", name, "\n", e$message, "\n\n")
  })
}

# Print the summary of each model
lapply(models, summary)


#**************************___Seasonality_Adjustment__**************************#

# In this section, I'm performing seasonal adjustment for both CPI and Core CPI.

CPI_adj <- CPI[-(1:12), ]
CPI_adj_data <- ts(CPI_adj[,2], start=c(2006,1), frequency=12)
seas_fit_CPI <- seas(x=CPI_adj_data)
sa_CPI <- final(seas_fit_CPI)
plot(sa_CPI)

Core_CPI_adj <- Core_CPI[-(1:12), ]
Core_CPI_adj_data <- ts(Core_CPI_adj[,2], start=c(2006,1), frequency=12)
seas_fit_core_CPI <- seas(x=Core_CPI_adj_data)
sa_core_CPI <- final(seas_fit_core_CPI)
plot(sa_core_CPI)


log_sa_CPI <- log(sa_CPI)
log_sa_core_CPI <- log(sa_core_CPI)


summary(seas(log_sa_CPI))
summary(seas(ts_log_list$log_CPI))

summary(seas(log_sa_core_CPI))
summary(seas(ts_log_list$log_Core_CPI))

# Assigning the seasonally adjusted CPI and Core CPI data to a new column in ts_log_list

log_sa_CPI_pre <- log_sa_CPI[-length(log_sa_CPI)]
log_sa_CPI <- ts(log_sa_CPI_pre, start=c(2006,1), frequency=12)

log_sa_core_CPI_pre <- log_sa_core_CPI[-length(log_sa_core_CPI)]
log_sa_core_CPI <- ts(log_sa_core_CPI_pre, start=c(2006,1), frequency=12)


ts_log_list$log_sa_CPI <- log_sa_CPI
ts_log_list$log_sa_core_CPI <- log_sa_core_CPI


ts.plot(ts_log_list$log_sa_CPI, xlab = "year", ylab = "sd_CR", col = "red", lwd = 4)
ts.plot(ts_log_list$log_CPI, xlab = "year", ylab = "CPI", col = "red", lwd = 4)

ts.plot(ts_log_list$log_sa_core_CPI, xlab = "year", ylab = "sd_CR", col = "red", lwd = 4)
ts.plot(ts_log_list$log_Core_CPI, xlab = "year", ylab = "Core_CPI", col = "red", lwd = 4)



#***************************___Convert_Variables_xts__*************************#

# In order to control stationary convert the variables ts object.

# create an empty list
xts_log_list <- list()

# Transformation
for (i in 1:length(ts_log_list)) {
  zaman_damgalari <- seq(from = as.Date("2006-01-15"), length.out = length(ts_log_list[[i]]), by = "month")
  xts_log_list[[i]] <- xts(ts_log_list[[i]], order.by = zaman_damgalari)
}


# Assign the names
names(xts_log_list) <- names(ts_log_list)

# Check the result
xts_log_list

