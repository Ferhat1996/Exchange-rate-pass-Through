#********************************___05_Forecast__******************************#


#****************************___Download_Libraries__***************************#

#install.packages("glmnet")
#install.packages("forecast")
library(glmnet)
library(forecast)

#***************************__Extract_Data__***********************************#
# List the datasets within the EVDS database that contain the terms "exchange" and "rate".
searchCBRT(c("survey", "participants"))

# Identify the appropriate dataset from the list.
# For Currency data:
# Group code: "bie_dkdovytl" 
# Group name: 8: Exchange Rates 

# Display Group information: "bie_dkdovytl" 
showGroupInfo("bie_urbek")

# Extract currency data. Below code directly calculates the monthly average of bid and ask USD/TRY rates.
expectations_2005_2012 <- getDataSeries(c("TP.BEKA.S01.A.A", "TP.BEKA.S01.B.A", "TP.BEKA.S01.C.A", 
                                          "TP.BEKA.S01.E.A", "TP.BEKA.S01.F.A"
), 
freq = 4, 
startDate = "2005-01-01", 
endDate = "2012-12-31")



expectations_2005_2012 <- expectations_2005_2012[day(time) == 15]


# Change the column names
names(expectations_2005_2012)[names(expectations_2005_2012) %in% c(
  "TP.BEKA.S01.A.A", "TP.BEKA.S01.B.A", "TP.BEKA.S01.C.A", 
  "TP.BEKA.S01.E.A", "TP.BEKA.S01.F.A"
)] <- c(
  "CPI_Current_Month",
  "CPI_1_Month_Ahead",
  "CPI_2_Months_Ahead",
  "CPI_12_Months_Ahead",
  "CPI_24_Months_Ahead"
)


expectations_2013_2024 <- getDataSeries(c("TP.BEK.S01.A.A", "TP.BEK.S01.B.A", "TP.BEK.S01.C.A", 
                                          "TP.BEK.S01.E.A", "TP.BEK.S01.F.A"), 
                                        freq = 5, 
                                        startDate = "2013-01-01", 
                                        endDate = "2023-12-15")

# Change the column names
names(expectations_2013_2024)[names(expectations_2013_2024) %in% c(
  "TP.BEK.S01.A.A", "TP.BEK.S01.B.A", "TP.BEK.S01.C.A", 
  "TP.BEK.S01.E.A", "TP.BEK.S01.F.A"
)] <- c(
  "CPI_Current_Month",
  "CPI_1_Month_Ahead",
  "CPI_2_Months_Ahead",
  "CPI_12_Months_Ahead",
  "CPI_24_Months_Ahead"
)


summary(expectations_2005_2012)
summary(expectations_2013_2024)

colnames(expectations_2005_2012)
colnames(expectations_2013_2024)


#***************************__Combine_Data__***********************************#

expectations <- rbind(expectations_2005_2012, expectations_2013_2024)


#*************************__Create_Variables__*********************************#
forecast_cpi <- expectations 
forecast_cpi$realized_yearly_CPI <- yearly_CPI$yearly_CPI


forecast_yearly_cpi <- forecast_cpi[, c("time", "CPI_12_Months_Ahead", "realized_yearly_CPI")]
forecast_yearly_cpi <- forecast_yearly_cpi[-c(1:12), ]


# Calculating error terms
forecast_yearly_cpi$CPI_12_Months_Ahead_error <-  abs(lag(forecast_yearly_cpi$CPI_12_Months_Ahead, 12) - forecast_yearly_cpi$realized_yearly_CPI) / forecast_yearly_cpi$realized_yearly_CPI


CPI_forecast <- forecast_yearly_cpi[ ,c(1,2)]
# CPI_forecast$time <- seq(as.Date("2006-01-15"), length.out = nrow(forecast_yearly_cpi_sd), by = "months")


# Filtering from the first month of 2006 to the last month of 2022
CPI_forecast_filtered <- CPI_forecast[CPI_forecast$time >= as.Date("2006-01-01") & CPI_forecast$time <= as.Date("2022-12-31"), ]

clean_CPI_forecast_filtered <- CPI_forecast_filtered[,2]

# Assigning a time column to the CPI_forecast_filtered dataframe starting from January 15, 2006, sequentially
clean_CPI_forecast_filtered$time <- seq(as.Date("2007-01-15"), length.out = nrow(CPI_forecast_filtered), by = "months")



# Extracting the 'realized_yearly_CPI' column from 'forecast_yearly_cpi'
realized <- forecast_yearly_cpi$realized_yearly_CPI

# Extracting the 'CPI_12_Months_Ahead_error' column from 'forecast_yearly_cpi'
error <- forecast_yearly_cpi$CPI_12_Months_Ahead_error

# Removing the first 12 entries from the 'error' variable
CPI_12_Months_Ahead_error <- error[-(1:12)]

# Removing the first 12 entries from the 'realized' variable
realized_yearly_CPI <- realized[-(1:12)]

# Assigning the modified error and realized CPI values to the respective columns
# in the 'clean_CPI_forecast_filtered' data frame.
clean_CPI_forecast_filtered$CPI_12_Months_Ahead_error <- CPI_12_Months_Ahead_error 
clean_CPI_forecast_filtered$realized_yearly_CPI <- realized_yearly_CPI

# Storing the modified data frame in 'CPI_expectations'.
CPI_expectations <- clean_CPI_forecast_filtered


#*************************__2024_Survey_Expectations__*************************#

data_2023 <- subset(forecast_yearly_cpi, time >= '2023-01-01' & time <= '2023-12-31')
CPI_2024_expectations <- data_2023[, c(1,2)]

CPI_2024_expectations[, time := as.Date(time, format = "%Y-%m-%d")]
start_date <- as.Date("2024-01-15")
CPI_2024_expectations[, time := start_date + (0:(nrow(CPI_2024_expectations) - 1)) * 30]

# Check the results
print(CPI_2024_expectations)


#***********************************ARIMA***************************************


# Calculate the difference between realized and forecasted inflation

month_lag <- -192

estimation_error <- head(CPI_expectations$CPI_12_Months_Ahead_error, month_lag)


# Forecast the difference using the ARIMA model
model <- auto.arima(CPI_expectations$CPI_12_Months_Ahead_error)
arima_estimation_error <- forecast(model, h = 12)

# Print the result
print(arima_estimation_error)


# Create lower and upper bound data sets
lower_bound <- CPI_2024_expectations$CPI_12_Months_Ahead * ( 1 - arima_estimation_error$mean)
upper_bound <- CPI_2024_expectations$CPI_12_Months_Ahead * (  1 + arima_estimation_error$mean)

CPI_2024_arima_expectations <- CPI_2024_expectations

CPI_2024_arima_expectations$lower_bound <- lower_bound
CPI_2024_arima_expectations$upper_bound <- upper_bound


#***********************************LASSO***************************************

# Calculate the difference between realized and forecasted inflation
estimation_error <- CPI_expectations$CPI_12_Months_Ahead_error


# Independent variable matrix
X <- matrix(c(CPI_expectations$time, CPI_expectations$CPI_12_Months_Ahead), ncol=2)

# Determine optimum alpha and lambda values with cross-validation
cvfit <- cv.glmnet(x = X, y = estimation_error)

# Determine the optimum alpha value
optimum_alpha <- cvfit$lambda.min

# Print the optimum alpha value
print(optimum_alpha)

# Fit and train the model
model <- cv.glmnet(x = X, y = estimation_error, alpha = 0.006)

# Independent variable values for the future period
estimation_2024 <- matrix(c(tail(CPI_forecast$time, 12), tail(CPI_forecast$CPI_12_Months_Ahead, 12)), ncol=2)

# Make predictions
lasso_estimation_error <- predict(model, s = "lambda.min", newx = estimation_2024)

print(lasso_estimation_error)

# Create lower and upper bound data sets
lasso_lower_bound <- CPI_2024_expectations$CPI_12_Months_Ahead * ( 1 - lasso_estimation_error)
lasso_upper_bound <- CPI_2024_expectations$CPI_12_Months_Ahead * ( 1 + lasso_estimation_error)
lasso_lower_bound_1 <- CPI_forecast$CPI_12_Months_Ahead * mean(( 1 - lasso_estimation_error))
lasso_upper_bound_1<- CPI_forecast$CPI_12_Months_Ahead * mean(( 1 + lasso_estimation_error))

CPI_2024_lasso_expectations <- CPI_2024_expectations
CPI_2024_lasso_expectations$lasso_lower_bound <- lasso_lower_bound
CPI_2024_lasso_expectations$lasso_upper_bound <- lasso_upper_bound



#**********************__Plot_the_Models_2007-2018_****************************#

# First plot - CPI_forecast
plot(CPI_forecast$time, CPI_forecast$CPI_12_Months_Ahead, type="l", xlim=c(as.Date("2007-01-01"), as.Date("2017-12-31")), ylim=c(0, 20), xlab="Time", ylab="CPI(%)", col="blue", lwd=2, main=" LASSO CPI Forecast - 2007-2018")
lines(CPI_forecast$time, CPI_forecast$realized_yearly_CPI, type="l", col="green", lwd=2) # Realized CPI within CPI_forecast

# Second plot - CPI_2024_lasso_expectations
lines(dates, CPI_2024_lasso_expectations$CPI_12_Months_Ahead, type="l", ylim=c(10, 70), xlab="Time", ylab="CPI", col="blue", lwd=2, xaxt="n")

lines(CPI_forecast$time, lasso_lower_bound_1, col="red", lty=2, lwd=1) # Lower bound
lines(CPI_forecast$time, lasso_upper_bound_1, col="red", lty=2, lwd=1) # Upper bound
lines(dates, lasso_lower_bound, col="red", lty=2, lwd=1) # Lower bound
lines(dates, lasso_upper_bound, col="red", lty=2, lwd=1) # Upper bound


# Shade the area
polygon(c(dates, rev(dates)), c(lasso_lower_bound, rev(lasso_upper_bound)), col=rgb(0.8,0.8,0.8,0.5), border=NA)
polygon(c(CPI_forecast$time, rev(CPI_forecast$time)), c(lasso_lower_bound_1, rev(lasso_upper_bound_1)), col=rgb(0.8,0.8,0.8,0.5), border=NA)

legend("topleft", legend=c("CPI 12 Months Ahead", "Realized CPI", "Lasso Confidence Interval"), col=c("blue", "green", "red"), lty=c(1, 1,2), lwd=c(2,2, 1), cex=0.8)


#************************__Plot_the_Models_2018-2024__*************************#

# First plot - CPI_forecast
plot(CPI_forecast$time, CPI_forecast$CPI_12_Months_Ahead, type="l", xlim=c(as.Date("2018-01-01"), as.Date("2024-12-31")), ylim=c(0, 100), xlab="Time", ylab="CPI(%)", col="blue", lwd=2, main=" LASSO/ARIMA CPI Forecast - 2018-2024")
lines(CPI_forecast$time, CPI_forecast$realized_yearly_CPI, type="l", col="green", lwd=2) # Realized CPI within CPI_forecast

# Second plot - CPI_2024_lasso_expectations
lines(dates, CPI_2024_lasso_expectations$CPI_12_Months_Ahead, type="l", xlab="Time", ylab="CPI", col="blue", lwd=2, xaxt="n")
lines(dates, CPI_2024_arima_expectations$CPI_12_Months_Ahead, type="l", col="red", lwd=2) 

lines(CPI_forecast$time, lasso_lower_bound_1, col="red", lty=2, lwd=1) # Lower bound
lines(CPI_forecast$time, lasso_upper_bound_1, col="red", lty=2, lwd=1) # Upper bound
lines(dates, lasso_lower_bound, col="red", lty=2, lwd=1) # Lower bound
lines(dates, lasso_upper_bound, col="red", lty=2, lwd=1) # Upper bound

# Plot the confidence intervals
lines(dates, lower_bound, col="purple", lty=2, lwd=1) # Lower bound
lines(dates, upper_bound, col="purple", lty=2, lwd=1) # Upper bound



# Shade the areas
polygon(c(dates, rev(dates)), c(lasso_lower_bound, rev(lasso_upper_bound)), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(c(CPI_forecast$time, rev(CPI_forecast$time)), c(lasso_lower
                                                        
                                                        