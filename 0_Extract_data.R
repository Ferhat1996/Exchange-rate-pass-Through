
#*******************************___0_Extract_Data___***************************#

#****************************___Download_Libraries___**************************#

#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("devtools")
#install_github("etaymaz/CBRT")     # Neccesary to extract data from EVDS and TURKSTAT
#install.packages("missForest")
#install.packages("forecast")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("xts")
#install.packages("seastests")
#install.packages("zoo")
#install.packages("stats")
#install.packages("graphics")
#install.packages("utils")
#install.packages("tseries")
#install.packages("seasonal")
#install.packages("urca")
#install.packages("zoo")



library(dplyr)
library(lubridate)
library(devtools)
library(CBRT)
library(missForest)
library(forecast)
library(ggplot2)
library(gridExtra)
library(xts)
library(seastests)
library(zoo)
library(stats)
library(graphics)
library(utils)
library(tseries)
library(seasonal)
library(urca)
library(zoo)

#*****************************___Create_Account___*****************************#

# Create and account from "https://evds2.tcmb.gov.tr/index.php?/evds/login"
# You can see the API Key with these steps:  
# Sign in  
# Click your username  
# Select Profile menu  
# Select API Key option 

#Assign your individual API
myCBRTKey <- "**********"     # The API is neccesary to 	extract data from the EVDS data base

#******************************___Assign_Dates___******************************#

#Assign dates
startDate <- "01-01-2005"
endDate <- "31-12-2023"


#*****************************___Create_Data_Sets___***************************#

# The process of extracting currency datasets is explained in detail as an example.
# Other variables are only briefly described unless they have unique aspects 
# that warrant further explanation.

#*********************************___USD/TRY___********************************#

# List the datasets within the EVDS database that contain the terms "exchange" and "rate".
searchCBRT(c("exchange", "rate"))

# Identify the appropriate dataset from the list.
# For Currency data:
# Group code: "bie_dkdovytl" 
# Group name: 8: Exchange Rates 

# Display Group information: "bie_dkdovytl" 
showGroupInfo("bie_dkdovytl")

# Extract currency data. Below code directly calculate montly average of bid and ask USD/TRY rates.
Currency_rate <- getDataSeries(c("TP.DK.USD.A.YTL", "TP.DK.USD.S.YTL"), freq = 5, startDate = startDate, endDate = endDate)

# Calculate the average of the bid and ask rates columns and replace these columns with the average of these columns
Currency_rate$USD_TRY  <- rowMeans(Currency_rate[, 2:3])

# Clear data
Currency_rate <- Currency_rate[, -c(2, 3)]

#*************___Rolling_03-month_standard_deviation_of_USD_TRY__**************#


# First, convert the data into a time series object
sd_cr <- ts(Currency_rate, start = c(2005,01), frequency = 12)

# Calculate the rolling 3-month standard deviation

sd_cr <- Currency_rate[ ,2]
rolling_sd_cr <- rollapply(sd_cr, width = 3, FUN = sd, align = "right", fill = NA)


rolling_sd_cr_ts <- ts(rolling_sd_cr, start = c(2005,1), frequency = 12)
rolling_sd_cr_xts <- xts(rolling_sd_cr[,1], order.by = seq(as.Date("2005-01-01"), by = "month", length.out = nrow(rolling_sd_cr)))

# Remove D0lk 12 satD1rD1 ve son satD1rD1 silme
rolling_sd_cr_xts <- rolling_sd_cr_xts[-c(1:12, nrow(rolling_sd_cr_xts)), ]

#***********************************___CPI___**********************************#

# Consumer Price Index (2003=100)(source:TURKSTAT)
searchCBRT("CPI")
showGroupInfo("bie_feoktg")

# Identify the appropriate Series code from the data list.

# Extract consumer price index data set.
CPI <- getDataSeries("TP.FE.OKTG01", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a CPI.
names(CPI)[names(CPI) == "TP.FE.OKTG01"] <- "CPI"

# Extract consumer price excluding seasonal products index data set.
#zsp_CPI <- getDataSeries("TP.FE.OKTG02", freq = 1, startDate = startDate, endDate = endDate)

# Change the column name as a CPI.
#names(sa_CPI)[names(sa_CPI) == "TP.FE.OKTG02"] <- "sa_CPI"


#********************************___Core_CPI ___*******************************#

# Core Consumer Price Index (2003=100)(source:TURKSTAT)
# Core Inflation  means CPI excluding energy, food and non-alcoholic beverages, alcoholic beverages, tobacco and gold.

# GroupInfo of Core_CPI is same with CPI.
# Identify the appropriate Series code from the data list.

# Extract core consumer price index data set.
Core_CPI <- getDataSeries("TP.FE.OKTG03", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a Core_CPI.
names(Core_CPI)[names(Core_CPI) == "TP.FE.OKTG03"] <- "Core_CPI"


#******************************_Import_Price_Index_****************************#

# The searchCBRT results are temporarily assigned to a dataset for easier identification of the appropriate data set.
Temporary_data_set <- searchCBRT(c("import", "index", "price"))
showGroupInfo("bie_dtitfby")

# Remove Temporary_data_set
rm(Temporary_data_set)

# There a two seperate import price index in TURKSTAT database
# First one is Trade Import Unit Value Index by Classification of BEC (2003=100)(TURKSTAT)
IPI_2005_2012 <- getDataSeries("TP.DT.IT.FIY.B01.Y", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a IPI.
names(IPI_2005_2012)[names(IPI_2005_2012) == "TP.DT.IT.FIY.B01.Y"] <- "IPI"


# Second one is Foreign Trade Import Unit Value Index by Classification of BEC (2010=100)(TURKSTAT)
showGroupInfo("bie_dtitfb10")
IPI_2013_2023 <- getDataSeries("TP.DT.IT.FIY.D01.2010", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a IPI.
names(IPI_2013_2023)[names(IPI_2013_2023) == "TP.DT.IT.FIY.D01.2010"] <- "IPI"

# To get single data set change the index value first data set from 2003=100 to 2010 = 100.
# Identify index value of "2010-01-15" in first (2003=100) data set. 
original_index_2010 <- IPI_2005_2012[IPI_2005_2012$time == "2010-01-15", ]

# Seperate index values from first data set.
index_values_1 <- IPI_2005_2012$IPI

# Calculate updated index values of first data and change this values with previous ones.
normalized_index_values_1 <- index_values_1 / as.numeric(original_index_2010[,2]) * 100
IPI_2005_2012$IPI <- normalized_index_values_1

# Merge the data sets
IPI <- rbind(IPI_2005_2012, IPI_2013_2023) #Import Price Index

# Remove Temporary_data_set
rm(IPI_2005_2012, IPI_2013_2023, original_index_2010)

#*************************__Consumer_Confidence_Index__************************#

# The searchCBRT results are temporarily assigned to a dataset for easier identification of the appropriate data set.
Temporary_data_set <- searchCBRT(c("confidence", "index"))
showGroupInfo("bie_mbgven")     #Consumer confidence index (Archive)(TURKSTAT)
showGroupInfo("bie_mbgven2")    #Consumer confidence index (TURKSTAT)

# Remove Temporary_data_set
rm(Temporary_data_set)

# There a two seperate import price index in TURKSTAT database
# First one is Consumer confidence index (Archive)(TURKSTAT)
CCI_2005_2012 <- getDataSeries("TP.TG.EN", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a CCI.
names(CCI_2005_2012)[names(CCI_2005_2012) == "TP.TG.EN"] <- "CCI"

# First one is Consumer confidence index (TURKSTAT)
CCI_2013_2023 <- getDataSeries("TP.TG2.Y01", freq = 5, startDate = "2013-01-15", endDate = endDate)

# Change the column name as a CCI.
names(CCI_2013_2023)[names(CCI_2013_2023) == "TP.TG2.Y01"] <- "CCI"

# Merge the data sets
CCI <- rbind(CCI_2005_2012, CCI_2013_2023)

# Remove Temporary_data_set
rm(CCI_2005_2012, CCI_2013_2023)


#*********************************___M3_Supply___******************************#

searchCBRT(c("money", "supply"))
money_supply_sources <- showGroupInfo("bie_pbpanal2")

# If anyone want to control the variable who can follow the below steps:
# https://evds2.tcmb.gov.tr/index.php?/evds/serieMarket/collapse_4/5896/DataGroup/turkish/bie_pbpanal2/
# First click 5.M3 (after 2005/12) and 6.M3 (before 2005/11) button in the Monetary Aggregates and Counterpart Items (Thousand TRY)(Monthly) 
# Secondly click Add buton. Then click create report symbol under the report settings.
# Finally take series number of M3 (after 2005/12) an M3 (before 2005/12) in the series descriptions.

# Money supply M3 before 2005/12 data set
M3_before_12_2005 <- getDataSeries("TP.PBD.H26", freq = 5, startDate = startDate, endDate = "15-11-2005")

# Change the column name as a M3.
names(M3_before_12_2005)[names(M3_before_12_2005) == "TP.PBD.H26"] <- "M3"

# Money supply M3 after 2005/12 data set
M3_after_12_2005 <- getDataSeries("TP.PBD.H17", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a M3.
names(M3_after_12_2005)[names(M3_after_12_2005) == "TP.PBD.H17"] <- "M3"

# Merge data sets
M3 <- rbind(M3_before_12_2005, M3_after_12_2005)

# Remove Temporary_data_set
rm(M3_after_12_2005, M3_before_12_2005, money_supply_sources)

# Calculate Billion values of M3 and change this values with previous ones.
Billion_values_M3 <- M3$M3/1000000
M3$M3 <- Billion_values_M3

# Add currency column to the CBRT_Reserves data set
m3_data <- merge(M3, Currency_rate, by = "time")

M3$M3 <- m3_data$M3 / m3_data$USD_TRY

# Remove Temporary_data_set
rm(m3_data)

#*******************************___CBRT_Reserves___****************************#

searchCBRT(c("balance", "sheet"))
searchCBRT(c("central", "swap"))

#CBRT_Reserves_data_set_balance_sheet <- showGroupInfo("bie_abanlbil")
#CBRT_Reserves_data_set_swap <- showGroupInfo("bie_abmbbil1")


CBRT_Reserves <- getDataSeries(c("TP.AB.A02","TP.AB.A10","TP.DOVVARNC.K14"), freq = 5, startDate = startDate, endDate = endDate)

# Change the columns names.
names(CBRT_Reserves)[names(CBRT_Reserves) == "TP.AB.A02"] <- "FOREIGN ASSETS(Thousand TRY)"
names(CBRT_Reserves)[names(CBRT_Reserves) == "TP.AB.A10"] <- "TOTAL FOREIGN LIABILITIES(Thousand TRY)"
names(CBRT_Reserves)[names(CBRT_Reserves) == "TP.DOVVARNC.K14"] <- "SWAPS"


# Add currency column to the CBRT_Reserves data set
CBRT_Reserves <- merge(CBRT_Reserves, Currency_rate, by = "time")

# Calculate Net Reserves
CBRT_Reserves$Net_Reserves <- (CBRT_Reserves[,2] - CBRT_Reserves[,3]) / (CBRT_Reserves[,5]*1000)

# Update NA cells with "0" in the Swaps column.
CBRT_Reserves <- CBRT_Reserves %>%
  mutate_at(4, ~ifelse(is.na(.), 0, .))

# Calculate Net Reserves without swaps.
CBRT_Reserves$CBRT_Net_reserves_excluding_swaps <- CBRT_Reserves[,4] + CBRT_Reserves[,6]

# Create new dataframe for VAR studies
CBRT_Net_reserves_excluding_swaps <- data.frame(time = CBRT_Reserves$time, CBRT_Net_reserves_excluding_swaps = CBRT_Reserves$CBRT_Net_reserves_excluding_swaps)

# Calculate Billion values of Net_reserves_excluding_swaps and change this values with previous ones.
Billion_values_reserve <- CBRT_Net_reserves_excluding_swaps$CBRT_Net_reserves_excluding_swaps/1000
CBRT_Net_reserves_excluding_swaps$CBRT_Net_reserves_excluding_swaps <- Billion_values_reserve

summary(CBRT_Net_reserves_excluding_swaps)

#*******************************___Policy_Rate ___*****************************#

# (2 to 14 DAYS)Net Proceeds (TRY Thousand)(REPO) 

searchCBRT(c("week", "repo", "interest"))
showGroupInfo("bie_pyrepo")     

policy_rate <- getDataSeries("TP.API.REP.ORT.G214", freq = 5, startDate = startDate, endDate = endDate)

# Change the column name as a Policy_Rate.
names(policy_rate)[names(policy_rate) == "TP.API.REP.ORT.G214"] <- "Policy_Rate"


#*************************___Calculate_Yearly_CPI_Rates___**********************#

# Creating a vector to hold the yearly CPI, seasonally adjusted CPI and core CPI rates
yearly_CPI <- numeric(length(CPI$CPI))
Core_yearly_CPI <- numeric(length(Core_CPI$Core_CPI))

# Calculating yearly CPI rates using a for loop
for (i in 13:length(CPI$CPI)) {
  yearly_CPI[i] <- (CPI$CPI[i] / CPI$CPI[i - 12] - 1) * 100
}

for (i in 13:length(Core_CPI$Core_CPI)) {
  Core_yearly_CPI[i] <- (Core_CPI$Core_CPI[i] / Core_CPI$Core_CPI[i - 12] - 1) * 100
}

yearly_CPI <- data.frame(yearly_CPI, order.by = as.Date(CPI$time))
Core_yearly_CPI <- data.frame(Core_yearly_CPI, order.by = as.Date(Core_CPI$time))

colnames(yearly_CPI)[2] <- "time"
colnames(Core_yearly_CPI)[2] <- "time"


# Displaying the results
print(yearly_CPI)
print(Core_yearly_CPI)


plot(yearly_CPI)

#***********************___Calculate_Reel_Interest_Rates___********************#

# Create a data set which includes all variables
data_list_rir <- list(yearly_CPI, Core_yearly_CPI, policy_rate)
Reel_interest_rates <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), data_list_rir)

# Fill missing value of policy rate with missForest
#VAR_data_filtered$time <- as.numeric(VAR_data_filtered$time)
#VAR_data_imputed <- missForest(VAR_data_filtered)

# Fill missing value of policy rate with na.interp
Reel_interest_rates$Policy_Rate <- as.numeric(na.interp(Reel_interest_rates$Policy_Rate))

# Summary of the data
summary(Reel_interest_rates)

# Filter data according to date
Reel_interest_rates <- Reel_interest_rates %>% 
  filter(time >= "2006-01-15" & time <= "2023-11-15")

# Calculate reel interest rates
Reel_interest_rates$rir <- (( 100 + Reel_interest_rates$Policy_Rate) / ( 100 + Reel_interest_rates$yearly_CPI) - 1) * 100
Reel_interest_rates$core_rir <- (( 100 + Reel_interest_rates$Policy_Rate ) / ( 100 + Reel_interest_rates$Core_yearly_CPI) - 1) * 100

# Indicate results
print(Reel_interest_rates)

# Summary of the Reel_interest_rates
summary(Reel_interest_rates)

policy_rate <- data.frame(Reel_interest_rates$Policy_Rate, order.by = as.Date(Reel_interest_rates$time))
rir <- data.frame(Reel_interest_rates$rir, order.by = as.Date(Reel_interest_rates$time))
core_rir <- data.frame(Reel_interest_rates$core_rir, order.by = as.Date(Reel_interest_rates$time))

colnames(policy_rate) <- c("policy_rate", "time")
colnames(rir) <- c("rir", "time")
colnames(core_rir) <- c("core_rir", "time")



