#***********************__02_1_Philips_equation_with_dummy__*******************#

#****************************___Download_Libraries___**************************#

#install.packages("openxlsx")
library(openxlsx)

#*****************************___Data_Manupulation___**************************#
#*
# Split 'time' column into month and year
log_VAR_data$log_sa_CPI <- log_sa_CPI
log_VAR_data$log_sa_core_CPI <- log_sa_core_CPI

log_VAR_data$month <- format(log_VAR_data$log_time, "%m")
log_VAR_data$year <- format(log_VAR_data$log_time, "%Y")

# Take only the 3rd, 6th, 9th, and 12th months
quarterly_log_VAR_data <- log_VAR_data[log_VAR_data$month %in% c("03", "06", "09", "12"), ]

# Convert data to ts format
quarterly_ts <- ts(quarterly_log_VAR_data[,-1], start = c(2006,1), frequency = 4)

# Drop the last two columns
quarterly_ts <- quarterly_ts[, -c(ncol(quarterly_ts)-1, ncol(quarterly_ts))]

# Transform ts format to the xts format
quarterly_xts <- as.xts(quarterly_ts)
quarterly_xts_diff <- diff(quarterly_xts, differences = 1)

quarterly_xts_diff <- quarterly_xts_diff[-1, ]
quarterly_xts_diff$sd_CR <- quarterly_xts$sd_CR[-1, ]

# Result
print(quarterly_xts_diff)



#*****************************___Plot_Variables__****************************#

# Display variables individualy
ts.plot(quarterly_xts$log_USD_TRY, xlab = "year", ylab = "USD_TRY", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_CPI, xlab = "year", ylab = "CPI", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_sa_CPI, xlab = "year", ylab = "sa_CPI", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_Core_CPI, xlab = "year", ylab = "Core_CPI", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_sa_core_CPI, xlab = "year", ylab = "Core_CPI", col = "red", lwd = 4)
grid(nx = NULL, ny = NULL, lty = "solid", col = "gray", lwd = par("lwd"), equilogs = TRUE)
# Add title
title(main = "Logarithm of CPI")


ts.plot(quarterly_xts$log_IPI, xlab = "year", ylab = "IPI", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_CCI, xlab = "year", ylab = "CCI", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_M3, xlab = "year", ylab = "M3", col = "red", lwd = 4)
ts.plot(quarterly_xts$log_CBRT_Net_reserves_excluding_swaps, xlab = "year", ylab = "CBRT_Net_reserves_excluding_swaps", col = "red", lwd = 4)
ts.plot(quarterly_xts$policy_rate, xlab = "year", ylab = "Policy_Rate", col = "red", lwd = 4)
ts.plot(quarterly_xts$rir, xlab = "year", ylab = "rir", col = "red", lwd = 4)
ts.plot(quarterly_xts$core_rir, xlab = "year", ylab = "core_rir", col = "red", lwd = 4)
ts.plot(quarterly_xts$sd_CR, xlab = "year", ylab = "sd_CR", col = "red", lwd = 4)



#*****************************___Plot_Variables__****************************#

# Calculation of lags and adding them to the dataset
quarterly_xts_diff$lag_log_USD_TRY_1 <- lag(quarterly_xts_diff$log_USD_TRY, 1)
quarterly_xts_diff$lag_log_USD_TRY_2 <- lag(quarterly_xts_diff$log_USD_TRY, 2)
quarterly_xts_diff$lag_log_USD_TRY_3 <- lag(quarterly_xts_diff$log_USD_TRY, 3)
quarterly_xts_diff$lag_log_USD_TRY_4 <- lag(quarterly_xts_diff$log_USD_TRY, 4)

quarterly_xts_diff$lag_log_sa_core_CPI_1 <- lag(quarterly_xts_diff$log_sa_core_CPI, 1)
quarterly_xts_diff$lag_log_IPI_1 <- lag(quarterly_xts_diff$log_IPI, 1)


#*****************************___Define_directory__****************************#

# Target directory
directory <- "C:/Users/Lenovo/Desktop/Data_analysis/"

# Folder name
folder_name <- "Model_output_dummy"
folder_name_2 <- "Model_output_without_dummy"

# Concatenate full path to the folder
folder_path <- file.path(directory, folder_name)
folder_path_2 <- file.path(directory, folder_name_2)

# Create the folder
dir.create(folder_path, recursive = TRUE)
dir.create(folder_path_2, recursive = TRUE)

cat("Folder created:", folder_path, "\n")
cat("Folder created:", folder_path_2, "\n")


#*****************************___Define_directory__****************************#

# P-değerleri için yıldız işaretleme fonksiyonu
add_stars <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else if (p_value < 0.1) {
    return(".")
  } else {
    return("")
  }
}

#*****************************___Define_directory__****************************#

# Önce dummy_2019 adında yeni bir sütun oluşturun ve tüm değerleri 0 olarak ayarlayın
quarterly_xts_diff$dummy_2018Q3 <- 0
quarterly_xts_diff$dummy_2018Q4 <- 0
#quarterly_xts_diff$dummy_2021Q4 <- 0
quarterly_xts_diff$dummy_2022Q4 <- 0
#quarterly_xts_diff$dummy_2023Q2 <- 0  # Bunu uygulamıyoruz çünkü ikili kur rejimi vardı


# Ardından 50. ve 51. satırların dummy_2019 sütununu 1 olarak ayarlayın
quarterly_xts_diff[50, "dummy_2018Q3"] <- 1
quarterly_xts_diff[51, "dummy_2018Q4"] <- 1
#quarterly_xts_diff[63, "dummy_2021Q4"] <- 1
quarterly_xts_diff[67, "dummy_2022Q4"] <- 1
#quarterly_xts_diff[69, "dummy_2023Q2"] <- 1

dummy_2018Q3 <- quarterly_xts_diff$dummy_2018Q3
dummy_2018Q4 <- quarterly_xts_diff$dummy_2018Q4
dummy_2022Q4 <- quarterly_xts_diff$dummy_2022Q4

colnames(quarterly_xts_diff)


#*****************************___Define_directory__****************************#

# Define your filtered datasets
filtered_datasets <- list(
  "2015-12-31" = quarterly_xts_diff["::2015-12-31"],
  "2016-12-31" = quarterly_xts_diff["::2016-12-31"],
  "2017-12-31" = quarterly_xts_diff["::2017-12-31"],
  "2018-06-30" = quarterly_xts_diff["::2018-06-30"],
  "2019-12-31" = quarterly_xts_diff["::2019-12-31"],
  "2021-12-31" = quarterly_xts_diff["::2021-12-31"],
  "2023-09-31" = quarterly_xts_diff["::2023-09-31"]
)

#*****************************___Define_directory__****************************#

# Loop over filtered datasets
for (date_filter in names(filtered_datasets)) {
  # Select the dataset for the current date filter
  x <- filtered_datasets[[date_filter]]
  
  # Define your regression models
  models <- list()
  count <- 1
  
  for (num_vars in 1:5) {  
    combinations <- combn(c("log_CCI", "log_CBRT_Net_reserves_excluding_swaps", "log_M3", "core_rir", "sd_CR"), num_vars)
    for (i in 1:ncol(combinations)) {
      selected_vars <- c("log_sa_core_CPI", "lag_log_sa_core_CPI_1", "log_USD_TRY", "lag_log_USD_TRY_1", "lag_log_USD_TRY_2", "lag_log_USD_TRY_3", "lag_log_USD_TRY_4", "log_IPI", combinations[,i])
      
      # Add dummy variables based on date_filter
      if (date_filter == "2019-12-31") {
        selected_vars <- c(selected_vars, "dummy_2018Q3", "dummy_2018Q4")
      } else if (date_filter == "2021-12-31") {
        selected_vars <- c(selected_vars, "dummy_2018Q3", "dummy_2018Q4")
      } else if (date_filter == "2023-09-31") {
        selected_vars <- c(selected_vars, "dummy_2018Q3", "dummy_2018Q4", "dummy_2022Q4") #"dummy_2023Q2"
      }
      
      formula <- paste("log_sa_core_CPI ~ 1 + lag_log_sa_core_CPI_1 + 
                        log_USD_TRY + lag_log_USD_TRY_1 + lag_log_USD_TRY_2 + lag_log_USD_TRY_3 + lag_log_USD_TRY_4 +
                        log_IPI + ",
                       paste(selected_vars[-1], collapse = " + "),
                       sep = " ")
      model_name <- paste("model", count, sep = "")
      models[[model_name]] <- lm(formula, data = x)
      count <- count + 1
    }
  }
  
  
  
  # Store the model summaries
  model_summaries <- lapply(models, summary)
  
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  # Add a new Excel worksheet for each model summary
  for (model_name in names(model_summaries)) {
    addWorksheet(wb, sheetName = model_name)
    model_summary <- model_summaries[[model_name]]
    
    # Modify the coefficients table to include significance stars
    coefficients_df <- as.data.frame(model_summary$coefficients)
    coefficients_df$Significance <- sapply(coefficients_df$`Pr(>|t|)`, add_stars)
    
    # Write the modified coefficients table to the Excel worksheet
    writeData(wb, sheet = model_name, x = coefficients_df, rowNames = TRUE, colNames = TRUE)
    
    # Write R-squared and adjusted R-squared values to the Excel worksheet
    writeData(wb, sheet = model_name, x = "R-squared:", startCol = 8)
    writeData(wb, sheet = model_name, x = model_summary$r.squared, startCol = 9)
    writeData(wb, sheet = model_name, x = "Adjusted R-squared:", startCol = 8, startRow = 2)
    writeData(wb, sheet = model_name, x = model_summary$adj.r.squared, startCol = 9, startRow = 2)
    
    # Calculate Short_ERPT and Long_RPT and write to Excel
    coefficients <- coef(model_summary)
    Short_ERPT <- coefficients[3] + 
      coefficients[4] + 
      coefficients[5] +
      coefficients[6] +
      coefficients[7]
    
    Long_RPT <- Short_ERPT / (1 - coefficients[2])
    
    # Write calculated values to Excel
    writeData(wb, sheet = model_name, x = "Short_ERPT:", startRow = 21)
    writeData(wb, sheet = model_name, x = Short_ERPT, startCol = 2, startRow = 21)
    writeData(wb, sheet = model_name, x = "Long_RPT:", startRow = 22)
    writeData(wb, sheet = model_name, x = Long_RPT, startCol = 2, startRow = 22)
  }
  
  # Save the Excel workbook to the specified directory and filename
  output_path <- paste0(folder_path, "/model_summary_", date_filter, ".xlsx")
  saveWorkbook(wb, file = output_path, overwrite = TRUE)
}
