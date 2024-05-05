
#**********************__02_2_Philips_equation_without_dummy__******************#

# Loop over filtered datasets
for (date_filter in names(filtered_datasets)) {
  # Select the dataset for the current date filter
  x <- filtered_datasets[[date_filter]]
  
  # Define your regression models
  models <- list()
  count <- 1
  
  for (num_vars in 1:5) {  # Değişken sayısına göre döngü oluşturuluyor
    combinations <- combn(c("log_CCI", "log_CBRT_Net_reserves_excluding_swaps", "log_M3", "core_rir", "sd_CR"), num_vars)
    for (i in 1:ncol(combinations)) {
      selected_vars <- c("log_sa_core_CPI", "lag_log_sa_core_CPI_1", "log_USD_TRY", "lag_log_USD_TRY_1", "lag_log_USD_TRY_2", "lag_log_USD_TRY_3", "lag_log_USD_TRY_4", "log_IPI", combinations[,i])
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
  # Calculate Short_ERPT and Long_RPT and write to Excel
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
  output_path <- paste0(folder_path_2, "/model_summary_", date_filter, ".xlsx")
  saveWorkbook(wb, file = output_path, overwrite = TRUE)
}
