
#***************************___04_3_Impulse_responses__************************#

# Create a matrix of variables
#var_model_a <- cbind(v_NR, v_CCI, v_IPI, v_CR, v_sa_core_CPI)
# Assign column names
#colnames(var_model_a) <- c("v_NR", "v_CCI", "v_IPI", "v_CR", "v_sa_core_CPI")

# Create a list of filtered datasets for different dates
filtered_var_model <- list(
  "2018-12-31" = var_model_a["::2018-12-31"],
  "2019-12-31" = var_model_a["::2019-12-31"],
  "2020-12-31" = var_model_a["::2020-12-31"],
  "2021-12-31" = var_model_a["::2021-12-31"],
  "2022-12-31" = var_model_a["::2022-12-31"],
  "2023-12-31" = var_model_a["::2023-12-31"]
)

# Plot Cumulative Impulse Response Functions
plot(NULL, type = "n", xlim = c(0, 36), ylim = c(0, 0.75), 
     xlab = "", ylab = "", 
     main = "Cumulative Impulse Response Functions", xaxt = "n", yaxt = "n",
     cex.main = 2)

# Add horizontal gridlines
abline(h = seq(0, 0.75, by = 0.05), col = "lightgray", lty = "dotted")
# Add vertical lines on x-axis
abline(v = c(0, 3, 6), col = "black", lty = "dashed")

# Set labels for y-axis
axis(2, at = seq(0, 0.75, by = 0.05))
# Add a custom x-axis
axis(1, at = seq(0, 36, by = 2), labels = seq(0, 36, by = 2))

# Iterate over each VAR model in the list
for (date_filter in names(filtered_var_model)) {
  # Get the filtered VAR model based on the selected date
  var_model_results <- VAR(filtered_var_model[[date_filter]], p = 5, type = "const")
  
  # Compute Impulse Response Function
  IR <- irf(var_model_results,
            impulse = "v_CR",
            response = "v_sa_core_CPI",
            n.ahead = 36,
            boot = TRUE,
            ortho = FALSE,
            runs = 100)
  
  # Compute Cumulative Impulse Response
  cumulative_irf <- cumsum(IR$irf[[1]])
  
  # Define time range (for illustration purpose)
  time_range <- 0:(length(cumulative_irf) - 1)
  
  # Plot Cumulative Impulse Response
  lines(time_range, cumulative_irf, type = "l", lwd = 2,
        col = rainbow(length(filtered_var_model))[which(names(filtered_var_model) == date_filter)])
}

# Add legend
legend("topright", legend = names(filtered_var_model), 
       col = rainbow(length(filtered_var_model)), lty = 1, cex = 0.8)


