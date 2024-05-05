
# 1.Step

# Before run the 0_Extract_data:
# Create and account from "https://evds2.tcmb.gov.tr/index.php?/evds/login"
# Then find yout API Key
# You can see the API Key with these steps:  
# Sign in  
# Click your username  
# Select Profile menu  
# Select API Key option 

# Then,
# Assign your individual API as:
# myCBRTKey <- "**********"
# The API is neccesary to extract data from the EVDS data base

# Extracting variable data and manipulating data are expained in detail in the code 
# Now, the 0_Extract_data is ready to extract data.
# Run the  0_Extract_data.

# 2.Step

# Run the 01_Log_Manupilation_of_Variables to the log transformation 

# 3.Step

# First of all update Target directory in the Define_directory. "79th row"

# Then Run the 02_1_Philips_equation_with_dummy 

# The script generates Excel files containing detailed 31 models summaries for each regression analysis, 
  stored in the specified directory under the folder named "Model_output_dummy". 
  Each Excel file corresponds to a specific date filter applied to the dataset.

# 4.Step

# Run the 02_2_Philips_equation_without_dummy

# The script generates Excel files containing detailed 31 models summaries for each regression analysis, 
  stored in the specified directory under the folder named "Model_output_without_dummy". 
  Each Excel file corresponds to a specific date filter applied to the dataset.

# 5.Step

# Run the 03_1_Stationary_test
# Run the 03_2_VAR_Model

# 6.Step

# 04_1_Reduced_VAR_Model
# Check graphs

# 04_2_std_Reduced_VAR_Model
# Check graphs

# 04_3_Impulse_response
# Check the graph

# 04_4_Robutsness_check
# Check the graphs

# 7.Step
# 05_Forecast
# Check the graphs


