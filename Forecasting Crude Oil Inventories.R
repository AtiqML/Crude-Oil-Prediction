# ==============================================================================
# PROJECT: The Pulse of Cushing - Predicting U.S. Crude Oil Inventories
# AUTHOR: Atiq Ur Rehman
# ==============================================================================

# 1. Load the necessary library to read Excel files

library(readxl)


# 2. Define the file path
# Replace 'oil_data.xlsx' with the actual name of your file.
# Make sure the file is in your specific "Working Directory" folder.
file_path <- "oil_data.xlsx" 

# 3. Read the data into a DataFrame
# 'sheet = 1' tells R to look at the first tab of your Excel file.
# 'col_names = TRUE' tells R that the first row contains your variable names (WTI, DXY, CPI, etc.)
oil_data <- read_excel(file_path, sheet = 1, col_names = TRUE)

# ==============================================================================
# INITIAL DATA INSPECTION
# ==============================================================================

# 4. View the first 6 rows 
# This helps you verify that the data loaded correctly and headers are right.
print("First 6 rows of the dataset:")
head(oil_data)

# 5. Check the data structure
# This is critical. It tells you if R sees your numbers as "Numeric" (good) 
# or "Character" (bad - text). It also checks if your Date column is recognized as a Date.
print("Structure of the dataset:")
str(oil_data)

# 6. Check for Missing Values (NA)
# Since you have 30 years of data, checking for empty cells is important.
print("Count of missing values in each column:")
colSums(is.na(oil_data))