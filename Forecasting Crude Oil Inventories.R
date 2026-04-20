

# ==============================================================================
# PROJECT: Forecasting U.S. Crude Oil Inventories
# AUTHOR: Atiq Ur Rehman
# ==============================================================================

# 1. Load the necessary library to read Excel files

library(readxl)
library(tidyverse)
library(lubridate)
library(tsibble)
library(psych)

library(ggplot2)
library(scales)

# 3. Read the data into a DataFrame
# 'sheet = 1' tells R to look at the first tab of your Excel file.
# 'col_names = TRUE' tells R that the first row contains your variable names (WTI, DXY, CPI, etc.)
df <- read_excel(
  "C:/Users/atiqur.rehman/Desktop/crude oil inventory.xlsx",
  sheet = 1,
  col_names = TRUE
)


# ==============================================================================
# INITIAL DATA INSPECTION
# ==============================================================================
df$Date <- as.Date(df$Date)


# ==============================================================================
# FORCE CLEANING (The Nuclear Option)
# ==============================================================================

print(paste("Rows before cleaning:", nrow(df)))

df <- df %>%
  # Step A: Ensure Date is a Date Object
  mutate(Date = as.Date(Date)) %>%
  
  # Step B: Force all other columns to be Numeric
  # This is crucial. If a cell has text like "null" or " ", this turns it into a real NA.
  mutate(across(-Date, as.numeric)) %>%
  
  # Step C: NOW drop the NAs
  drop_na()

# ==============================================================================
# VERIFICATION
# ==============================================================================

print(paste("Rows after cleaning:", nrow(df)))

# Verify no NAs remain
if(sum(is.na(df_clean)) == 0) {
  print("SUCCESS: All null values have been removed.")
} else {
  print("WARNING: There are still missing values.")
  print(colSums(is.na(df)))
}

# Check the final structure
str(df)
# ==============================================================================
# DESCRIPTIVE STATISTICS
# ==============================================================================

#  Base R Summary (Good for a quick console check)
print("--- Basic Summary ---")
summary(df)


print("--- Detailed Academic Descriptive Statistics ---")
# We exclude the 'Date' column since we only want stats on the numeric variables
desc_stats <- describe(df %>% select(-Date))
print(desc_stats)



# 3. Define the start and end dates for your 3-Split Strategy
split1_start <- as.Date("1994-01-01")
split1_end   <- as.Date("2003-12-31")

split2_start <- as.Date("2004-01-01")
split2_end   <- as.Date("2015-12-31")

split3_start <- as.Date("2016-01-01")
split3_end   <- as.Date("2025-12-31")

# 4. Create the Regime-Aware Time Series Plot
regime_plot <- ggplot(data = df, aes(x = Date, y = Target)) +
  # Add the shaded regions FIRST so they sit behind the line
  annotate("rect", xmin = split1_start, xmax = split1_end, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "green") +
  annotate("rect", xmin = split2_start, xmax = split2_end, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "red") +
  annotate("rect", xmin = split3_start, xmax = split3_end, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "blue") +
  
  # Add the actual data line
  geom_line(color = "black", linewidth = 0.8) +
  
  # Add text labels for the eras at the top of the chart
  annotate("text", x = as.Date("1999-01-01"), y = max(df$Target, na.rm=TRUE) * 0.98, 
           label = "Stable Era", fontface = "bold") +
  annotate("text", x = as.Date("2010-01-01"), y = max(df$Target, na.rm=TRUE) * 0.98, 
           label = "Crisis Era", fontface = "bold") +
  annotate("text", x = as.Date("2021-01-01"), y = max(df$Target, na.rm=TRUE) * 0.98, 
           label = "Modern Era", fontface = "bold") +
  
  # Formatting
  scale_y_continuous(labels = comma) + # 'comma' works now because library(scales) is loaded
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
  labs(
    title = "Structural Breaks in U.S. Crude Oil Inventories (1994-2025)",
    x = "Year",
    y = "Inventories (Thousand Barrels)"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

# 5. Display the plot
print(regime_plot)

# 1. Install 'corrplot' if you don't have it (uncomment to run once)
# install.packages("corrplot")

# 2. Load the library
library(corrplot)
library(dplyr) # for data manipulation

# 3. Calculate the Correlation Matrix
# We MUST drop the 'Date' column because correlation only works on numeric data.
# use = "complete.obs" ensures it handles any lingering missing values gracefully.
numeric_df <- df %>% select(-Date)
cor_matrix <- cor(numeric_df, use = "complete.obs", method = "pearson")

# 4. Generate the Academic Heatmap
# This creates a clean, upper-triangle heatmap with correlation coefficients
corrplot(cor_matrix, 
         method = "color",       # Fills the squares with color based on correlation
         type = "upper",         # Only shows the top half (avoids duplicate data)
         tl.col = "black",       # Makes text labels black
         tl.cex = 0.8,           # Adjusts text label size
         tl.srt = 45,            # Rotates text labels 45 degrees for readability
         addCoef.col = "black",  # Adds the actual numbers inside the squares
         number.cex = 0.6,       # Adjusts the size of the numbers
         col = colorRampPalette(c("#BB4444", "#FFFFFF", "#4477AA"))(200), # Red-White-Blue color scale
         title = "Correlation Matrix of Predictor Variables",
         mar = c(0,0,1,0))       # Fixes margin cutting off the title

         # 1. Prepare the data (removing the Date column)
plot_data <- df %>% select(-Date)
vars <- names(plot_data)

# ==============================================================================
# WINDOW 1: Variables 1 to 8
# ==============================================================================
# Set up a 2x4 grid (2 rows, 4 columns)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1)) 

for (i in 1:8) {
  boxplot(plot_data[[vars[i]]], 
          main = vars[i], 
          col = "lightblue", 
          border = "darkblue",
          horizontal = FALSE)
}

# 1. Define concise labels for all 15 variables to avoid overlapping
# Make sure the order matches your 'vars' vector (CPI, GPR, ..., WTI)
short_labels <- c(
  "CPI", "GPR", "GPR Act", "GPR Threat", "Ind. Prod.", 
  "Net Imports", "News Sentiment", "Policy Uncert.", 
  "Crude Prod.", "Fin. Stress", "Ending Stocks", 
  "Refinery Util.", "Unemployment", "USD Index", "WTI Price"
)

# 2. Open the plotting window
windows() 

# 3. Set up a 2x4 grid for the second set (variables 9 to 15)
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1)) 

for (i in 9:15) {
  boxplot(plot_data[[vars[i]]], 
          main = short_labels[i], # Uses the short label instead of the long name
          col = "lightgreen", 
          border = "darkgreen",
          horizontal = FALSE,
          las = 1)
}

# 4. Reset to default 1x1
par(mfrow = c(1, 1))