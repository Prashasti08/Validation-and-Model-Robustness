# Loading the 'tidyverse' package, which includes tools for loading, cleaning, and plotting data
library(tidyverse)

# Reading the original dataset CSV file into a data frame named 'data_1087'
data_1087 <- read_csv("RMotivation_for_analysis.csv")

# Checking no. of rows and columns in this dataset 
dim(data_1087)

# Checking the number of missing values this dataset 
data_1087 %>%
  summarise(Missing_npkgs = sum(is.na(npkgs)),
            Missing_mintrinsic = sum(is.na(mintrinsic)))
# 192 missing values found in npkgs and 235 in mintrinsic

# Creating a new, clean data frame (data_841) by removing rows with missing values
# in the two specified columns npkgs and mintrinsic
data_841 <- data_1087 %>%
  drop_na(npkgs, mintrinsic)


# Checking the final number of rows
nrow(data_841)

# Check for missing values again
data_841 %>%
  summarise(
    Missing_npkgs = sum(is.na(npkgs)),
    Missing_mintrinsic = sum(is.na(mintrinsic)))

# Descriptive statistics for the clean dataset with 841 cases

# Descriptive Statistics for Intrinsic Motivation (mintrinsic)
mean(data_841$mintrinsic)   # Mean
sd(data_841$mintrinsic)    # Standard Deviation
min(data_841$mintrinsic)    # Minimum value
max(data_841$mintrinsic)    # Maximum value

# Descriptive Statistics for Total Packages Contributed (npkgs)
mean(data_841$npkgs)   # Mean
sd(data_841$npkgs)     # Standard Deviation
min(data_841$npkgs)    # Minimum value
max(data_841$npkgs)    # Maximum value

# Scatterplot Creation
plot(data_841$mintrinsic, data_841$npkgs,
     main = "Intrinsic Motivation and Package Contributions (N=841)",
     xlab = "Intrinsic Motivation Score (mintrinsic)",
     ylab = "Total Packages Contributed (npkgs)",
     pch = 19, # Plotting symbol
     col = "black" # Point color
)

# Adding the Regression Line to the plot
abline(model_slr, col = "red", lwd = 2)

#Simple Linear Regression

# Running the Simple Linear Regression (SLR) model
model_slr <- lm(npkgs ~ mintrinsic, data = data_841)

# Displaying the full statistical summary
summary(model_slr)


# Statistical Analysis: Check R-squared
# The "Multiple R-squared" value from the summary(model_slr) output was used here

# Running Diagnostic Plots to check for technical flaws: The 4 plots checked by running the below code 4 times
plot(model_slr)

