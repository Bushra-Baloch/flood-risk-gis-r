# Flood Risk Project - Step 3: Load Rainfall Data

# Install package (only first time)
install.packages("readr")

# Load library
library(readr)

# Load dataset
rainfall_data <- read_csv("data/raw/rainfall_lahore_2015_2024.csv")

# View first rows
head(rainfall_data)

# Check structure
str(rainfall_data)