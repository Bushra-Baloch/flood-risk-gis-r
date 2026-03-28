# Flood Risk Project - Step 3: Load Rainfall Data


# Load library
library(readr)

# Load dataset
rainfall_data <- read_csv("data/raw/rainfall_lahore_2015_2024.csv", skip = 10)

# View first rows
head(rainfall_data)

# Check structure
str(rainfall_data)


# Check for missing values in dataset
sum(is.na(rainfall_data))

# Check missing values per column
colSums(is.na(rainfall_data))

head(rainfall_data)
str(rainfall_data)

# Replace -999 with NA (real missing values)
rainfall_data$PRECTOTCORR[rainfall_data$PRECTOTCORR == -999] <- NA

# Check again for missing values
sum(is.na(rainfall_data))
colSums(is.na(rainfall_data))

# Remove useless column
rainfall_data <- rainfall_data[, c("YEAR", "DOY", "IMERG_PRECTOT")]

# Rename column for simplicity
colnames(rainfall_data)[3] <- "RAINFALL"

# Create proper DATE column from YEAR and DOY
rainfall_data$DATE <- as.Date(rainfall_data$DOY - 1, 
                              origin = paste0(rainfall_data$YEAR, "-01-01"))

# View updated data
head(rainfall_data)

# Check structure
str(rainfall_data)

# Check structure again
str(rainfall_data)

# Install ggplot2 (only if not installed before)
install.packages("ggplot2")

# Load library
library(ggplot2)

# Create rainfall trend plot
ggplot(rainfall_data, aes(x = DATE, y = RAINFALL)) +
  geom_line(color = "blue") +
  labs(title = "Daily Rainfall Trend (Lahore 2015–2024)",
       x = "Date",
       y = "Rainfall (mm)") +
  theme_minimal()

# Load dplyr
library(dplyr)

# Create monthly rainfall totals
monthly_rainfall <- rainfall_data %>%
  mutate(MONTH = format(DATE, "%Y-%m")) %>%
  group_by(MONTH) %>%
  summarise(TOTAL_RAINFALL = sum(RAINFALL, na.rm = TRUE))

# View data
head(monthly_rainfall)

# Load dplyr
library(dplyr)

# Create Year-Month column
rainfall_data <- rainfall_data %>%
  mutate(MONTH = format(DATE, "%Y-%m"))

# Monthly rainfall total
monthly_rainfall <- rainfall_data %>%
  group_by(MONTH) %>%
  summarise(TOTAL_RAINFALL = sum(RAINFALL, na.rm = TRUE))

# View result
head(monthly_rainfall)
library(ggplot2)

ggplot(monthly_rainfall, aes(x = as.Date(paste0(MONTH, "-01")), y = TOTAL_RAINFALL)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Rainfall Trend (2015–2024)",
       x = "Month",
       y = "Total Rainfall (mm)") +
  theme_minimal()
source("C:/Users/HP/OneDrive/Desktop/flood-risk-gis-r/scripts/rainfall_analysis.R", echo = TRUE)

extreme_rainfall <- rainfall_data %>%
  filter(RAINFALL > 50)

head(extreme_rainfall)
# Step: Extreme Rainfall Detection

library(dplyr)

# Filter heavy rainfall days (> 50 mm)
extreme_rainfall <- rainfall_data %>%
  filter(RAINFALL > 50)

# View result
head(extreme_rainfall)

# Count total extreme days
nrow(extreme_rainfall)

library(dplyr)
library(lubridate)

# Convert DATE to proper date format
rainfall_data$DATE <- as.Date(rainfall_data$DATE)

# Create month column
rainfall_data <- rainfall_data %>%
  mutate(MONTH = floor_date(DATE, "month"))

# Monthly rainfall sum
monthly_rainfall <- rainfall_data %>%
  group_by(MONTH) %>%
  summarise(TOTAL_RAINFALL = sum(RAINFALL, na.rm = TRUE))

# View result
head(monthly_rainfall)




# library(ggplot2)

ggplot(monthly_rainfall, aes(x = MONTH, y = TOTAL_RAINFALL)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Rainfall Trend",
       x = "Month",
       y = "Total Rainfall (mm)") +
  theme_minimal()

library(dplyr)
library(lubridate)

# Ensure DATE is correct
rainfall_data$DATE <- as.Date(rainfall_data$DATE)

# Extract year
rainfall_data <- rainfall_data %>%
  mutate(YEAR = year(DATE))

# Filter extreme rainfall
extreme_rainfall <- rainfall_data %>%
  filter(RAINFALL > 50)

# Count extreme days per year
yearly_extreme <- extreme_rainfall %>%
  group_by(YEAR) %>%
  summarise(EXTREME_DAYS = n())

# View result

library(ggplot2)

ggplot(yearly_extreme, aes(x = YEAR, y = EXTREME_DAYS)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Yearly Extreme Rainfall Days",
       x = "Year",
       y = "Number of Extreme Days") +
  theme_minimal()


library(dplyr)

rainfall_data <- rainfall_data %>%
  mutate(RISK_LEVEL = case_when(
    RAINFALL <= 20 ~ "Low",
    RAINFALL > 20 & RAINFALL <= 50 ~ "Medium",
    RAINFALL > 50 ~ "High"
  ))
print(yearly_extreme)
