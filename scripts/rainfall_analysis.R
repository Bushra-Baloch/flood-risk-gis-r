# ================================
# Flood Risk Assessment Project
# ================================

# Load Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# -------------------------------
# 1. Load Data
# -------------------------------
rainfall_data <- read_csv("data/raw/rainfall_lahore_2015_2024.csv", skip = 10)

# -------------------------------
# 2. Clean Data
# -------------------------------

# Replace missing values
rainfall_data$PRECTOTCORR[rainfall_data$PRECTOTCORR == -999] <- NA

# Keep useful columns
rainfall_data <- rainfall_data[, c("YEAR", "DOY", "IMERG_PRECTOT")]
colnames(rainfall_data)[3] <- "RAINFALL"

# Create DATE
rainfall_data$DATE <- as.Date(rainfall_data$DOY - 1,
                              origin = paste0(rainfall_data$YEAR, "-01-01"))

# -------------------------------
# 3. Daily Rainfall Plot
# -------------------------------
ggplot(rainfall_data, aes(x = DATE, y = RAINFALL)) +
  geom_line(color = "blue") +
  labs(title = "Daily Rainfall Trend (2015–2024)",
       x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

# -------------------------------
# 4. Monthly Analysis
# -------------------------------
monthly_rainfall <- rainfall_data %>%
  mutate(MONTH = floor_date(DATE, "month")) %>%
  group_by(MONTH) %>%
  summarise(TOTAL_RAINFALL = sum(RAINFALL, na.rm = TRUE))

ggplot(monthly_rainfall, aes(x = MONTH, y = TOTAL_RAINFALL)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Rainfall Trend",
       x = "Month", y = "Total Rainfall (mm)") +
  theme_minimal()

# -------------------------------
# 5. Extreme Rainfall
# -------------------------------
extreme_rainfall <- rainfall_data %>%
  filter(RAINFALL > 50)

print(nrow(extreme_rainfall))

# -------------------------------
# 6. Yearly Extreme Analysis
# -------------------------------
yearly_extreme <- extreme_rainfall %>%
  mutate(YEAR = year(DATE)) %>%
  group_by(YEAR) %>%
  summarise(EXTREME_DAYS = n())

ggplot(yearly_extreme, aes(x = YEAR, y = EXTREME_DAYS)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Yearly Extreme Rainfall Days",
       x = "Year", y = "Days") +
  theme_minimal()

# -------------------------------
# 7. Flood Risk Classification
# -------------------------------
rainfall_data <- rainfall_data %>%
  mutate(RISK_LEVEL = case_when(
    RAINFALL <= 20 ~ "Low",
    RAINFALL > 20 & RAINFALL <= 50 ~ "Medium",
    RAINFALL > 50 ~ "High"
  ))

table(rainfall_data$RISK_LEVEL)

ggplot(rainfall_data, aes(x = RISK_LEVEL, fill = RISK_LEVEL)) +
  geom_bar() +
  labs(title = "Flood Risk Distribution",
       x = "Risk Level", y = "Days") +
  theme_minimal()

