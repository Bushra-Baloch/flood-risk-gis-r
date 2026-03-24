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
ggplot(rainfall_data, aes(x = DATE, y = RAINFALL))source("C:/Users/HP/OneDrive/Desktop/github Repo/flood-risk-gis-r/scripts/rainfall_analysis.R", echo = TRUE)
 +
  geom_line(color = "blue") +
  labs(title = "Daily Rainfall Trend (Lahore 2015–2024)",
       x = "Date",
       y = "Rainfall (mm)") +
  theme_minimal()