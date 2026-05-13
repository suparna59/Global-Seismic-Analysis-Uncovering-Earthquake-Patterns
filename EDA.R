# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Read the dataset
earthquakes <- read_csv("Global_Seismic_2023.csv")

# Check structure of the dataset
str(earthquakes)

# Convert 'time' column to datetime format
earthquakes$time <- ymd_hms(earthquakes$time)

# Check for missing values
colSums(is.na(earthquakes))

# Summary statistics
summary(earthquakes$mag)
summary(earthquakes$depth)

# Histogram of magnitudes
hist(earthquakes$mag, main = "Distribution of Earthquake Magnitudes", xlab = "Magnitude")

# Scatter plot of depth vs. magnitude
plot(earthquakes$depth, earthquakes$mag, main = "Depth vs. Magnitude", xlab = "Depth", ylab = "Magnitude")

# Create boxplot for each type of event
ggplot(earthquakes, aes(x = type, y = mag)) +
  geom_boxplot() +
  labs(title = "Magnitude Distribution Across Different Types of Events",
       x = "Type of Event", y = "Magnitude") +
  theme_minimal()

# Extract year and month from the time column
earthquakes <- earthquakes %>%
  mutate(year_month = format(time, "%Y-%m"))

# Convert the time column to a date format
earthquakes$time <- as.POSIXct(earthquakes$time)

# Group the data by year and month, and calculate the mean magnitude
monthly_magnitudes <- earthquakes %>%
  group_by(year_month) %>%
  summarize(mean_magnitude = mean(mag, na.rm = TRUE))

# Convert the data to a time series object
monthly_magnitudes_ts <- ts(monthly_magnitudes$mean_magnitude, start = c(year(min(earthquakes$time)), month(min(earthquakes$time))), frequency = 12)

# Plot the time series of monthly mean magnitudes
plot(monthly_magnitudes_ts,
     main = "Monthly Mean Earthquake Magnitudes",
     xlab = "Time",
     ylab = "Mean Magnitude",
     xaxt = "n") 

# Add custom x-axis labels for years
axis(1, at = seq(1, length(monthly_magnitudes_ts), by = 12), labels = unique(substr(monthly_magnitudes$year_month, 1, 4)))

# Calculate the correlation coefficient between rms and nst
correlation_coefficient <- cor(earthquakes$rms, earthquakes$nst)

# Print the correlation coefficient
print(paste("Correlation Coefficient between rms and nst:", correlation_coefficient))

# Select relevant variables
relevant_vars <- c("rms", "mag", "depth", "gap")

# Calculate the correlation coefficients
correlation_results <- cor(earthquakes[relevant_vars])

# Print correlation coefficients
print(correlation_results)
