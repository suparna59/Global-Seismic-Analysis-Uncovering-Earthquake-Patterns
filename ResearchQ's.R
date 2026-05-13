# Research Question - 1
# Load required libraries
library(randomForest)
library(caret) 
library(tidyverse)
library(rpart)
library(rpart.plot)

# Read the earthquake data
earthquakes <- read.csv("Global_Seismic_2023.csv")

# Convert the 'type' column to a factor
earthquakes$type <- as.factor(earthquakes$type)

# Select relevant features
features <- earthquakes %>%
  select(latitude, longitude, depth, mag, magType, nst, gap, dmin, rms)

# Split data into training and test sets
set.seed(123)
train_index <- createDataPartition(earthquakes$type, p = 0.8, list = FALSE)
train_data <- earthquakes[train_index, ]
test_data <- earthquakes[-train_index, ]

# Random Forest Model
rf_model <- randomForest(type ~ ., data = train_data[, c("type", names(features))])
rf_predictions <- predict(rf_model, newdata = test_data[, names(features)])
rf_accuracy <- mean(rf_predictions == test_data$type)
print(paste("Random Forest Accuracy:", rf_accuracy))

# Decision Tree Model
dt_model <- rpart(type ~ ., data = train_data[, c("type", names(features))])
dt_predictions <- predict(dt_model, newdata = test_data[, names(features)], type = "class")
dt_accuracy <- mean(dt_predictions == test_data$type)
print(paste("Decision Tree Accuracy:", dt_accuracy))

# Visualize the decision tree
rpart.plot(dt_model, main = "Decision Tree for Earthquake Type Prediction", extra = 0)



# Research Question - 2
# Fit a multiple linear regression model
lm_model <- lm(mag ~ latitude + longitude + depth + rms, data = earthquakes)

# Summary of the regression model
summary(lm_model)
# Diagnostic plots
par(mfrow = c(2, 2))  
plot(lm_model)



#Research Question - 3

# Filter for only earthquake events
earthquakes_only <- earthquakes %>%
  filter(type == "earthquake")

# Select relevant variables for clustering
clustering_vars <- earthquakes %>%
  select(latitude, longitude, depth, mag)

# Check for missing values
summary(clustering_vars)

# Apply k-means clustering to the selected variables
set.seed(123) # Set a seed for reproducibility
K <- kmeans(clustering_vars, centers = 20, nstart = 25)

# Print the cluster assignments for the first 5 earthquakes
head(K$cluster, n = 5)

# Print the number of earthquakes in each cluster
K$size

# Print the cluster centers (mean values of each variable for each cluster)
K$centers

# Create a data frame with the original data and the assigned cluster
df <- data.frame(earthquakes, cluster = K$cluster)

# Add the median magnitude for each cluster
df <- group_by(df, cluster) %>%
  mutate(medMag = median(mag)) %>%
  ungroup()

# Visualize the clusters on a map
ggplot(df) +
  geom_point(aes(x = longitude, y = latitude, color = medMag)) +
  theme_minimal()

# Write data frame "df" to a csv file to use in Tableau
write.csv(df, "NewGlobalSeismic2023.csv")




