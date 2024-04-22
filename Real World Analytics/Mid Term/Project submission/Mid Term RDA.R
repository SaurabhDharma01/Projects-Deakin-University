# get the working directory
getwd()

# Set the working directory
setwd("C:/Users/Arun/Downloads/Deakin/Real World Analytics/Mid Term/Data")

# Read the data file with tab-separated values
data <- read.table("ENB_2023.txt", header = FALSE)

# Display the data
data

# Assign meaningful column names
colnames(data) <- c("Temperature kitchen Celsius", " Humidity kitchen percentage", "Temperature outside Celsius", "Humidity outside percentage", "Visibility km", "Appliances, energy use, in Wh")

# Read the data file into a matrix
the.data <- as.matrix(read.table("ENB_2023.txt", header = FALSE))

# Display the matrix
the.data


# Generate a subset with 340 rows and 6 columns
my.data <- the.data[sample(1:nrow(the.data), 340), c(1:6)]

# Display the first few rows of the subset
head(my.data)

# Extract only the "Y" variable (column)
Y_variable <- my.data[, 6]

# Display the first few values of the "Y" variable
head(Y_variable)



# The following tasks are based on the 340 sample data.
# (iv) Use scatter plots and histograms to understand the relationship between each of the variables X1, X2, X3, X4, 
# X5, and your variable of interest Y. (You should build 5 scatter plots and 6 histograms).




# Set up a 2x3 grid for plots
par(mfrow = c(2, 3))

# Scatter plot for X1 vs Y with a blue color
plot(my.data[, 1], Y_variable, main = "V1 vs Y", xlab = "V1", ylab = "Y", col = "lightblue")
abline(lm(Y_variable ~ my.data[, 1]), col = "blue")  # Add regression line

# Similar plots for X2 to X5...
# Scatter plot for X2 vs Y with a green color
plot(my.data[, 2], Y_variable, main = "V2 vs Y", xlab = "V2", ylab = "Y", col = "lightgreen")
abline(lm(Y_variable ~ my.data[, 2]), col = "green")

# Scatter plot for X3 vs Y with a red color
plot(my.data[, 3], Y_variable, main = "V3 vs Y", xlab = "V3", ylab = "Y", col = "red")
abline(lm(Y_variable ~ my.data[, 3]), col = "darkred")

# Scatter plot for X4 vs Y with a purple color
plot(my.data[, 4], Y_variable, main = "V4 vs Y", xlab = "V4", ylab = "Y", col = "purple1")
abline(lm(Y_variable ~ my.data[, 4]), col = "purple")

# Scatter plot for X5 vs Y with an orange color
plot(my.data[, 5], Y_variable, main = "V5 vs Y", xlab = "V5", ylab = "Y", col = "orange")
abline(lm(Y_variable ~ my.data[, 5]), col = "darkorange")

# Add an overall title
main_title <- "Scatter Plots of Independent Variables (V1 to V5) vs Dependent Variable (Y)"
main_title <- paste(main_title, "\n with Regression Lines", sep="")
mtext(main_title, line = 0.5, outer = TRUE, cex = 1.2)




# Histogram

# Reset the layout to 2x3 for histograms
par(mfrow = c(2, 3))

# Histogram for X1 with a blue color
hist(my.data[, 1], main = "Histogram for V1", xlab = "V1", col = "lightblue", breaks = 20)

# Histogram for X2 with a green color
hist(my.data[, 2], main = "Histogram for V2", xlab = "V2", col = "lightgreen", breaks = 20)

# Histogram for X3 with a red color
hist(my.data[, 3], main = "Histogram for V3", xlab = "V3", col = "red", breaks = 20)

# Histogram for X4 with a purple color
hist(my.data[, 4], main = "Histogram for V4", xlab = "V4", col = "purple1", breaks = 20)

# Histogram for X5 with an orange color
hist(my.data[, 5], main = "Histogram for V5", xlab = "V5", col = "orange", breaks = 20)

# Histogram for Y with a pink color
hist(Y_variable, main = "Histogram for Y", xlab = "Y", col = "pink", breaks = 20)

# Add an overall title
main_title_hist <- "Histograms of Variables (X1 to X5) and Y"
mtext(main_title_hist, line = 0.5, outer = TRUE, cex = 1.2)




# Box Plots

# Set up a 2x3 grid for box plots
par(mfrow = c(2, 3))

# Box plot for X1
boxplot(my.data[, 1], main = "Boxplot for X1", col = "lightblue", border = "black", horizontal = TRUE)

# Box plot for X2
boxplot(my.data[, 2], main = "Boxplot for X2", col = "lightgreen", border = "black", horizontal = TRUE)

# Box plot for X3
boxplot(my.data[, 3], main = "Boxplot for X3", col = "red", border = "black", horizontal = TRUE)

# Box plot for X4
boxplot(my.data[, 4], main = "Boxplot for X4", col = "purple1", border = "black", horizontal = TRUE)

# Box plot for X5
boxplot(my.data[, 5], main = "Boxplot for X5", col = "orange", border = "black", horizontal = TRUE)

# Box plot for Y
boxplot(Y_variable, main = "Boxplot for Y", col = "pink", border = "black", horizontal = TRUE)

# Add an overall title
main_title_boxplot <- "Box Plots of Variables (X1 to X5) and Y"
mtext(main_title_boxplot, line = 0.5, outer = TRUE, cex = 1.2)

# Reset the layout
par(mfrow = c(1, 1))


## Shailesh
min_max_scaler <- function(x) {
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)  # na.rm will ignore NA values
  if (range == 0) {
    return(rep(0, length(x)))  # shall deal with situations when all elements are same
  } else {
    return((x - min(x, na.rm = TRUE)) / range)
  }
}


# Function to perform min-max scaling on a vector
min_max_scaling <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  scaled_values <- (x - min_val) / (max_val - min_val)
  return(scaled_values)
}

# Apply min-max scaling to each column of the data
scaled_data <- apply(my.data, 2, min_max_scal)

# Display the first few rows of the scaled data
head(scaled_data)






# Summary statistics for the original data
summary(my.data)

# Summary statistics for the scaled data
summary(scaled_data)

# Visualize histograms for the original and scaled data
par(mfrow = c(2, 2))
hist(my.data[, 1], main = "Original V1", xlab = "V1", col = "blue")
hist(scaled_data[, 1], main = "Scaled V1", xlab = "V1", col = "lightblue")
hist(my.data[, 2], main = "Original V2", xlab = "V2", col = "green")
hist(scaled_data[, 2], main = "Scaled V2", xlab = "V2", col = "lightgreen")
# Repeat for other variables...

# Reset the layout
par(mfrow = c(1, 1))

# Install and load the e1071 package if not already installed
#if (!requireNamespace("e1071", quietly = TRUE)) {
 # install.packages("e1071")
 #}
library(e1071)

# Compute skewness for each variable in the original data
original_skewness <- apply(my.data, 2, skewness)

# Compute skewness for each variable in the scaled data
scaled_skewness <- apply(scaled_data, 2, skewness)

# Display skewness for the original data
print("Skewness for Original Data:")
print(original_skewness)

# Display skewness for the scaled data
print("Skewness for Scaled Data:")
print(scaled_skewness)










# Converting scaled_data from matrix to dataframe
scaled_data_df <- as.data.frame(scaled_data)

head(scaled_data_df)

# Assuming scaled_data_df is your dataframe
# Assuming you have a linear regression model
model <- lm(V6 ~ V1 + V2 + V3 + V4 + V5, data = scaled_data_df)

# Predict V6 using the model
predictions <- predict(model, scaled_data_df)

# Calculate Mean Squared Error (MSE) for each variable
mse_values <- sapply(colnames(scaled_data_df[, -6]), function(variable) {
  mean((scaled_data_df[, variable] - predictions)^2)
})

# Display MSE values for each variable
mse_values


# Selecting variables V1, V2, V3, V5, and V6
your.data <- scaled_data_df[, c("V1", "V2", "V3", "V5", "V6")]

# Display the first few rows of the new data frame
head(your.data)


# Save selected_variables to a text file
write.table(your.data, "name-transformed.txt")


# T3

# Install lpSolve if not installed
# install.packages("lpSolve")

#Loading AggWaFt718 
source("AggWaFit718.R")

# Attempt to read the data again without header
your_data_matrix <- read.table("name-transformed.txt", header = FALSE, skip = 1)

# Check the structure of the data
str(your_data_matrix)

head(your_data_matrix)




# Load the AggWaFit718 package
source("AggWaFit718.R")

# Attempt to read the data again without header and excluding the first column
your_data_matrix <- read.table("name-transformed.txt", header = FALSE, skip = 1)[, -1]

# Check the structure of the data
str(your_data_matrix)
head(your_data_matrix)








# Load the AggWaFit718 package
source("AggWaFit718.R")

# Fit the data using the correct arguments
wam <- fit.QAM(your_data_matrix, "output_wam.txt", "stats_wam.txt", g = AM, g.inv = invAM)

# Check the result
print(wam)
summary(wam)
print(wam)
debug(fit.QAM)
fit.QAM(your_data_matrix, "output_wam.txt", "stats_wam.txt", g = AM, g.inv = invAM)
debugonce(fit.QAM)




wam <- fit.QAM(your_data_matrix, "output_wam.txt", "stats_wam.txt", g = AM, g.inv = invAM)

head(wam)

View(wam)


# (a) Weighted Arithmetic Mean (WAM):
waam <- fit.QAM(your_data_matrix, "output_wam.txt", "stats_wam.txt", AM, invAM)

# (b) Weighted Power Means (WPM) with p = 0.5:
Wpmp05 <- fit.QAM(your_data_matrix, "output_wpm_05.txt", "stats_wpm_05.txt", function(x) PM(x, p = 0.5), function(x) invPM05(x))

# (c) Weighted Power Means (WPM) with p = 2:
wpmp2 <- fit.QAM(your_data_matrix, "output_wpm_2.txt", "stats_wpm_2.txt", function(x) PM(x, p = 2), function(x) invQM(x))

# (d) Ordered Weighted Averaging Function (OWA):
owa <- fit.OWA(your_data_matrix, "output_owa.txt", "stats_owa.txt")

# (e) Choquet Integral:
choquet <- fit.choquet(your_data_matrix, "output_choquet.txt", "stats_choquet.txt", kadd = ncol(your_data_matrix) - 1)








## ###### What to do

# install.packages("dplyr")  # Uncomment and run if needed
# library(dplyr)

# List of model names and corresponding output files
model_names <- c("WAM", "WPM_p0.5", "WPM_p2", "OWA", "Choquet")
output_files <- c("output_wam.txt", "output_wpm_05.txt", "output_wpm_2.txt", "output_owa.txt", "output_choquet.txt")

# Data frame to store model evaluation results
model_results <- data.frame(Model = character(), Metric = numeric(), stringsAsFactors = FALSE)

# Loop through each model and evaluate performance
for (i in seq_along(model_names)) {
  # Read the output file for the current model
  output_data <- read.table(output_files[i], header = TRUE)
  
  # Calculate the metric of interest (replace "Accuracy" with your chosen metric)
  metric_value <- mean(output_data$Accuracy)
  
  
  
  # Loop through each model and evaluate performance
  for (i in seq_along(model_names)) {
    # Read the output file for the current model
    output_data <- read.table(output_files[i], header = TRUE)
    
    # Check the structure of output_data to identify the correct column
    print(str(output_data))
    
    # Assuming the metric of interest is in a column named "YourMetricName"
    metric_value <- mean(output_data$YourMetricName)
    
    # Store results in the data frame
    model_results <- rbind(model_results, data.frame(Model = model_names[i], Metric = metric_value))
  }
  
  # Print the results
  print(model_results)
  
  # Identify the best model based on the metric of interest
  best_model <- model_results[which.max(model_results$Metric), ]
  
  cat("Best Model:", best_model$Model, "\n")
  cat("Best Metric Value:", best_model$Metric, "\n")
  
  
  
  
  
  
  
  # Store results in the data frame
  model_results <- rbind(model_results, data.frame(Model = model_names[i], Metric = metric_value))
}

# Print the results
print(model_results)

# Identify the best model based on the metric of interest
best_model <- model_results[which.max(model_results$Metric), ]

cat("Best Model:", best_model$Model, "\n")
cat("Best Metric Value:", best_model$Metric, "\n")




## Checking
print(colnames(output_data))
print(str(output_data))

# Print the column names
print(colnames(output_data))

# Check the structure of the data
print(str(output_data))

# Check for null values in your_data_matrix
has_null_values <- any(is.na(your_data_matrix))

# Print the result
if (has_null_values) {
  print("Your matrix has null values.")
} else {
  print("Your matrix does not have null values.")
}







# T5


# New data
new_data <- c(X1 = 22, X2 = 38, X3 = 4, X4 = 88.2, X5 = 34)

# Use the min and max values from the original data
min_val <- min(my.data)
max_val <- max(my.data)

# Apply min-max scaling using the same parameters
scaled_new_data <- (new_data - min_val) / (max_val - min_val)

# Display the scaled new data
print(scaled_new_data)










# If not installed 
#install.packages("caret")

library(caret)

# Set up folds for cross-validation
folds <- createFolds(1:nrow(your_data_matrix), k = 5, list = TRUE, returnTrain = FALSE)

# Initialize a vector to store the performance of each model
model_performance <- numeric(length(models))

# Loop through each model
for (i in seq_along(models)) {
  # Initialize vector to store performance for each fold
  fold_performance <- numeric(length(folds))
  
  # Loop through each fold
  for (j in seq_along(folds)) {
    # Split data into training and validation sets
    train_data <- your_data_matrix[-folds[[j]], ]
    val_data <- your_data_matrix[folds[[j]], ]
    
    # Fit the model on the training data
    model <- fit_model(train_data, ...)  # Replace 'fit_model' with the actual function you use to fit the model
    
    # Make predictions on the validation set
    predictions <- predict(model, newdata = val_data[, -ncol(val_data)])
    
    # Evaluate performance using the chosen metric (e.g., RMSE)
    fold_performance[j] <- calculate_rmse(predictions, val_data$target_variable)  # Replace 'calculate_rmse' with your chosen metric function
  }
  
  # Average performance across folds for this model
  model_performance[i] <- mean(fold_performance)
}

# Identify the index of the best-performing model
best_model_index <- which.min(model_performance)

# Use the best model for prediction on the new, scaled data
best_model <- models[[best_model_index]]
scaled_new_data_predictions <- predict(best_model, newdata = scaled_new_data)























