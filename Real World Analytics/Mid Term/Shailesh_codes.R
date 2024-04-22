# get the working directory
getwd()

# set the working directory
setwd('C:/Users/shail/OneDrive/Documents/R Language')


# Read the data in a data frame
df_complete<- read.table("ENB_2023.txt")

# find the number of rows and columns
nrow.df_complete <- nrow(df_complete)
print(paste('Number of rows are:',nrow.df_complete))   
ncol.df_complete <- ncol(df_complete)
print(paste('Number of columns are:',ncol.df_complete))

# Check for any missing data
missing_data <- any(is.na(df_complete))  
print(paste('Missing Data :', missing_data))

# Check for duplicates
duplicate <- sum(duplicated(df_complete))  
print(paste('Duplicated Rows in the data set are:',duplicate))

# find the names of the columns
column_names <- names(df_complete)
print(paste('The Column Names in the data set are:',column_names))

# rename the columns
colnames(df_complete)<- c('X1','X2','X3','X4','X5','Y')

# veiw the dataframe
top10rows= head(df_complete,10)
View(top10rows)


# Print structure of the data frame (data types of columns)
print("Structure and Data Types:")
str(df_complete)

# Statistical Summary of the data
print("Statistical Summary of features:")
summary(df_complete)

# Check the distribution of the features by plotting Histograms for all columns
par(mfrow = c(2, ncol(df_complete)/2), # get all histograms in 2 rows & 3 cols
    mar = c(4, 4, 4, 2) + 0.1) # set the margin around the plots
for (i in 1:ncol(df_complete)) {
  hist(df_complete[[i]],
       main = paste("Histogram of", names(df_complete)[i]),  # title of plots
       xlab = names(df_complete)[i])  # X labels
}


# install packages to ascertain skewness of the distribution of the columns
install.packages("e1071")
library(e1071)
# Calculate skewness for each column
skewness_values <- sapply(df_complete, skewness)

# Print the skewness values
print(skewness_values)

# Define a function to calculate the min_max scale
min_max_scaler <- function(x) {
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)  # na.rm will ignore NA values
  if (range == 0) {
    return(rep(0, length(x)))  # shall deal with situations when all elements are same
  } else {
    return((x - min(x, na.rm = TRUE)) / range)
  }
}

# Applying the min-max scaling to all numeric columns of the data frame. 
# In this specific case all the columns are numeric in any case.
df_scaled <- as.data.frame(lapply(df_complete, function(x) {
  if (is.numeric(x)) min_max_scaler(x) else x
}))

# view the top 5 rows after the transformation
top10scaled <- head(df_scaled,10)
View((top10scaled))
# view the statistical summary
summary(df_scaled)


# Histogram of features of the scaled sub-set data
par(mfrow = c(2, 3),
    mar = c(4, 4, 4, 2) + 0.1)  # to see all histograms in the same page
for (i in 1:ncol(df_scaled)) {
  hist(df_scaled[[i]],
       main = paste("Histogram of Scaled", names(df_scaled)[i]),
       xlab = names(df_scaled)[i])
}
dim(df_scaled)

# finding the correlation
correlation_pearson <- round(cor(df_scaled,method = "pearson"),2)
correlation_pearson

par(cex.main = 0.8)  # to set the size of the title
heatmap(correlation_pearson,
        Rowv = NA,
        Colv = NA,
        main = "Pearson Correlation Matrix",
        margins = c(5,5),
        col = colorRampPalette(c("blue", "white", "red"))(20),
        cexRow = 0.8,  # to set the size of x-labels
        cexCol = 0.8)  # to set the size of y-labels




# We note that X4 ( Humidity outside (from weather station), given as a percentage)
# has the least Correlation with Y
# Therefore we drop X4 from our data set for further study

# drop X4 (Humidity Outside) as a feature from the data frame
df_scaled$X4<- NULL

dim(df_scaled)


# randomly select 340 rows out of total 671 rows (without replacement) 
set.seed(123) # to ensure reproducibility
df <- df_scaled[sample(1:671,340),c(1:5)] 
head(df) # view the top 5 rows of the randomly selected 340 samples
dim(df)  # confirm the dimensions 

View(head(df,10))

# Scatter Plots of selected features vs the dependent variable - Y
num_features = ncol(df) - 1
par(mfrow = c(num_features/2, 3))

target_feature = 'Y'
for (feature in names(df)) {
  if (feature != target_feature) {
    plot(df[[feature]], 
         df[[target_feature]], 
         main = paste(feature, "vs", target_feature),
         xlab = feature,
         ylab = target_feature,
         col = "red")  
  }
}


# install libraries to run linear programming models
library(lpSolveAPI)

source('AggWaFit718.R')


# a. A weighted arithmetic mean (WAM),
fit.QAM(df,
        output.1<-"Assessment2_AM_X4drop_output.txt",
        stats.1<-"Assessment2_AM_X4drop_stats.txt",
        g<-AM,
        g.inv<-invAM)

# b. Weighted power means (WPM) with p <- 0.5,
fit.QAM(df,
        output.1<-"Assessment2_PM05_X4drop_output.txt",
        stats.1<-"Assessment2_PM05_X4drop_stats.txt",
        g<-PM05,
        g.inv <- invPM05)

# c. Weighted power means (WPM) with p <- 2,
fit.QAM(df,
        output.1<-"Assessment2_QM_X4drop_output.txt",
        stats.1<-"Assessment2_QM_X4drop_stats.txt",
        g<-QM,
        g.inv <- invQM)


# d. An ordered weighted averaging function (OWA).
matrix_df = as.matrix(df)  # for Ordered Weighted Average the data frame is converted to a matrix
fit.OWA(matrix_df,
        output.1<-"Assessment2_OWA_X4drop_output.txt",
        stats.1<-"Assessment2_OWA_X4drop_stats.txt")

# e. The Choquet integral
fit.choquet(df,
            output.1<-"Assessment2_CHOQUET_X4drop_output.txt",
            stats.1<-"Assessment2_CHOQUET_X4drop_stats.txt")


# New data
X1<-22
X2<-38
X3<-4
X4<-88.2 # we have dropped this feature from our data set
X5<-34

summary(df_complete) # to ascertain the min and max values of the columns

min_value_X1 <- min(df_complete$X1)
min_value_X2 <- min(df_complete$X2)
min_value_X3 <- min(df_complete$X3)
min_value_X5 <- min(df_complete$X5)
min_value_Y  <- min(df_complete$Y)

max_value_X1 <- max(df_complete$X1)
max_value_X2 <- max(df_complete$X2)
max_value_X3 <- max(df_complete$X3)
max_value_X5 <- max(df_complete$X5)
max_value_Y  <- max(df_complete$Y)

# Apply min-max scaling
scale_function <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}


X1_scaled <- scale_function(X1, min_value_X1, max_value_X1)
X2_scaled <- scale_function(X2, min_value_X2, max_value_X2)
X3_scaled <- scale_function(X3, min_value_X3, max_value_X3)
X5_scaled <- scale_function(X5, min_value_X5, max_value_X5)

X1_scaled 
X2_scaled 
X3_scaled 
X5_scaled 



ChoquetWeights = c(0,
                   0,
                   0,
                   0.0682615925604888,
                   0.0682615925604888,
                   0.0682615925604888,
                   0.0682615925604888,
                   0.0620276949741637,
                   0.127987735279462,
                   0.0620276949741637,
                   0.127987735279462,
                   0.19351506089918,
                   0.382102163216329,
                   0.19351506089918,
                   0.999999999999998)




Y_scaled_predicted = choquet(c(X1_scaled,X2_scaled,X3_scaled,X5_scaled),
                             ChoquetWeights)

Y_scaled_predicted

# Apply reverse min_max scaling
rev_scale_function <- function(x, min_val, max_val) {
  (x*(max_val - min_val) + min_val)
}

Y_predicted <- rev_scale_function(Y_scaled_predicted,min_value_Y,max_value_Y)
Y_predicted

Y_actual = 100

Absolute_Percent_Error <- abs(Y_predicted - Y_actual)*100 / Y_actual
Absolute_Percent_Error

