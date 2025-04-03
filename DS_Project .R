
# Load the dataset

data <- read.csv(file.choose())
View(data)

# Use the first 300 rows
data <- head(data,300)

# Summary of Dataset 
summary(data)

# Run simple Linear Regression
model <- lm(data$mpg ~ data$horsepower, data = data)
summary(model)

B0 <- model$coefficients[1]
B1 <- model$coefficients[2]
B0

horsepower <- 17

data <- B0 + B1 * horsepower
data

Y <- 23 + -54.63333 * horsepower
Y


# Load the dataset
data <- read.csv(file.choose()) 


# Convert horsepower to numeric, handling missing values (Organizing data)
data$horsepower <- as.numeric(as.character(data$horsepower))


# Remove rows with missing horsepower values (Cleaning the data)
data <- data[!is.na(data$horsepower), ]


# Split the dataset into training (first 300 rows) and testing (remaining 98 rows)
train_data <- head(data, 300)
test_data <- tail(data, 98)



# Fit the best-performing linear regression model
# I am using horsepower, weight, and acceleration as predictors
best_model <- lm(mpg ~ horsepower + weight + acceleration, data = train_data)
summary(best_model)


# Predict mpg for the test data
predictions <- predict(best_model, newdata = test_data)


# Print the first few rows of the test data with predictions and residuals
head(test_data)

# Calculate Residuals
residuals <- test_data$mpg - predictions

# Residual Plot
plot(predictions, residuals, 
     xlab = "Predicted MPG", 
     ylab = "Residuals", 
     main = "Residual Plot",
     pch = 19, col = "blue")

# Histogram of Residuals
hist(residuals, 
     breaks = 20, 
     main = "Histogram of Residuals",
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")



