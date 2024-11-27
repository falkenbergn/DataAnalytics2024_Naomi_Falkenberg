### Data Analytics Assignment 5 ###
## Loading libraries
library(ggplot2)
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(class)

##Load dataset
nycitywide<- read_csv("C:/Users/chaos/Downloads/NYC_Citywide_Annualized_Calendar_Sales_Update_20241111.csv")

# Initial summary of data
summary(nycitywide)

# Remove rows with missing values in key columns and filter for Borough 3
nycitywide_borough3 <- nycitywide %>%
  drop_na("BUILDING CLASS CATEGORY", "SALE PRICE", "GROSS SQUARE FEET", "NEIGHBORHOOD") %>%
  filter(BOROUGH == "3")
# Remove rows where SALE PRICE or GROSS SQUARE FEET is 0 or NA
nycitywide_borough3_clean <- nycitywide_borough3 %>%
  mutate(
    `SALE PRICE` = as.numeric(`SALE PRICE`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`)
  ) %>%
  drop_na(`SALE PRICE`, `GROSS SQUARE FEET`) %>% # Remove rows with NAs in key columns
  filter(`SALE PRICE` > 0, `GROSS SQUARE FEET` > 0) # Filter out rows with 0 or negative values

drop_na(nycitywide_borough3_clean)

# Summary after cleaning
summary(nycitywide_borough3_clean)

# --- Exploratory Data Analysis ---
#  Perform exploratory data analysis (variable distributions, etc.) and describe what you 
# did including plots and other descriptions. Identify the outlier values in the data for Sale 
# Price and generate suitable plots to demonstrate the outliers relative to the other data 
# points.

# 1. Histogram of Sale Price
ggplot(nycitywide_borough3_clean, aes(x = `SALE PRICE`)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  scale_x_log10() + # Log-transform to better view skewed data
  labs(title = "Log-transformed Sale Price Distribution", x = "Log Sale Price", y = "Frequency")

# 2. Boxplot for Sale Price
ggplot(nycitywide_borough3_clean, aes(y = `SALE PRICE`)) +
  geom_boxplot(fill = "orange", color = "black") +
  scale_y_log10() + # Log-transform for better visibility
  labs(title = "Log-transformed Sale Price Boxplot", y = "Log Sale Price")

# 3. Scatter plot of Sale Price vs. Property square feet
ggplot(nycitywide_borough3_clean, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`)) +
  geom_point(alpha = 0.6, color = "blue") +
  scale_x_log10() + 
  scale_y_log10() +
  labs(title = "Sale Price vs. Gross Square Feet (Log-Log Scale)",
       x = "Log Gross Square Feet", y = "Log Sale Price") +
  theme_minimal()

# --- Outlier Analysis ---
#Outlier assessment
Q1 <- quantile(nycitywide_borough3_clean$`SALE PRICE`, 0.25)
Q3 <- quantile(nycitywide_borough3_clean$`SALE PRICE`, 0.75)
IQR <- Q3 - Q1

outliers <- nycitywide_borough3_clean %>%
  filter(`SALE PRICE` < (Q1 - 1.5 * IQR) | `SALE PRICE` > (Q3 + 1.5 * IQR))

print(outliers)

# Threshold Outlier Plot
ggplot(nycitywide_borough3_clean, aes(x = `SALE PRICE`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = c(Q1 - 1.5 * IQR, Q3 + 1.5 * IQR), color = "red", linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Density Plot of Sale Price with Outlier Thresholds", x = "Log Sale Price", y = "Density")

# --- Plots free of outliers ---
## Setting bounds
nycitywide_filtered <- nycitywide_borough3_clean %>%
  filter(`SALE PRICE` >= 200000 & `SALE PRICE` <= 2000000)

summary(nycitywide_filtered)


# --- Histogram ---
ggplot(nycitywide_filtered, aes(x = `SALE PRICE`)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  scale_x_log10() +
  labs(title = "Log-transformed Sale Price Distribution (Without Outliers)", 
       x = "Log Sale Price", y = "Frequency")

# --- Boxplots ---
ggplot(nycitywide_filtered, aes(y = `SALE PRICE`)) +
  geom_boxplot(fill = "orange", color = "black") +
  scale_y_log10() +
  labs(title = "Log-transformed Sale Price Boxplot (Without Outliers)", 
       y = "Log Sale Price")

# --- Scatterplots ---
ggplot(nycitywide_filtered, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`)) +
  geom_point(alpha = 0.6, color = "blue") +
  scale_x_log10() + 
  scale_y_log10() +
  labs(title = "Sale Price vs. Gross Square Feet (Log-Log Scale, Without Outliers)",
       x = "Log Gross Square Feet", y = "Log Sale Price") +
  theme_minimal()

# no scaling
ggplot(nycitywide_filtered, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Sale Price vs. Gross Square Feet (Without Outliers)",
       x = "Log Gross Square Feet", y = "Log Sale Price") +
  theme_minimal()

# --- . Conduct Multivariate Regressions ---
# Conduct Multivariate Regression on the 1 borough dataset to predict the Sale Price 
# using other variables that may have a meaningful connection to price. After you identify a 
# well-performing model test it on 2 subsets of 1 borough dataset (based on meaningful 
# criteria of your choice, such as building class or sq footage value) and compare the results 
# you obtained. You may have to try multiple models and drop variables with very low 
# significance.

# Numeric SALE PRICE and GROSS SQUARE FEET
nycitywide_filtered <- nycitywide_filtered %>%
  mutate(
    `SALE PRICE` = as.numeric(`SALE PRICE`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`)
  )

# Remove rows with NA values in SALE PRICE or predictors
regression_data <- nycitywide_filtered %>%
  filter(!is.na(`SALE PRICE`) & 
           !is.na(`GROSS SQUARE FEET`) & 
           !is.na(`RESIDENTIAL UNITS`) & 
           !is.na(`COMMERCIAL UNITS`) & 
           !is.na(`YEAR BUILT`))

# --- Basic Multivariate Regression Model ---
# Build a multivariate regression model
model <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = regression_data
)
summary(model)

# --- Linear regression model ---
lm_model <- lm(`GROSS SQUARE FEET` ~ `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`, 
               data = regression_data)

# Plotting residuals 
plot(lm_model$residuals, main = "Residuals of Linear Model", 
     ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# Predicting the values
predicted_values <- predict(lm_model, newdata = regression_data)

plot(regression_data$`GROSS SQUARE FEET`, predicted_values,
     xlab = "Actual GROSS SQUARE FEET", ylab = "Predicted GROSS SQUARE FEET",
     main = "Actual vs Predicted GROSS SQUARE FEET")
abline(0, 1, col = "red")


# --- Subset Analysis ---
# High Gross Feet
subset1 <- regression_data %>%
  filter(`GROSS SQUARE FEET` > 800)

# Low Gross Feet
subset2 <- regression_data %>%
  filter(`GROSS SQUARE FEET` <= 800)

# --- Subset 1: High Gross Square Feet ---
model_subset1 <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = subset1
)

summary(model_subset1)

# --- Subset 2: Low Gross Square Feet ---
model_subset2 <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = subset2
)

summary(model_subset2)

# --- Comparing Results ---
# Compare R-squared values and coefficients
summary(model)
summary(model_subset1)$r.squared
summary(model_subset2)$r.squared


# Add a subset column to each group
model_subset1 <- subset1 %>% mutate(Subset = "High Gross Square Feet")
model_subset2 <- subset2 %>% mutate(Subset = "Low Gross Square Feet")

# Combine both subsets
combined_data <- bind_rows(model_subset1, model_subset2)

# Boxplot comparison
ggplot(combined_data, aes(x = Subset, y = `SALE PRICE`, fill = Subset)) +
  geom_boxplot() +
  scale_y_log10() + # Log-transform for better visibility of skewed data
  labs(
    title = "Comparison of Sale Price by Gross Square Feet Subset",
    x = "Gross Square Feet Subset",
    y = "Log-transformed Sale Price"
  ) +
  theme_minimal()

# Density comparison
ggplot(combined_data, aes(x = `SALE PRICE`, fill = Subset)) +
geom_density(alpha = 0.5) +
  scale_x_log10() + # Log-transform for better visibility
  labs(
    title = "Density Plot of Sale Price by Gross Square Feet Subset",
    x = "Log-transformed Sale Price",
    y = "Density"
  ) +
  theme_minimal()

# Scatterplot comparison
ggplot(combined_data, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`, color = Subset)) +
  geom_point(alpha = 0.6) +
  scale_y_log10() + # Log-transform Sale Price
  scale_x_log10() + # Log-transform Gross Square Feet
  labs(
    title = "Scatter Plot of Sale Price vs Gross Square Feet by Subset",
    x = "Log-transformed Gross Square Feet",
    y = "Log-transformed Sale Price"
  ) +
  theme_minimal()

# ---  Supervised learning model ---
# Pick more than one supervised learning model (these need not be restricted to the 
# models you’ve learned so far), e.g., Naïve Bayes, k-NN, Random Forest, SVM to explore a 
# classification problem using the data. You may choose which categorical variable (e.g.
# neighborhood, building class) to use as class label. Evaluate the results (contingency Tables & metrics).

# Clean dataset
classification_data <- nycitywide_filtered %>%
  select(`BUILDING CLASS CATEGORY`, `GROSS SQUARE FEET`, `RESIDENTIAL UNITS`, `COMMERCIAL UNITS`, `YEAR BUILT`) %>%
  mutate(
    `BUILDING CLASS CATEGORY` = as.factor(`BUILDING CLASS CATEGORY`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`),
    `RESIDENTIAL UNITS` = as.numeric(`RESIDENTIAL UNITS`),
    `COMMERCIAL UNITS` = as.numeric(`COMMERCIAL UNITS`),
    `YEAR BUILT` = as.numeric(`YEAR BUILT`)
  ) %>%
  filter(
    !is.na(`GROSS SQUARE FEET`) & 
      !is.na(`RESIDENTIAL UNITS`) & 
      !is.na(`COMMERCIAL UNITS`) & 
      !is.na(`YEAR BUILT`) & 
      !is.na(`BUILDING CLASS CATEGORY`)
  )

set.seed(10)
sample_index <- sample(nrow(classification_data), 0.7 * nrow(classification_data))
train_data <- classification_data[sample_index, ]
test_data <- classification_data[-sample_index, ]

# Separate features and labels
train_x <- train_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]
test_x <- test_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]
train_y <- train_data$`BUILDING CLASS CATEGORY`
test_y <- test_data$`BUILDING CLASS CATEGORY`

# --- Model 1: Naïve Bayes ---
# Train Naive Bayes Classifier
naive_bayes_classifier <- naiveBayes(
  classification_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")], 
  classification_data$`BUILDING CLASS CATEGORY`
)

# Evaluate Classifier
confusion_matrix <- table(
  predict(naive_bayes_classifier, classification_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]),
  classification_data$`BUILDING CLASS CATEGORY`,
  dnn = list('Predicted', 'Actual')
)
print(confusion_matrix)

# Extract Means and Standard Deviations for GROSS SQUARE FEET
category_means <- naive_bayes_classifier$tables$`GROSS SQUARE FEET`[, 1]  # Column 1: Mean
category_sds <- naive_bayes_classifier$tables$`GROSS SQUARE FEET`[, 2]    # Column 2: Standard Deviation
categories <- rownames(naive_bayes_classifier$tables$`GROSS SQUARE FEET`) # Row names (categories)

# Plot Distributions for Selected Categories (e.g., first two categories)
plot(
  function(x) dnorm(x, category_means[1], category_sds[1]),
  min(classification_data$`GROSS SQUARE FEET`), max(classification_data$`GROSS SQUARE FEET`),
  col = "red", main = "GROSS SQUARE FEET Distribution by Category", xlab = "Gross Square Feet", ylab = "Density"
)
curve(
  dnorm(x, category_means[2], category_sds[2]),
  add = TRUE, col = "blue"
)
legend(
  "topright", legend = categories[1:2], col = c("red", "blue"), lty = 1
)

# Plot Distributions for All Categories
colors <- rainbow(length(categories))

# Base plot with the first category
plot(
  function(x) dnorm(x, category_means[1], category_sds[1]),
  min(classification_data$`GROSS SQUARE FEET`), max(classification_data$`GROSS SQUARE FEET`),
  col = colors[1], main = "GROSS SQUARE FEET Distribution by Category", xlab = "Gross Square Feet", ylab = "Density"
)

# Add other categories
for (i in 2:length(categories)) {
  curve(
    dnorm(x, category_means[i], category_sds[i]),
    add = TRUE, col = colors[i]
  )
}

# Add a legend
legend(
  "topright", legend = categories, col = colors, lty = 1, cex = 0.7
)

# --- Model 2: k-NN ---
# Perform k-NN for multiple values of k
k_values <- seq(3, 102, by = 3)
accuracies <- sapply(k_values, function(k) {
  knn_predictions <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  mean(knn_predictions == test_y) # Calculate accuracy
})

# Plot accuracy against k values
plot(k_values, accuracies, type = "b", xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "k-NN Accuracy for Different k Values", ylim = c(min(accuracies), max(accuracies)))

# Find the best k
best_k <- k_values[which.max(accuracies)]
cat("Best k:", best_k, "with accuracy:", max(accuracies), "\n")

# Train and predict using the best k
KNNpred <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Confusion matrix
knn_confusion_matrix <- table(Predicted = KNNpred, Actual = test_y)
print("Confusion Matrix for best k:")
print(knn_confusion_matrix)

# Compute precision, recall, and F1 score
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- rowSums(conf_matrix)
  colsums <- colSums(conf_matrix)
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1, row.names = rownames(conf_matrix))
}

# --- Model 3: SVM linear kernel ---
svm_model <- svm(train_x, as.factor(train_y), kernel = 'linear')

# View the model details
print(svm_model)

# --- Make Predictions ---
svm_predictions <- predict(svm_model, test_x)

# --- Confusion Matrix ---
svm_conf_matrix <- table(Predicted = svm_predictions, Actual = test_y)
print("SVM Confusion Matrix:")
print(svm_conf_matrix)

# --- Compute Metrics (Accuracy, Precision, Recall, F1) ---
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  n <- sum(conf_matrix) # Total instances
  
  accuracy <- sum(diag) / n
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * precision * recall / (precision + recall)
  
  list(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
}

# Get metrics for SVM
svm_metrics <- compute_metrics(svm_conf_matrix)
print("SVM Metrics:")
print(svm_metrics)


# Print metrics
print("k-NN Metrics for best k:")
knn_metrics <- compute_metrics(knn_confusion_matrix)
print(knn_metrics)

# --- Compare Results ---
# --- Naive Bayes Model ---
nb_model <- naiveBayes(train_x, train_y)
nb_predictions <- predict(nb_model, test_x)

# Generate confusion matrix
nb_conf_matrix <- table(Predicted = nb_predictions, Actual = test_y)

# --- k-NN Model ---
best_k <- 39
knn_predictions <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Generate confusion matrix
knn_conf_matrix <- table(Predicted = knn_predictions, Actual = test_y)

# --- Metrics Comparison ---
# Helper function to compute metrics
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  n <- sum(conf_matrix) # Total instances
  
  accuracy <- sum(diag) / n
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  list(
    Accuracy = accuracy,
    Precision = mean(precision, na.rm = TRUE),
    Recall = mean(recall, na.rm = TRUE),
    F1 = mean(f1, na.rm = TRUE)
  )
}

# Compute metrics for each model
nb_metrics <- compute_metrics(nb_conf_matrix)
knn_metrics <- compute_metrics(knn_conf_matrix)

# Combine results into a single data frame
comparison_df <- data.frame(
  Metric = c("Accuracy", "Precision (Avg)", "Recall (Avg)", "F1 Score (Avg)"),
  Naive_Bayes = c(nb_metrics$Accuracy, mean(nb_metrics$Precision, na.rm = TRUE), 
                  mean(nb_metrics$Recall, na.rm = TRUE), mean(nb_metrics$F1, na.rm = TRUE)),
  kNN = c(knn_metrics$Accuracy, mean(knn_metrics$Precision, na.rm = TRUE), 
          mean(knn_metrics$Recall, na.rm = TRUE), mean(knn_metrics$F1, na.rm = TRUE)),
  SVM = c(svm_metrics$Accuracy, mean(svm_metrics$Precision, na.rm = TRUE), 
          mean(svm_metrics$Recall, na.rm = TRUE), mean(svm_metrics$F1, na.rm = TRUE))
)

# Print the comparison table
print(comparison_df)

# --- For entire 5 Borough set ---
# --- Regression Model ALL ---
nycitywide_clean<- nycitywide %>%
  drop_na("BUILDING CLASS CATEGORY", "SALE PRICE", "GROSS SQUARE FEET", "NEIGHBORHOOD")

# Remove rows where SALE PRICE or GROSS SQUARE FEET is 0 or NA
nycitywide_clean <- nycitywide_clean %>%
  mutate(
    `SALE PRICE` = as.numeric(`SALE PRICE`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`)
  ) %>%
  drop_na(`SALE PRICE`, `GROSS SQUARE FEET`) %>% # Remove rows with NAs in key columns
  filter(`SALE PRICE` > 0, `GROSS SQUARE FEET` > 0) # Filter out rows with 0 or negative values

drop_na(nycitywide_clean)

summary(nycitywide_clean)

# Numeric SALE PRICE and GROSS SQUARE FEET
nycitywide_clean <- nycitywide_clean %>%
  mutate(
    `SALE PRICE` = as.numeric(`SALE PRICE`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`)
  )

# Remove rows with NA values in SALE PRICE or predictors
regression_data_all <- nycitywide_clean %>%
  filter(!is.na(`SALE PRICE`) & 
           !is.na(`GROSS SQUARE FEET`) & 
           !is.na(`RESIDENTIAL UNITS`) & 
           !is.na(`COMMERCIAL UNITS`) & 
           !is.na(`YEAR BUILT`))

# --- Step 1: Basic Multivariate Regression Model ---
# Build a multivariate regression model
model_all <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = regression_data_all
)
summary(model_all)

# --- Linear regression ---
lm_model_all <- lm(`GROSS SQUARE FEET` ~ `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`, 
               data = regression_data_all)

# Plotting residuals 
plot(lm_model_all$residuals, main = "Residuals of Linear Model", 
     ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# Predicting the values from the linear model
predicted_values_all <- predict(lm_model_all, newdata = regression_data_all)

# More residuals
residuals_all <- regression_data_all$`SALE PRICE` - predicted_values_all

# Plot residuals to check for patterns
plot(residuals_all, main = "Residuals of Linear Model", 
     ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

plot(regression_data_all$`SALE PRICE`, predicted_values_all,
     xlab = "Actual SALE PRICE", ylab = "Predicted SALE PRICE",
     main = "Actual vs Predicted SALE PRICE")
abline(0, 1, col = "red")

# --- Step 2: Subset Analysis ---
# High Gross Feet
subset1_all <- regression_data_all %>%
  filter(`GROSS SQUARE FEET` > 1224)

#Low Gross Feet
subset2_all <- regression_data_all %>%
  filter(`GROSS SQUARE FEET` <= 1224)

# --- Subset 1: High Gross Square Feet ---
model_subset1_all <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = subset1_all
)

summary(model_subset1_all)

# --- Subset 2: Low Gross Square Feet ---
model_subset2_all <- lm(
  `SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `COMMERCIAL UNITS` + `YEAR BUILT`,
  data = subset2_all
)

summary(model_subset2_all)
# Compare R-squared values and coefficients between the full model and subsets
summary(model_subset1_all)$r.squared
summary(model_subset2_all)$r.squared


# Add a subset column to each group
model_subset1_all <- subset1_all %>% mutate(Subset = "High Gross Square Feet")
model_subset2_all <- subset2_all %>% mutate(Subset = "Low Gross Square Feet")

# Combine both subsets
combined_data_all <- bind_rows(model_subset1_all, model_subset2_all)

# Boxplot comparison
ggplot(combined_data_all, aes(x = Subset, y = `SALE PRICE`, fill = Subset)) +
  geom_boxplot() +
  scale_y_log10() + # Log-transform for better visibility of skewed data
  labs(
    title = "Comparison of Sale Price by Gross Square Feet Subset",
    x = "Gross Square Feet Subset",
    y = "Log-transformed Sale Price"
  ) +
  theme_minimal()

# Density comparison
ggplot(combined_data_all, aes(x = `SALE PRICE`, fill = Subset)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() + # Log-transform for better visibility
  labs(
    title = "Density Plot of Sale Price by Gross Square Feet Subset",
    x = "Log-transformed Sale Price",
    y = "Density"
  ) +
  theme_minimal()

# Scatterplot comparison
ggplot(combined_data_all, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`, color = Subset)) +
  geom_point(alpha = 0.6) +
  scale_y_log10() + # Log-transform Sale Price
  scale_x_log10() + # Log-transform Gross Square Feet
  labs(
    title = "Scatter Plot of Sale Price vs Gross Square Feet by Subset",
    x = "Log-transformed Gross Square Feet",
    y = "Log-transformed Sale Price"
  ) +
  theme_minimal()

# --- Classification Model ALL ---
classification_data_all <- nycitywide_clean %>%
  select(`BUILDING CLASS CATEGORY`, `GROSS SQUARE FEET`, `RESIDENTIAL UNITS`, `COMMERCIAL UNITS`, `YEAR BUILT`) %>%
  mutate(
    `BUILDING CLASS CATEGORY` = as.factor(`BUILDING CLASS CATEGORY`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`),
    `RESIDENTIAL UNITS` = as.numeric(`RESIDENTIAL UNITS`),
    `COMMERCIAL UNITS` = as.numeric(`COMMERCIAL UNITS`),
    `YEAR BUILT` = as.numeric(`YEAR BUILT`)
  ) %>%
  filter(
    !is.na(`GROSS SQUARE FEET`) & 
      !is.na(`RESIDENTIAL UNITS`) & 
      !is.na(`COMMERCIAL UNITS`) & 
      !is.na(`YEAR BUILT`) & 
      !is.na(`BUILDING CLASS CATEGORY`)
  )

set.seed(2)
sample_index <- sample(nrow(classification_data_all), 0.7 * nrow(classification_data_all))
train_data <- classification_data_all[sample_index, ]
test_data <- classification_data_all[-sample_index, ]

# Separate features and labels
train_x <- train_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]
test_x <- test_data[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]
train_y <- train_data$`BUILDING CLASS CATEGORY`
test_y <- test_data$`BUILDING CLASS CATEGORY`

# --- Model 1: Naïve Bayes ---
# Train Naive Bayes Classifier
naive_bayes_classifier <- naiveBayes(
  classification_data_all[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")], 
  classification_data_all$`BUILDING CLASS CATEGORY`
)

# Evaluate Classifier
confusion_matrix <- table(
  predict(naive_bayes_classifier, classification_data_all[, c("GROSS SQUARE FEET", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT")]),
  classification_data_all$`BUILDING CLASS CATEGORY`,
  dnn = list('Predicted', 'Actual')
)
print(confusion_matrix)

# Extract Means and Standard Deviations for GROSS SQUARE FEET
category_means <- naive_bayes_classifier$tables$`GROSS SQUARE FEET`[, 1]  # Column 1: Mean
category_sds <- naive_bayes_classifier$tables$`GROSS SQUARE FEET`[, 2]    # Column 2: Standard Deviation
categories <- rownames(naive_bayes_classifier$tables$`GROSS SQUARE FEET`) # Row names (categories)

# Plot Distributions for Selected Categories (e.g., first two categories)
plot(
  function(x) dnorm(x, category_means[1], category_sds[1]),
  min(classification_data_all$`GROSS SQUARE FEET`), max(classification_data_all$`GROSS SQUARE FEET`),
  col = "red", main = "GROSS SQUARE FEET Distribution by Category", xlab = "Gross Square Feet", ylab = "Density"
)
curve(
  dnorm(x, category_means[2], category_sds[2]),
  add = TRUE, col = "blue"
)
legend(
  "topright", legend = categories[1:2], col = c("red", "blue"), lty = 1
)

# Plot Distributions for All Categories
colors <- rainbow(length(categories))

# Base plot with the first category
plot(
  function(x) dnorm(x, category_means[1], category_sds[1]),
  min(classification_data_all$`GROSS SQUARE FEET`), max(classification_data_all$`GROSS SQUARE FEET`),
  col = colors[1], main = "GROSS SQUARE FEET Distribution by Category", xlab = "Gross Square Feet", ylab = "Density"
)

# Add other categories
for (i in 2:length(categories)) {
  curve(
    dnorm(x, category_means[i], category_sds[i]),
    add = TRUE, col = colors[i]
  )
}

# Add a legend
legend(
  "bottom", legend = categories, col = colors, lty = 1, cex = 0.7
)

# --- Model 2: k-NN ---
# Perform k-NN for multiple values of k
k_values <- seq(3, 102, by = 3)
accuracies <- sapply(k_values, function(k) {
  knn_predictions <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  mean(knn_predictions == test_y) # Calculate accuracy
})

# Plot accuracy against k values
plot(k_values, accuracies, type = "b", xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "k-NN Accuracy for Different k Values", ylim = c(min(accuracies), max(accuracies)))

# Find the best k
best_k <- k_values[which.max(accuracies)]
cat("Best k:", best_k, "with accuracy:", max(accuracies), "\n")

# Train and predict using the best k
KNNpred <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Confusion matrix
knn_confusion_matrix <- table(Predicted = KNNpred, Actual = test_y)
print("Confusion Matrix for best k:")
print(knn_confusion_matrix)

# Compute precision, recall, and F1 score
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- rowSums(conf_matrix)
  colsums <- colSums(conf_matrix)
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1, row.names = rownames(conf_matrix))
}

# --- Model 3: SVM linear kernel ---
svm_model <- svm(train_x, as.factor(train_y), kernel = 'linear')

# View the model details
print(svm_model)

# --- Make Predictions ---
svm_predictions <- predict(svm_model, test_x)

# --- Confusion Matrix ---
svm_conf_matrix <- table(Predicted = svm_predictions, Actual = test_y)
print("SVM Confusion Matrix:")
print(svm_conf_matrix)

# --- Compute Metrics (Accuracy, Precision, Recall, F1) ---
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  n <- sum(conf_matrix) # Total instances
  
  accuracy <- sum(diag) / n
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * precision * recall / (precision + recall)
  
  list(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
}

# Get metrics for SVM
svm_metrics <- compute_metrics(svm_conf_matrix)
print("SVM Metrics:")
print(svm_metrics)


# Print metrics
print("k-NN Metrics for best k:")
knn_metrics <- compute_metrics(knn_confusion_matrix)
print(knn_metrics)

# --- Compare Results ---
# --- Naive Bayes Model ---
nb_model <- naiveBayes(train_x, train_y)
nb_predictions <- predict(nb_model, test_x)

# Generate confusion matrix
nb_conf_matrix <- table(Predicted = nb_predictions, Actual = test_y)

# --- k-NN Model ---
best_k <- 9
knn_predictions <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Generate confusion matrix
knn_conf_matrix <- table(Predicted = knn_predictions, Actual = test_y)

# --- Metrics Comparison ---
# Helper function to compute metrics
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  n <- sum(conf_matrix) # Total instances
  
  accuracy <- sum(diag) / n
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  list(
    Accuracy = accuracy,
    Precision = mean(precision, na.rm = TRUE),
    Recall = mean(recall, na.rm = TRUE),
    F1 = mean(f1, na.rm = TRUE)
  )
}

# Compute metrics for each model
nb_metrics <- compute_metrics(nb_conf_matrix)
knn_metrics <- compute_metrics(knn_conf_matrix)

# Combine results into a single data frame
comparison_df <- data.frame(
  Metric = c("Accuracy", "Precision (Avg)", "Recall (Avg)", "F1 Score (Avg)"),
  Naive_Bayes = c(nb_metrics$Accuracy, mean(nb_metrics$Precision, na.rm = TRUE), 
                  mean(nb_metrics$Recall, na.rm = TRUE), mean(nb_metrics$F1, na.rm = TRUE)),
  kNN = c(knn_metrics$Accuracy, mean(knn_metrics$Precision, na.rm = TRUE), 
          mean(knn_metrics$Recall, na.rm = TRUE), mean(knn_metrics$F1, na.rm = TRUE)),
  SVM = c(svm_metrics$Accuracy, mean(svm_metrics$Precision, na.rm = TRUE), 
          mean(svm_metrics$Recall, na.rm = TRUE), mean(svm_metrics$F1, na.rm = TRUE))
)

# Print the comparison table
print(comparison_df)
