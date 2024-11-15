##########################################
### Principal Component Analysis (PCA) ###
##########################################
library(ggplot2)
library(readr)
library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
wine <- read_csv(unz("C:/Users/chaos/Downloads/wine.zip", "wine.data"), col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

# Performing PCA with scaling variables
wine.pca <- prcomp(wine[, -1], center = TRUE, scale. = TRUE)

# Plotting the first two principal components
autoplot(wine.pca, data = wine, colour = 'Type', x = 1, y = 2,
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Wine PCA: First and Second Principal Components") +
  theme_minimal()

# Identifying the variables that contribute the most to the 1st PC in rstudio
# Loadings for the first principal component (PC1)
loading_scores <- wine.pca$rotation[, 1]

# Calculating absolute values of the loading scores
abs_loading_scores <- abs(loading_scores)
ranked_variables <- sort(abs_loading_scores, decreasing = TRUE)
print(ranked_variables)

#Variables that most greatly contribute are Flavanoids, Total phenols, and Od280/od315 of diluted wines 

# Train a classifier model to predict wine type using the 11 attributes.
set.seed(42)

# Sample ~70% of the dataset
sample_index <- sample(nrow(wine), 0.7 * nrow(wine))
wine.train <- wine[sample_index, ]
wine.test <- wine[-sample_index, ]

# Extract predictor variables and target variable for training and testing
train_x <- wine.train[, -1]  
train_y <- wine.train$Type  
test_x <- wine.test[, -1]     
test_y <- wine.test$Type      

# Calculate the approximate square root
sqrt_train_size <- sqrt(nrow(wine.train))
k <- round(sqrt_train_size)

# Choosing the number of neighbors (k) for the model
accuracy <- c()
ks <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99)

# Looping through different k values to evaluate accuracy
for (k in ks) {
  # Train and predict using k-NN
  KNNpred <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  
  # Create confusion matrix
  contingency.table <- table(Predicted = KNNpred, Actual = test_y)
  contingency.matrix <- as.matrix(contingency.table)
  
  # Calculate accuracy for each k
  accuracy <- c(accuracy, sum(diag(contingency.matrix)) / length(test_y))
}
# Plot accuracy against k values
plot(ks, accuracy, type = "b", xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "k-NN Accuracy for Different k Values", ylim = c(min(accuracy), max(accuracy)))

# Selecting the best k (based on highest accuracy) and print metrics
best_k <- ks[which.max(accuracy)]
cat("Best k:", best_k, "\n")

# Train and predict using the best k
KNNpred <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Printing
knn_confusion_matrix <- table(Predicted = KNNpred, Actual = test_y)
print("Confusion Matrix for best k:")
print(knn_confusion_matrix)

# Computing precision, recall, and F1 score
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

# Print
print("k-NN Metrics for best k:")
knn_metrics <- compute_metrics(knn_confusion_matrix)
print(knn_metrics)

#Train a classifier model to predict wine type using the data projected into the first 3 PCs.
set.seed(36)
wine_pca_data <- data.frame(Type = wine$Type, wine.pca$x[, 1:3])

# Sample ~70% of the PCA-transformed data
sample_index <- sample(nrow(wine_pca_data), 0.7 * nrow(wine_pca_data))
train_pca_data <- wine_pca_data[sample_index, ]
test_pca_data <- wine_pca_data[-sample_index, ]

# Extract predictor variables and target variable for training and testing
train_pca_x <- train_pca_data[, -1]  
train_pca_y <- train_pca_data$Type  
test_pca_x <- test_pca_data[, -1]   
test_pca_y <- test_pca_data$Type    

# k tests
accuracy <- c()
ks <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99)

# Loop through different k values to evaluate accuracy on PCA-reduced data
for (k in ks) {
  # Train and predict using k-NN
  knn_pca_pred <- knn(train = train_pca_x, test = test_pca_x, cl = train_pca_y, k = k)
  
  # Create confusion matrix
  pca_contingency_table <- table(Predicted = knn_pca_pred, Actual = test_pca_y)
  pca_contingency_matrix <- as.matrix(pca_contingency_table)
  
  # Calculate accuracy for each k
  accuracy <- c(accuracy, sum(diag(pca_contingency_matrix)) / length(test_pca_y))
}

# Plot accuracy against k values
plot(ks, accuracy, type = "b", xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "k-NN Accuracy on PCA Data for Different k Values", ylim = c(min(accuracy), max(accuracy)))

# Select the best k (based on highest accuracy) and display metrics
best_k <- ks[which.max(accuracy)]
cat("Best k:", best_k, "\n")

# Train and predict using the best k on PCA-reduced data
knn_pca_pred <- knn(train = train_pca_x, test = test_pca_x, cl = train_pca_y, k = best_k)

# Display the final confusion matrix for the best k on PCA data
knn_pca_confusion_matrix <- table(Predicted = knn_pca_pred, Actual = test_pca_y)
print("Confusion Matrix for best k (PCA data):")
print(knn_pca_confusion_matrix)

# Computing precision, recall, and F1 score
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

# Print
print("k-NN Metrics for best k (PCA-transformed data):")
knn_pca_metrics <- compute_metrics(knn_pca_confusion_matrix)
print(knn_pca_metrics)

# Drop the least contributing variables (Color Intensity, Alcohol, Magnesium)
wine_reduced <- wine[, !names(wine) %in% c("Color Intensity", "Alcohol", "Magnesium")]

# Perform PCA again on the reduced dataset
wine_reduced.pca <- prcomp(wine_reduced[, -1], center = TRUE, scale. = TRUE)

# Plotting the first two principal components
autoplot(wine_reduced.pca, data = wine, colour = 'Type', x = 1, y = 2,
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Reduced Wine Dataset PCA: First and Second Principal Components") +
  theme_minimal()


# Rerunning PCA: Reduced dataset onto the first three principal components
set.seed(60)
wine_reduced_pca_data <- data.frame(Type = wine$Type, wine_reduced.pca$x[, 1:3])

# Sample ~70% of the reduced PCA-transformed data
sample_index <- sample(nrow(wine_reduced_pca_data), 0.7 * nrow(wine_reduced_pca_data))
train_reduced_pca_data <- wine_reduced_pca_data[sample_index, ]
test_reduced_pca_data <- wine_reduced_pca_data[-sample_index, ]

# Extracting predictor variables and target variable for training and testing
train_reduced_pca_x <- train_reduced_pca_data[, -1]  
train_reduced_pca_y <- train_reduced_pca_data$Type   
test_reduced_pca_x <- test_reduced_pca_data[, -1]    
test_reduced_pca_y <- test_reduced_pca_data$Type     

# k test
accuracy <- c()
ks <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99)

# Loop through different k values on reduced PCA data
for (k in ks) {
  # Train and predict using k-NN on the reduced PCA-transformed data
  knn_reduced_pca_pred <- knn(train = train_reduced_pca_x, test = test_reduced_pca_x, cl = train_reduced_pca_y, k = k)
  
  # Create confusion matrix
  reduced_pca_contingency_table <- table(Predicted = knn_reduced_pca_pred, Actual = test_reduced_pca_y)
  reduced_pca_contingency_matrix <- as.matrix(reduced_pca_contingency_table)
  
  # Calculate accuracy for each k
  accuracy <- c(accuracy, sum(diag(reduced_pca_contingency_matrix)) / length(test_reduced_pca_y))
}

# Plot accuracy against k values
plot(ks, accuracy, type = "b", xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "k-NN Accuracy on Reduced PCA Data for Different k Values", ylim = c(min(accuracy), max(accuracy)))

# Select the best k (based on highest accuracy) and display metrics
best_k <- ks[which.max(accuracy)]
cat("Best k:", best_k, "\n")

# Train and predict using the best k on reduced PCA data
knn_reduced_pca_pred <- knn(train = train_reduced_pca_x, test = test_reduced_pca_x, cl = train_reduced_pca_y, k = best_k)

# Print
knn_reduced_pca_confusion_matrix <- table(Predicted = knn_reduced_pca_pred, Actual = test_reduced_pca_y)
print("Confusion Matrix for best k (Reduced PCA data):")
print(knn_reduced_pca_confusion_matrix)

# Compute precision, recall, and F1 score
compute_metrics <- function(conf_matrix) {
  diag <- diag(conf_matrix)
  rowsums <- apply(conf_matrix, 1, sum)
  colsums <- apply(conf_matrix, 2, sum)
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * (precision * recall) / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

# Print
print("k-NN Metrics for best k (Reduced PCA-transformed data):")
knn_reduced_pca_metrics <- compute_metrics(knn_reduced_pca_confusion_matrix)
print(knn_reduced_pca_metrics)

##Comparing the 3 classification models-- see contingency tables/recall/precision/f1 metrics.
#Training a classifier model to predict wine type using the data projected into the first 3 PCs performed the best with its 1:1:1 ability in all three categories.

