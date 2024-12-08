## Lab 5: Support Vector Machines
#Load libraries
library(ggplot2)
library("caret")
library(e1071)
library(readr)
library(ggfortify)
library(e1071)
library(class)
library(psych)

#Loading wine dataset
wine <- read_csv(unz("C:/Users/chaos/Downloads/wine.zip", "wine.data"), col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

#Using the wine dataset:
#Train 2 SVM classifiers to predict the type of wine using a subset of the other 13 
#variables. You may choose the subset based on previous analysis. One using a linear 
#kernel and another of your choice.

## split train/test
train.indexes <- sample(178,0.7*178)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

## separate x (features) & y (class labels)
x <- wine[,2:11] 
y <- wine[,1]

## feature boxplots
boxplot(x, main="wine features")

## class label distributions
plot(y)

## feature-class plots
featurePlot(x = x, y = y, plot = "ellipse")
featurePlot(x = x, y = y, plot = "box")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

ggplot(wine, aes(x = Flavanoids, y = `Nonflavanoid Phenols`, colour = Type)) +
  geom_point() +
  theme_minimal()

## train SVM model - linear kernel
svm.mod0 <- svm(Type ~ ., data = train, kernel = 'linear')

svm.mod0

train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


## train SVM model - polynomial kernel
svm.mod1 <- svm(Type ~ ., data = train, kernel = 'polynomial')

svm.mod1

train.pred <- predict(svm.mod1, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

#Use tune.svmto find the optimum C and gamma values.
## Tuned SVM - polynomial
tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(wine),1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm

svm.mod2 <- svm(Type ~ ., data = train, kernel = 'polynomial', gamma = 0.69, cost = .25)

svm.mod2

train.pred <- predict(svm.mod2, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

#Choose another classification method (kNN, NaiveBayes, etc.) and train a classifier 
#based on the same features.
#kNN classifier.
# Train a classifier model to predict wine type using the 11 attributes.
set.seed(10)

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

#Compare the performance of the 2 models (Precision, Recall, F1)
#The performance of the SVM works better than the kNN metrics for precision, recall and F1.

#Using the NY housing dataset:
#Loading dataset
nyhousedataset <- read.csv("C:/Users/chaos/Downloads/NY-House-Dataset.csv")
n <- nrow(nyhousedataset)  # Number of rows
colnames(nyhousedataset)

# Ensure columns are numeric
nyhousedataset$PRICE <- as.numeric(nyhousedataset$PRICE)
nyhousedataset$PROPERTYSQFT <- as.numeric(nyhousedataset$PROPERTYSQFT)

# Remove missing values
nyhousedataset <- nyhousedataset[!is.na(nyhousedataset$PRICE) & !is.na(nyhousedataset$PROPERTYSQFT), ]

# Set a price cap (e.g., $2,000,000)
price_cap <- 2000000

# Filter dataset to exclude rows where PRICE exceeds the cap
nyhousedataset <- nyhousedataset[nyhousedataset$PRICE <= price_cap, ]


# Split train/test
set.seed(123)
n <- nrow(nyhousedataset)
train.indexes <- sample(1:n, size = 0.7 * n)
train <- nyhousedataset[train.indexes, ]
test <- nyhousedataset[-train.indexes, ]

# Train SVM model
svm_model <- svm(PRICE ~ PROPERTYSQFT, data = train, type = "eps-regression", kernel = "radial")
print(svm_model)

# Predict on test set
predicted_prices <- predict(svm_model, newdata = test)
print(predicted_prices)

# Plot predicted vs actual prices
plot(test$PRICE, predicted_prices,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Predicted vs. Actual Price",
     col = "blue", pch = 16)
abline(0, 1, col = "red")

# Feature boxplot
boxplot(predicted_prices, main = "Boxplot of Predicted Prices")

# Feature-class plots
featurePlot(x = train[, "PROPERTYSQFT", drop = FALSE], y = train$PRICE, plot = "ellipse")
featurePlot(x = train[, "PROPERTYSQFT", drop = FALSE], y = train$PRICE, plot = "box")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = train[, "PROPERTYSQFT", drop = FALSE], y = train$PRICE, plot = "density", scales = scales)

# ggplot scatter plot
ggplot(nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE, colour = TYPE)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Price vs Property Square Footage",
       x = "Property Square Footage",
       y = "Price")

# Linear model
# Train
lm_model <- lm(PRICE ~ PROPERTYSQFT, data = train)
summary(lm_model)

# Predict on test set
lm_predicted <- predict(lm_model, newdata = test)

# Plot predicted vs actual prices for linear model
plot(test$PRICE, lm_predicted,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Linear Model: Predicted vs. Actual Price",
     col = "black", pch = 16)
abline(0, 1, col = "magenta")

