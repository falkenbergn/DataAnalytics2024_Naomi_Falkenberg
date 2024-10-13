### Lab 03/ Assignment 2: Exploratory data analysis: examining distributions, linear models, classification and clustering ###
### Read files ###

epi2024results06022024<- read.csv("C:/Users/chaos/Downloads/epi2024results_DA_F24_lab03.csv")

## Variable Distributions (2%) ##
# Using the EPI results dataset to perform the following:
#1. Derive 2 subsets each for a particular region
#1.1. Plot histograms for a variable of your choice for both regions with density lines overlayed
#Subset Group 1: Global West
epi.globalwest <-epi2024results06022024[epi2024results06022024$region == "Global West",]

#Subset Group 2: Asia-Pacific
epi.asia_pacific <-epi2024results06022024[epi2024results06022024$region == "Asia-Pacific",]

##Variable: AIR 
#No Subset:
hist(epi2024results06022024$AIR)

hist(epi2024results06022024$AIR, seq(0., 90., 5.0), prob=TRUE)

rug(epi2024results06022024$AIR)

lines(density(epi2024results06022024$AIR,na.rm=TRUE,bw=1))
lines(density(epi2024results06022024$AIR,na.rm=TRUE,bw="SJ"))

x <- seq(0., 90., 1.0)

#Subset: Global West
hist(epi.globalwest$AIR)

hist(epi.globalwest$AIR, seq(50., 90., 5.0), prob=TRUE)

rug(epi.globalwest$AIR)

lines(density(epi.globalwest$AIR,na.rm=TRUE,bw=1))
lines(density(epi.globalwest$AIR,na.rm=TRUE,bw="SJ"))

x <- seq(50., 90., 1.0)

#Subset: Asia-Pacific
hist(epi.asia_pacific$AIR)

hist(epi.asia_pacific$AIR, seq(0., 70., 5.0), prob=TRUE)

rug(epi.asia_pacific$AIR)

lines(density(epi.asia_pacific$AIR,na.rm=TRUE,bw=1))
lines(density(epi.asia_pacific$AIR,na.rm=TRUE,bw="SJ"))

x <- seq(0., 70., 1.0)

#1.2. Plot QQ plots for both variables compared to known probability distributions

##Variable: AIR
#No Subset:
qqnorm(epi2024results06022024$AIR, main = "Q-Q Plot of AIR")
qqline(epi2024results06022024$AIR)

x <- seq(0., 90., 1.0)
qqplot(qnorm(ppoints(200)), x, main = "Q-Q Plot of AIR")
qqline(x)

qqplot(qnorm(ppoints(200)),epi2024results06022024$AIR, main = "Q-Q Plot of AIR")
qqline(epi2024results06022024$AIR)

qqplot(rnorm(1000),epi2024results06022024$AIR, main = "Q-Q Plot of AIR")
qqline(epi2024results06022024$AIR)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

#Subset: Global West
qqnorm(epi.globalwest$AIR, main = "Q-Q Plot of Global West AIR")
qqline(epi.globalwest$AIR)

x <- seq(50., 90., 1.0)
qqplot(qnorm(ppoints(200)), x, main = "Q-Q Plot of GlobaL West AIR")
qqline(x)

qqplot(qnorm(ppoints(200)),epi.globalwest$AIR, main = "Q-Q Plot of GlobaL West AIR")
qqline(epi.globalwest$AIR)

qqplot(rnorm(1000),epi.globalwest$AIR)
qqline(epi.globalwest$AIR)

d1_west <- rnorm(10000)
d2_west <- rnorm(10000)
qqplot(d1_west,d1_west)
qqline(d1_west)

#Subset: Asia-Pacific
qqnorm(epi.asia_pacific$AIR, main = "Q-Q Plot of Asia-Pacific AIR")
qqline(epi.asia_pacific$AIR)

x <- seq(0., 70., 1.0)
qqplot(qnorm(ppoints(200)), x, main = "Q-Q Plot of Asia-Pacific AIR")
qqline(x)

qqplot(qnorm(ppoints(200)),epi.asia_pacific$AIR, main = "Q-Q Plot of Asia-Pacific AIR")
qqline(epi.asia_pacific$AIR)

qqplot(rnorm(1000),epi.asia_pacific$AIR)
qqline(epi.asia_pacific$AIR)

d1_asia <- rnorm(10000)
d2_asia <- rnorm(10000)
qqplot(d1_asia,d1_asia)
qqline(d1_asia)

#Variable AIR and Regions Together:
qqnorm(epi2024results06022024$AIR, main = "Q-Q Plot of AIR, Global West, Asia-Pacific", col = "black", pch = 1)
qqline(epi2024results06022024$AIR, col = "green")
qqpoints <- qqnorm(epi.globalwest$AIR, plot.it = FALSE)
points(qqpoints, col = "black", pch = 2)
qqline(epi.globalwest$AIR, col = "magenta")
qqpoints <- qqnorm(epi.asia_pacific$AIR, plot.it = FALSE)
points(qqpoints, col = "black", pch = 3)
qqline(epi.asia_pacific$AIR, col = "blue")

legend("bottomright", legend = c("AIR", "Global West AIR", "Asia-Pacific AIR"), 
       col = c("green", "magenta", "blue"),
       pch = c(1, 2, 3),                  
       lty = 1,                             
       bty = "n")

#Variable AIR and Regions Together with qqnorm:
qqplot(qnorm(ppoints(200)), epi2024results06022024$AIR, main = "Q-Q Plot of AIR", col = "black", pch = 1)
qqline(epi2024results06022024$AIR, col = "green")
qqpoints <- qqnorm((ppoints(200)), epi.globalwest$AIR, plot.it = FALSE)
points(qqpoints, col = "black", pch = 2)
qqline(epi.globalwest$AIR, col = "magenta")
qqpoints <- qqnorm(ppoints(200),epi.asia_pacific$AIR, plot.it = FALSE)
points(qqpoints, col = "black", pch = 3)
qqline(epi.asia_pacific$AIR, col = "blue")


#Variable Global West vs. Asia-Pacific
qqnorm(epi.globalwest$AIR, main = "Q-Q Plot of Global West vs. Asia-Pacific AIR", col = "black", pch = 2)
qqline(epi.globalwest$AIR, col = "magenta")
qqpoints <- qqnorm(epi.asia_pacific$AIR, plot.it = FALSE)
points(qqpoints, col = "black", pch = 3)
qqline(epi.asia_pacific$AIR, col = "blue")

legend("topleft", legend = c("Global West AIR", "Asia-Pacific AIR"), 
       col = c("magenta", "blue"),
       pch = c(2, 3),                  
       lty = 1,                             
       bty = "n")



## Linear Models (3%) ##
# Using the EPI results dataset to perform the following:
# 2. Fit linear models as follows:
# 2.1. Choose a subset of 5 variables (excluding EPI) and using the formula EPI~VAR1+VAR2+VAR3+
  #VAR4+VAR5, fit a linear model and identify which variable most significantly influences EPI. Plot that variable 
  #with another and overlay the fitted line.
#Choosing 5 variables (excluding EPI): AIR, GTP, H2O, USD, HMT
model <- lm(EPI ~ AIR + GTP + H2O + USD + HMT, data = epi2024results06022024)

#Determining most significant variable
summary(model)

#create linear model of GTP, the most significant variable, with AIR 
lin.mod.air_gtp <- lm(AIR ~ GTP, data = epi2024results06022024)
# Plot AIR vs GTP
plot(epi2024results06022024$GTP, epi2024results06022024$AIR, 
     xlab = "GTP", 
     ylab = "AIR", 
     main = "AIR vs GTP with Fitted Line")
abline(lin.mod.air_gtp, col = "red", lwd = 2)

# Summary of the linear model
summary(lin.mod.air_gtp)

# ggplot with AIR vs GTP and a linear regression line
library(ggplot2)
ggplot(epi2024results06022024, aes(x = GTP, y = AIR)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "AIR vs GTP with Fitted Line", x = "GTP", y = "AIR")

# Residual vs Fitted plot using ggplot
ggplot(lin.mod.air_gtp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "blue") +
  labs(title = 'Residual vs. Fitted AIR vs. GTP Values Plot', x = 'Fitted Values', y = 'Residuals')

#2.2. Repeat the previous model with a subset of 1 region and in 1-2 sentences explain which model is a 
      #better fit and why you think that is the case

#Asia-Pacific Region
epi.asia_pacific <-epi2024results06022024[epi2024results06022024$region == "Asia-Pacific",]

asia_pacific_model <- lm(EPI ~ AIR + GTP + H2O + USD + HMT, data = epi.asia_pacific)
summary(asia_pacific_model)

#create linear model of GTP, the most significant variable, with AIR 
lin.mod.asia_air_gtp <- lm(AIR ~ GTP, data = epi.asia_pacific)

# Plot AIR vs GTP
plot(epi.asia_pacific$GTP, epi.asia_pacific$AIR, 
     xlab = "GTP", 
     ylab = "AIR", 
     main = "AIR vs GTP with Fitted Line")
abline(lin.mod.asia_air_gtp, col = "red", lwd = 2)

# Summary of the linear model
summary(lin.mod.asia_air_gtp)

# ggplot with AIR vs GTP and a linear regression line
ggplot(epi.asia_pacific, aes(x = GTP, y = AIR)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "AIR vs GTP with Fitted Line", x = "GTP", y = "AIR")

# Residual vs Fitted plot using ggplot
ggplot(lin.mod.asia_air_gtp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "blue") +
  labs(title = 'Residual vs. Fitted AIR vs. GTP Values Plot', x = 'Fitted Values', y = 'Residuals')
  
## Classification (kNN) (3%) ##
#Using the EPI results dataset to perform the following:
# 3. Train 2 kNN models using ”region” as the class label as follows:
# 3.1. Choose a subset of 5 variables and filter the subset by region keeping 3 regions out of 8 (representing 3 
      #classes), then train a kNN model to predict the region based on these variables. Evaluate the model using a 
      #contingency matrix and calculate the accuracy of correct classifications.
library(class)
selected_regions <-c("Global West", "Asia-Pacific", "Eastern Europe")
epi.results.sub <- subset(epi2024results06022024, region %in% selected_regions)
variables_chosen <- c("AIR", "GTP", "H2O", "USD", "HMT", "region")

set.seed(123)
n <- nrow(epi.results.sub)
train.indexes <- sample(n, n * 0.7)

epi.train <- epi.results.sub[train.indexes, variables_chosen]
epi.test <- epi.results.sub[-train.indexes, variables_chosen]

sqrt(66)

k=8

KNNpred <- knn(train = epi.train[, 1:5], test = epi.test[, 1:5], cl = epi.train$region, k = k)
contingency.table <- table(Actual = KNNpred, Predicted = epi.test$region)
print(contingency.table)                           

contingency.matrix <- as.matrix(contingency.table)
accuracy <- sum(diag(contingency.matrix)) / length(epi.test$region)                           

print(paste("Accuracy for k =", k, ":", accuracy))

accuracy <- c()
ks <- c(3,5,7,9,11,13,15)

for (k in ks) {
  KNNpred <- knn(train = epi.train[, 1:5], test = epi.test[, 1:5], cl = epi.train$region, k = k)
  cm <- as.matrix(table(Actual = KNNpred, Predicted = epi.test$region))
  
  accuracy <- c(accuracy, sum(diag(cm)) / length(epi.test$region))
}

plot(ks, accuracy, type = "b", xlab = "k (Number of Neighbors)", ylab = "Accuracy", main = "kNN Model Accuracy vs k")

#3.2. Repeat the previous model with the same variables for another set of 3 other regions and evaluate. In 1-2 
      #sentences explain which model is better and why you think that is the case
library(class)
selected_regions2 <-c("Greater Middle East", "Latin America & Caribbean", "Sub-Saharan Africa")
epi.results.sub2 <- subset(epi2024results06022024, region %in% selected_regions2)
variables_chosen2 <- c("AIR", "GTP", "H2O", "USD", "HMT", "region")

set.seed(234)
n <- nrow(epi.results.sub2)
train.indexes <- sample(n, n * 0.7)

epi.train2 <- epi.results.sub2[train.indexes, variables_chosen2]
epi.test2 <- epi.results.sub2[-train.indexes, variables_chosen2]

sqrt(94)

k=10

KNNpred <- knn(train = epi.train2[, 1:5], test = epi.test2[, 1:5], cl = epi.train2$region, k = k)
contingency.table <- table(Actual = KNNpred, Predicted = epi.test2$region)
print(contingency.table)                           

contingency.matrix <- as.matrix(contingency.table)
accuracy <- sum(diag(contingency.matrix)) / length(epi.test2$region)                           

print(paste("Accuracy for k =", k, ":", accuracy))

accuracy <- c()
ks <- c(3,5,7,9,11,13,15)

for (k in ks) {
  KNNpred <- knn(train = epi.train2[, 1:5], test = epi.test2[, 1:5], cl = epi.train2$region, k = k)
  cm <- as.matrix(table(Actual = KNNpred, Predicted = epi.test2$region))
  
  accuracy <- c(accuracy, sum(diag(cm)) / length(epi.test2$region))
}

plot(ks, accuracy, type = "b", xlab = "k (Number of Neighbors)", ylab = "Accuracy", main = "kNN Model Accuracy vs k")

## Clustering (3%) ##
#Using the EPI results dataset to perform the following:
#4. Fit a k-means model for a subset of 5 variables for 2 different groups of regions (3 each)
#4.1. Compare the performance of the models using their within cluster sum of squares.
## plot dataset colored by class
selected_regions3 <-c("Asia-Pacific", "Global West")
epi.results.sub3 <- subset(epi2024results06022024, region %in% selected_regions3, c("AIR", "GTP", "H2O", "USD", "HMT", "region"))

#Variable comparison AIR and GTP
ggplot(epi.results.sub3, aes(x = AIR, y = GTP, colour = region)) +
  geom_point() +
  labs(title = "Scatterplot of AIR vs GTP", x = "AIR", y = "GTP")

#Variable comparison H2O and USD
ggplot(epi.results.sub3, aes(x = H2O, y = USD, colour = region)) +
  geom_point() +
  labs(title = "Scatterplot of H2O vs USD", x = "H2O", y = "USD")

#Variable comparison AIR and HMT
ggplot(epi.results.sub3, aes(x = AIR, y = HMT, colour = region)) +
  geom_point() +
  labs(title = "Scatterplot of AIR vs HMT", x = "AIR", y = "HMT")

## set random number generator start value
set.seed(345)

## train kmeans
epi.km <- kmeans(epi.results.sub3[,-6], centers = 2)

## WCSS: total within cluster sum of squares
epi.km$tot.withinss

## get and plot clustering output
assigned.clusters <- as.factor(epi.km$cluster)

#Variable comparison AIR and GTP
ggplot(epi.results.sub3, aes(x = AIR, y = GTP, colour = assigned.clusters)) +
  geom_point() +
  labs(title = "K-Means Clustering of AIR and GTP for Asia-Pacific and Global West", 
       x = "AIR", y = "GTP")

#Variable comparison H2O and USD
ggplot(epi.results.sub3, aes(x = H2O, y = USD, colour = assigned.clusters)) +
  geom_point() +
  labs(title = "K-Means Clustering of H2O and USD for Asia-Pacific and Global West", 
       x = "H2O", y = "USD")

#Variable comparison AIR and HMT
ggplot(epi.results.sub3, aes(x = AIR, y = HMT, colour = assigned.clusters)) +
  geom_point() +
  labs(title = "K-Means Clustering of AIR and HMT for Asia-Pacific and Global West", 
       x = "AIR", y = "HMT")

#4.2. In a loop fit kmeans models for both subsets using multiple values of k. Plot WCSS across k values. In 
      #1-2 sentences explain which model is better and why you think that is the case.

## run tests with multiple k values and plot WCSS
#k test 1
wcss <- c()
ks <- c(2,3,4,5)

for (k in ks) {
  
  epi.km <- kmeans(epi.results.sub3[,-6], centers = k)
  
  wcss <- c(wcss,epi.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

#k test 2
wcss <- c()
ks <- c(1,3,5,7,9,11,15,17,19)

for (k in ks) {
  
  epi.km <- kmeans(epi.results.sub3[,-6], centers = k)
  
  wcss <- c(wcss,epi.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

#k test 3
wcss <- c()
ks <- c(1,3)

for (k in ks) {
  
  epi.km <- kmeans(epi.results.sub3[,-6], centers = k)
  
  wcss <- c(wcss,epi.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

#### END ####
