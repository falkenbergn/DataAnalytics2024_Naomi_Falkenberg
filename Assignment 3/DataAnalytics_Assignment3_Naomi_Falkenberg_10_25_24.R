#### Assignment 3: Data Analytics ####
###  New York Times Covid-19 dataset ###

## Loading Files ##
uscounties2020 <- read.csv("C:/Users/chaos/Downloads/us-counties-2020.csv")
uscounties2023 <- read.csv("C:/Users/chaos/Downloads/us-counties-2023.csv")

## Omit NA values
uscounties2020 <- na.omit(read.csv("C:/Users/chaos/Downloads/us-counties-2020.csv"))
uscounties2023 <- na.omit(read.csv("C:/Users/chaos/Downloads/us-counties-2023.csv"))

## Differentiation for datasets
uscounties2020$Year <- "2020"
uscounties2023$Year <- "2023"

colnames(uscounties2020)[colnames(uscounties2020) == "cases"] <- "Cases"
colnames(uscounties2020)[colnames(uscounties2020) == "deaths"] <- "Deaths"

colnames(uscounties2023)[colnames(uscounties2023) == "cases"] <- "Cases"
colnames(uscounties2023)[colnames(uscounties2023) == "deaths"] <- "Deaths"

## Check combined dataset
uscounties <- rbind(uscounties2020,uscounties2023)
uscounties <- na.omit(uscounties[, c("Year", "Cases", "Deaths")])

## Boxplots ##
# Cases vs. Year (2020 vs. 2023)
library(ggplot2)
ggplot(uscounties, aes(x = Year, y = Cases)) +
  geom_boxplot() + scale_y_log10()+
  labs(title = "COVID-19 Cases 2020 vs 2023", x = "Year", y = "cases ")

summary(uscounties$Cases)

# Deaths vs. Year (2020 vs. 2023)
ggplot(uscounties, aes(x = Year, y = Deaths)) +
  geom_boxplot() + scale_y_log10()+
  labs(title = "COVID-19 Deaths 2020 vs 2023", x = "Year", y = "Deaths")

summary(uscounties$Deaths)

## Histograms ##
## Covid-19 Cases 2020 Histogram
summary(uscounties2020$Cases)
hist(uscounties2020$Cases,
     main = "Histogram of COVID-19 Cases (2020)", 
     xlab = "Number of Cases", 
     ylab = "Frequency",
     breaks = seq(0, 800000, by = 1000),
     xlim = c(0,5e+04))

# Load library
library(MASS)

# Remove zero and non-finite values for log-normal fitting
cleaned_cases2020 <- uscounties2020$Cases[uscounties2020$Cases > 0]

# Fit a log-normal distribution to the cleaned cases
log_normal_fit_cases_2020 <- fitdistr(cleaned_cases2020, "lognormal")
print(log_normal_fit_cases_2020)

# meanlog and sdlog
meanlog_cases_2020 <- log_normal_fit_cases_2020$estimate["meanlog"]
sdlog_cases_2020 <- log_normal_fit_cases_2020$estimate["sdlog"]

# Plot the histogram of COVID-19 cases
hist(uscounties2020$Cases,
     main = "Histogram of COVID-19 Cases (2020)", 
     xlab = "Number of Cases", 
     ylab = "Frequency",
     breaks = seq(0, 800000, by = 1000),
     xlim = c(0,5e+04),
     prob = TRUE)

# Create a sequence of x-values for plotting the log-normal density
x_values_2020 <- seq(1, 50000, length.out = 1000)

# Calculate the y-values for the log-normal distribution
y_values_2020 <- dlnorm(x_values_2020, meanlog = meanlog_cases_2020, sdlog = sdlog_cases_2020)

# Overlay the log-normal distribution curve on the histogram
lines(x_values_2020, y_values_2020, col = "blue", lwd = 2)

## Covid-19 Cases 2023 Histogram
summary(uscounties2023$Cases)
hist(uscounties2023$Cases,
     main = "Histogram of COVID-19 Cases (2023)", 
     xlab = "Number of Cases", 
     ylab = "Frequency",
     breaks = seq(0, 4000000, by = 30000),
     xlim = c(0,1.6e+06))

# Remove zero and non-finite values for log-normal fitting
cleaned_cases2023 <- uscounties2023$Cases[uscounties2023$Cases > 0]

# Fit a log-normal distribution to the cleaned cases
log_normal_fit_cases_2023 <- fitdistr(cleaned_cases2023, "lognormal")
print(log_normal_fit_cases_2023)

# meanlog and sdlog
meanlog_cases_2023 <- log_normal_fit_cases_2023$estimate["meanlog"]
sdlog_cases_2023 <- log_normal_fit_cases_2023$estimate["sdlog"]

# Plot the histogram of COVID-19 cases (2023) with probability density
hist(uscounties2023$Cases,
     main = "Histogram of COVID-19 Cases (2023)", 
     xlab = "Number of Cases", 
     ylab = "Frequency",
     breaks = seq(0, 4000000, by = 30000),
     xlim = c(0, 1.6e+06),
     prob = TRUE)

# x values
x_values_2023 <- seq(1, 1.6e+06, length.out = 1000)

# Calculate the y-values for the log-normal distribution
y_values_2023 <- dlnorm(x_values_2023, meanlog = meanlog_cases_2023, sdlog = sdlog_cases_2023)

# Overlay the log-normal distribution curve on the histogram
lines(x_values_2023, y_values_2023, col = "blue", lwd = 2)

## Covid-19 Deaths 2020 Histogram
summary(uscounties2020$Deaths)
hist(uscounties2020$Deaths,
     main = "Histogram of COVID-19 Deaths (2020)", 
     xlab = "Number of Deaths", 
     ylab = "Frequency",
     breaks = seq(0, 11000, by = 50),
     xlim = c(0,2500))

# Covid-19 Deaths 2020 Log Overlay
# Remove zero and non-finite values for log-normal fitting
cleaned_deaths2020 <- uscounties2020$Deaths[uscounties2020$Deaths > 0]

# Fit a log-normal distribution to the cleaned cases
log_normal_fit_deaths_2020 <- fitdistr(cleaned_deaths2020, "lognormal")
print(log_normal_fit_deaths_2020)

# meanlog and sdlog
meanlog_deaths_2020 <- log_normal_fit_deaths_2020$estimate["meanlog"]
sdlog_deaths_2020 <- log_normal_fit_deaths_2020$estimate["sdlog"]

# Plot the histogram of COVID-19 cases
hist(uscounties2020$Deaths,
     main = "Histogram of COVID-19 Deaths (2020)", 
     xlab = "Number of Deaths", 
     ylab = "Frequency",
     breaks = seq(0, 11000, by = 50),
     xlim = c(0,2500),
     prob = TRUE)

# Create a sequence of x-values for plotting the log-normal density
x_values_deaths_2020 <- seq(1, 2500, length.out = 50)

# Calculate the y-values for the log-normal distribution
y_values_deaths_2020 <- dlnorm(x_values_deaths_2020, meanlog = meanlog_deaths_2020, sdlog = sdlog_deaths_2020)

# Overlay the log-normal distribution curve on the histogram
lines(x_values_deaths_2020, y_values_deaths_2020, col = "red", lwd = 2)

## Covid-19 Deaths 2023 Histogram
summary(uscounties2023$Deaths)
hist(uscounties2023$Deaths,
     main = "Histogram of COVID-19 Deaths (2023)", 
     xlab = "Number of Deaths", 
     ylab = "Frequency",
     breaks = seq(0, 40000, by = 340),
     xlim = c(0,20000))

# Covid-19 Deaths 2023 Log Overlay
# Remove zero and non-finite values for log-normal fitting
cleaned_deaths2023 <- uscounties2023$Deaths[uscounties2023$Deaths > 0]

# Fit a log-normal distribution to the cleaned cases
log_normal_fit_deaths_2023 <- fitdistr(cleaned_deaths2023, "lognormal")
print(log_normal_fit_deaths_2023)

# meanlog and sdlog
meanlog_deaths_2023 <- log_normal_fit_deaths_2023$estimate["meanlog"]
sdlog_deaths_2023 <- log_normal_fit_deaths_2023$estimate["sdlog"]

# Plot the histogram of COVID-19 cases
hist(uscounties2023$Deaths,
     main = "Histogram of COVID-19 Deaths (2023)", 
     xlab = "Number of Deaths", 
     ylab = "Frequency",
     breaks = seq(0, 40000, by = 340),
     xlim = c(0,20000),
     prob = TRUE)

# Create a sequence of x-values for plotting the log-normal density
x_values_deaths_2023 <- seq(1, 20000, length.out = 340)

# Calculate the y-values for the log-normal distribution
y_values_deaths_2023 <- dlnorm(x_values_deaths_2023, meanlog = meanlog_deaths_2023, sdlog = sdlog_deaths_2023)

# Overlay the log-normal distribution curve on the histogram
lines(x_values_deaths_2023, y_values_deaths_2023, col = "red", lwd = 2)

## ECDFs ##
## Covid-19 Cases 2020 ECDF
#exclude NA values and anything below 500
uscounties2020.subset<- uscounties2020[uscounties2020$Cases <500,]
uscounties2020.subset <- uscounties2020.subset[complete.cases(uscounties2020.subset),]

#ECDF Plot
summary(uscounties2020.subset$Cases)
sd(uscounties2020.subset$Cases)
mu <- 2     # mean 
sd <- 2 # standard deviation 
n <- 1000   # sample size
uscounties2020.casesdata2020 <- rlnorm(n, mean = mu, sd = sd)
plot(ecdf(uscounties2020.casesdata2020), do.points=FALSE, main="Covid-19 Cases 2020")
lines(ecdf(uscounties2020.subset$Cases))

# Covid-19 Cases 2020 QQ-Plots
qqnorm(uscounties2020$Cases, main = "Q-Q Plot of COVID-19 Cases (2020)")
qqline(uscounties2020$Cases, col = "blue")

# Q-Q Plot for Cases using the fitted log-normal distribution
qqplot(qnorm(ppoints(length(cleaned_cases2020))), 
       log(cleaned_cases2020), 
       main = "Q-Q Plot of Log-Normal COVID-19 Cases (2020)")
qqline(log(cleaned_cases2020), col = "blue")

## Covid-19 Cases 2023 ECDF
#exclude NA values and anything below 500
uscounties2023.subset<- uscounties2023[uscounties2023$Cases <500,]
uscounties2023.subset <- uscounties2023.subset[complete.cases(uscounties2023.subset),]

#ECDF Plot
summary(uscounties2023.subset$Cases)
sd(uscounties2023.subset$Cases)
mu <- 2     # mean 
sd <- 2 # standard deviation 
n <- 1000   # sample size
uscounties2023.casesdata2023 <- rlnorm(n, mean = mu, sd = sd)
plot(ecdf(uscounties2023.casesdata2023), do.points=FALSE, main="Covid-19 Cases 2023")
lines(ecdf(uscounties2023.subset$Cases))

# Covid-19 Cases 2023 Log QQ-Plots
qqnorm(uscounties2023$Cases, main = "Q-Q Plot of COVID-19 Cases (2023)")
qqline(uscounties2023$Cases, col = "blue")

# Q-Q Plot for Cases using the fitted log-normal distribution
qqplot(qnorm(ppoints(length(cleaned_cases2023))), 
       log(cleaned_cases2023), 
       main = "Q-Q Plot of Log-Normal COVID-19 Cases (2023)")
qqline(log(cleaned_cases2023), col = "blue")

## Covid-19 Deaths 2020 ECDF
#exclude NA values and anything below 500
uscounties2020_deaths.subset<- uscounties2020[uscounties2020$Deaths <500,]
uscounties2020_deaths.subset <- uscounties2020_deaths.subset[complete.cases(uscounties2020_deaths.subset),]

#ECDF Plot
summary(uscounties2020_deaths.subset$Deaths)
sd(uscounties2020_deaths.subset$Deaths)
mu <- 2     # mean 
sd <- 2 # standard deviation 
n <- 1000   # sample size
uscounties2020.deathsdata2020 <- rlnorm(n, mean = mu, sd = sd)
plot(ecdf(uscounties2020.deathsdata2020), do.points=FALSE, main="Covid-19 Deaths 2020")
lines(ecdf(uscounties2020_deaths.subset$Deaths))

# Covid-19 Deaths 2020 QQ-Plots
qqnorm(uscounties2020$Deaths, main = "Q-Q Plot of COVID-19 Deaths (2020)")
qqline(uscounties2020$Deaths, col = "red")

# Q-Q Plot for Deaths using the fitted log-normal distribution
qqplot(qnorm(ppoints(length(cleaned_deaths2020))), 
       log(cleaned_cases2020), 
       main = "Q-Q Plot of Log-Normal COVID-19 Deaths (2020)")
qqline(log(cleaned_deaths2020), col = "red")

## Covid-19 Deaths 2023 ECDF
#exclude NA values and anything below 500
uscounties2023_deaths.subset<- uscounties2023[uscounties2023$Deaths <500,]
uscounties2023_deaths.subset <- uscounties2023_deaths.subset[complete.cases(uscounties2023_deaths.subset),]

#ECDF Plot
summary(uscounties2023_deaths.subset$Deaths)
sd(uscounties2023_deaths.subset$Deaths)
mu <- 2     # mean 
sd <- 2 # standard deviation 
n <- 1000   # sample size
uscounties2023.deathsdata2023 <- rlnorm(n, mean = mu, sd = sd)
plot(ecdf(uscounties2023.deathsdata2023), do.points=FALSE, main="Covid-19 Deaths 2023")
lines(ecdf(uscounties2023_deaths.subset$Deaths))

# Covid-19 Deaths 2023 QQ-Plots
qqnorm(uscounties2023$Deaths, main = "Q-Q Plot of COVID-19 Deaths (2023)")
qqline(uscounties2023$Deaths, col = "red")

# Q-Q Plot for Deaths using the fitted log-normal distribution
qqplot(qnorm(ppoints(length(cleaned_deaths2023))), 
       log(cleaned_deaths2023), 
       main = "Q-Q Plot of Log-Normal COVID-19 Deaths (2023)")
qqline(log(cleaned_deaths2023), col = "red")

### NY house dataset ###
## Loading Files ##
nyhousedataset <- read.csv("C:/Users/chaos/Downloads/NY-House-Dataset.csv")

# Filtering out outliers with the extreme PRICE value
filtered_nyhousedataset <- nyhousedataset[nyhousedataset$PRICE != 2147483647 & nyhousedataset$PRICE != 195000000, ]

##Fit a linear model using the formula PRICE ~ BEDS + BATH + PROPERTYSQFT and ## 
#identify the variable most significantly influencing house price. Produce a scatterplot of # 
#that variable with another and overlay the best fit line. Plot the residuals of the linear model.#
model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = filtered_nyhousedataset)
summary(model)
print(model)

#Propertysqft was the most significantly influenced by house pricing.
lin.mod.sqft_price <- lm(PRICE ~ PROPERTYSQFT, data = filtered_nyhousedataset)
summary(lin.mod.sqft_price)

#Plot PROPERTYSQFT and PRICE
library(ggplot2)
ggplot(filtered_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(x = "PROPERTYSQFT", y = "PRICE", title = "PROPERTYSQFT v. PRICE with Fitted Line")

# Summary of the linear model
summary(lin.mod.sqft_price)

# ggplot with PROPERTYSQFT vs PRICE and a linear regression line
ggplot(filtered_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of PROPERTYSQFT vs PRICE", x = "PROPERTYSQFT", y = "PRICE")

# Residual vs Fitted plot using ggplot
ggplot(lin.mod.sqft_price, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "blue") +
  labs(title = 'Residual vs. Fitted PROPERTYSQFT vs. PRICE Values', 
       x = 'Fitted Values', 
       y = 'Residuals')

# Scatterplot of PROPERTYSQFT and PRICE
ggplot(filtered_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
labs(title = 'Scatterplot of PROPERTYSQFT vs PRICE', x = 'PROPERTYSQFT', y = 'PRICE')

# Scatterplot of PROPERTYSQFT and BEDS
ggplot(filtered_nyhousedataset, aes(x = PROPERTYSQFT, y = BEDS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
labs(title = "Scatterplot of PROPERTYSQFT vs BEDS", x = "PROPERTYSQFT", y = "BEDS")

##Derive a subset of the dataset according to any criteria (e.g. PRICE > VALUE or BEDS < NUMBER) and repeat the linear model with its plots.##
VALUE <- 2000000
price_nyhousedataset <- subset(filtered_nyhousedataset, PRICE > VALUE)
model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = price_nyhousedataset)
summary(model)
print(model)

#Propertysqft was the most significantly influenced by house pricing.
lin.mod.sqft_price_subset <- lm(PRICE ~ PROPERTYSQFT, data = price_nyhousedataset)
summary(lin.mod.sqft_price_subset)

#Plot PROPERTYSQFT and PRICE
ggplot(price_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "lm", col = "red", lwd = 2) +
  labs(x = "PROPERTYSQFT", y = "PRICE") + 
  ggtitle("PROPERTYSQFT vs. PRICE with Fitted Line")


# Summary of the linear model
summary(lin.mod.sqft_price_subset)

# ggplot with PROPERTYSQFT vs PRICE and a linear regression line
library(ggplot2)
ggplot(price_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Linear Regression of PROPERTYSQFT vs PRICE", x = "PROPERTYSQFT", y = "PRICE")

# Residual vs Fitted plot using ggplot
ggplot(lin.mod.sqft_price_subset, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "blue") +
  labs(title = 'Residual vs. Fitted PROPERTYSQFT vs. PRICE Values', x = 'Fitted Values', y = 'Residuals')

# Scatterplot of PROPERTYSQFT and PRICE
ggplot(price_nyhousedataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
labs(title = 'Scatterplot of PROPERTYSQFT vs PRICE', x = "PROPERTYSQFT", y = "PRICE")

# Scatterplot of PROPERTYSQFT and BEDS
ggplot(price_nyhousedataset, aes(x = PROPERTYSQFT, y = BEDS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
labs(title = 'Scatterplot of PROPERTYSQFT vs BEDS', x = "PROPERTYSQFT", y = "BEDS")

#### END ####
