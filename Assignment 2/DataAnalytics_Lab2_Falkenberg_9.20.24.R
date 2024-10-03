####### Data Analytics Fall 2024 Lab 02 ######
library(ggplot2)

### set working directory
setwd("~/Courses/Data Analytics/Fall24/labs/lab01/")

### read in data
epi.results <- read.csv("C:/Users/chaos/Downloads/epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("C:/Users/chaos/Downloads/epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))


### Quantile-quantile plots

qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),EPI.new)
qqline(EPI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))


#### Populations Dataset ####

## read data
populations_2023 <- read.csv("C:/Users/chaos/Downloads/countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)

plot(EPI.new~population_log)
abline(lin.mod.epinew)

summary(lin.mod.epinew)

plot(lin.mod.epinew)


ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~EPI.new,epi.results.sub)
plot(population_log~EPI.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = EPI.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#Exercise 1: fitting a distribution
#Variable 1: SHI
epi.results$SHI.new
epi.results[1,5]
attach(epi.results)
SHI.new
SHI.new[1]
## NA values
na.indices <- is.na(SHI.new) 

## drop NAs
Shi.new.compl <- SHI.new[!na.indices]

## convert to data frame and add country
Shi.new.compl <- data.frame(Country = country[!na.indices], SHI = SHI.new[!na.indices])

## summary stats
summary(SHI.new)

fivenum(SHI.new,na.rm=TRUE)

## histograms
hist(SHI.new)

hist(SHI.new, seq(0., 100., 2.0), prob=TRUE)

rug(SHI.new)

lines(density(SHI.new,na.rm=TRUE,bw=1))
lines(density(SHI.new,na.rm=TRUE,bw="SJ"))

x <- seq(0., 100., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(PFL.old, SHI.new, names=c("PFL.old","SHI.new"))


### Quantile-quantile plots

qqnorm(SHI.new)
qqline(SHI.new)

x <- seq(0., 100., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),SHI.new)
qqline(SHI.new)

qqplot(rnorm(1000),SHI.new)
qqline(SHI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(SHI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. SHI.new ECDF")
lines(ecdf(SHI.new))

plot(ecdf(SHI.old), do.points=FALSE, main="SHI.old vs. SHI.new ECDF")
lines(ecdf(SHI.new))

#fitting other distributions
#Chi square
qqplot(qchisq(ppoints(200), df=5),SHI.new)
qqline(SHI.new)

#Variable 2: AIR
epi.results$AIR.new
epi.results[1,5]
attach(epi.results)
AIR.new
AIR.new[1]
## NA values
na.indices <- is.na(AIR.new) 

## drop NAs
Air.new.compl <- AIR.new[!na.indices]

## convert to data frame and add country
Air.new.compl <- data.frame(Country = country[!na.indices], AIR = AIR.new[!na.indices])

## summary stats
summary(AIR.new)

fivenum(AIR.new,na.rm=TRUE)

## histograms
hist(AIR.new)

hist(AIR.new, seq(0., 100., 2.0), prob=TRUE)

rug(AIR.new)

lines(density(AIR.new,na.rm=TRUE,bw=1))
lines(density(AIR.new,na.rm=TRUE,bw="SJ"))

x <- seq(0., 100., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(WWR.old, AIR.new, names=c("WWR.old","AIR.new"))


### Quantile-quantile plots

qqnorm(AIR.new)
qqline(AIR.new)

x <- seq(0., 100., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),AIR.new)
qqline(AIR.new)

qqplot(rnorm(1000),AIR.new)
qqline(AIR.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(AIR.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. AIR.new ECDF")
lines(ecdf(AIR.new))

plot(ecdf(AIR.old), do.points=FALSE, main="AIR.old vs. AIR.new ECDF")
lines(ecdf(AIR.new))

#fitting other distributions
#Chi square
qqplot(qchisq(ppoints(200), df=5),AIR.new)
qqline(AIR.new)

#Variable 3:USD
epi.results$USD.new
epi.results[1,5]
attach(epi.results)
USD.new
USD.new[1]
## NA values
na.indices <- is.na(USD.new) 

## drop NAs
Usd.new.compl <- USD.new[!na.indices]

## convert to data frame and add country
Usd.new.compl <- data.frame(Country = country[!na.indices], USD = USD.new[!na.indices])

## summary stats
summary(USD.new)

fivenum(USD.new,na.rm=TRUE)

## histograms
hist(USD.new)

hist(USD.new, seq(0., 100., 2.0), prob=TRUE)

rug(USD.new)

lines(density(USD.new,na.rm=TRUE,bw=1))
lines(density(USD.new,na.rm=TRUE,bw="SJ"))

x <- seq(0., 100., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(USD.new, H2O.new, names=c("USD.new","H2O.new"))


### Quantile-quantile plots

qqnorm(USD.new)
qqline(USD.new)

x <- seq(0., 100., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),USD.new)
qqline(USD.new)

qqplot(rnorm(1000),USD.new)
qqline(USD.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(USD.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. USD.new ECDF")
lines(ecdf(USD.new))

plot(ecdf(USD.old), do.points=FALSE, main="USD.old vs. USD.new ECDF")
lines(ecdf(USD.new))

#fitting other distributions
#Chi square
qqplot(qchisq(ppoints(200), df=5),USD.new)
qqline(USD.new)

#Boxplot comparing 3 variables
boxplot(TCG.old, AIR.new, HFD.old, names=c("WWR.old","AIR.new", "HFD.old"))

#Q-Q plots for 3 variables compared to some known distribution
par(mfrow = c(1, 3))
qqnorm(AIR.new, main = "Q-Q Plot of AIR.new")
qqline(AIR.new, col = "blue")
qqnorm(HPE.new, main = "Q-Q Plot of HPE.new")
qqline(HPE.new, col = "orange")
qqnorm(OZD.new, main = "Q-Q Plot of OZD.new")
qqline(OZD.new, col = "purple")
par(mfrow = c(1, 1))

#ECDF plots for 3 variables compared to each other
plot(ecdf(AIR.new), do.points=FALSE, main="AIR.new vs. SHI.new vs. HPE.new ECDF")
lines(ecdf(epi.results$AIR.new))
lines(ecdf(epi.results$SHI.new))
lines(ecdf(epi.results$HPE.new))

#Exercise 2: linear models
#Variable 1: GTI 
## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","GTI.old","GTI.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.gtinew <- lm(GTI.new~population_log,epi.results.sub)

plot(GTI.new~population_log)
abline(lin.mod.gtinew)

summary(lin.mod.gtinew)

plot(lin.mod.gtinew)


ggplot(epi.results.sub, aes(x = population_log, y = GTI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.gtinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~GTI.new,epi.results.sub)
plot(population_log~GTI.new)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = GTI.new, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#Variable 2: GTP
## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","GTP.old","GTP.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.gtpnew <- lm(GTP.new~population_log,epi.results.sub)

plot(GTP.new~population_log)
abline(lin.mod.gtpnew)

summary(lin.mod.gtpnew)

plot(lin.mod.gtpnew)


ggplot(epi.results.sub, aes(x = population_log, y = GTP.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.gtpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~GTP.new,epi.results.sub)
plot(population_log~GTP.new)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = GTP.new, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#Variable #3: PAR
## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","PAR.old","PAR.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.parnew <- lm(PAR.new~population_log,epi.results.sub)

plot(PAR.new~population_log)
abline(lin.mod.parnew)

summary(lin.mod.parnew)

plot(lin.mod.parnew)


ggplot(epi.results.sub, aes(x = population_log, y = PAR.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.parnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~PAR.new,epi.results.sub)
plot(population_log~PAR.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = PAR.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
