setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project1/NO2/")
##################################Importing necessary libraries ###########
library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")

##################################Loading the dataset##################################
no2 = read.csv("NO2.csv", col.names = c("particles", "carsHour", "temp2", "windSpeed", "tempDiff25to2", "windDir", "time", "day"))
attach(no2)
head(no2)

##################################First Exploratory Analysis##################################
pairs.panels(no2, 
             method = "pearson", # correlation method
             hist.col = "#00146E",
             col = "red",
             lm = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             #pch = c(21,21)[class],
             #bg=my_cols[class],
             rug = FALSE,
             cex.cor = 5,
             scale = TRUE,
             density = TRUE  # show density plots
)

##################################FIXING THE VARIABLES#######################################
## Cleaning day by season. The day variable is measured as number of days from October 1. 2001
# Spring = 0; Summer = 1; Autumn = 2; Winter = 3
summary(day)
seasons <- day
for (i in 1:length(day)) {
  if ((day[i] >= 0 && day[i] <= 91) || (day[i] >= 366 && day[i] <= 457)) {
    seasons[i] <- as.double(2)
  }
  else if ((day[i] >= 92 && day[i] <= 183) || (day[i] >= 458 && day[i] <= 549)) {
    seasons[i] <- as.double(3)
  }
  else if ((day[i] >= 184 && day[i] <= 275) || (day[i] >= 549 && day[i] <= 608)) {
    seasons[i] <- as.double(0)
  }
  else if (day[i] >= 276 && day[i] <= 365) {
    seasons[i] <- as.double(1)
  }
}

no2$day <- seasons

## Fixing wind direction by splitting it into two clusters since there seems to be two clearly different wind directions
clusterWindDir <- kmeans(windDir,2)$cluster
no2$windDir<-clusterWindDir-1

#Ploting the data coloring according to the categorical values (No clear difference in the data with these two groupings):
pairs(no2[c(sapply(no2,class)!="factor")],col=no2$windDir)
pairs(no2[c(sapply(no2,class)!="factor")],col=no2$day)

###################################DESCRIPTIVE ANALYSIS######################
dev.off() # Cleaning the plotting area

#This functions plots two graphs that help descrive the different variables
variable_analysis<-function(data,chart_name="chart_name"){
  par(mfcol=c(1,2),oma=c(0,0,2,0))
  
  boxplot(data,main=names(data),title="Boxplot")
  text(x = 1.4,labels = round(boxplot.stats(data)$stats,1),y = boxplot.stats(data)$stats)
  
  plot(density(data),main = "",xlab = "")  
  abline()
  mtext(chart_name,outer = T,cex=2)
}

variable_analysis(carsHour,"Cars per Hour")
variable_analysis(particles, "Particles")
variable_analysis(temp2, "Temperature at 2 meters")
variable_analysis(windSpeed, "Wind Speed")
variable_analysis(tempDiff25to2, "Difference in Temperature")


###################### ANALYZING LINEARITY PREDICTORS VS RESPONSE ###################
# This function plots the response variable vs the predictor.
# It also builds a linear model and tests the normality of the residuals assumptions
# by plotting its distribution.
models<-function(tobepredict="temp2",predictor="particles",data=no2){
  temp_1<-sapply(no2[names(no2)==tobepredict],as.numeric)
  temp_2<-sapply(no2[names(no2)==predictor],as.numeric)
  mod_temp <- lm(temp_1 ~ temp_2, data = no2)
  print(summary(mod_temp))
  #plot(temp_2~temp_1,xlab = predictor,ylab=tobepredict,main="Linear Model") The scatterplot shows more info
  plot(density(mod_temp$residuals),main="Theorical residuals Vs Empirical")
  lines(density(rnorm(n=10000,sd=sd(mod_temp$residuals))),col="blue")
  scatterplot(x=temp_1,y=temp_2,col = 1, regLine = FALSE, smooth = FALSE,xlab = predictor,ylab = tobepredict)
  #abline(coef=mod_temp$coefficients, col = "red")
  print("Check the two graphs")
  return(mod_temp)
}

## particles vs carsHour
# Linear relation is observed
modCarsParticles <- models("particles","carsHour")
# Slightly negative skewness
skew(modCarsParticles$residuals)

## particles vs temp2 
# Linear relation can not be appreciated (cloud of points)
modTempParticles <- models("particles","temp2")
# Negative skewed - not large
skew(modTempParticles$residuals)

## Particles vs windSpeed 
# Linear relation can be observed
modWindSpeedParticles <- models("particles","windSpeed")
# Similar skewness to that of temp2
skew(modWindSpeedParticles$residuals)

## particles vs tempDiff25to2 
# Not linear relation. Most of the points close to 0 -> low variance of the predictor, might not be useful 
modTempDiffParticles <- models("particles","tempDiff25to2")
# Similar skewness to that of temp2
skew(modTempDiffParticles$residuals)

dev.off()
## particles VS windDir
# WinDir transformed into binary variable, as we've seen before there's no division according to windDir
plot(particles,col=clusterWindDir,pch=19,cex=0.8)

## CarsHour vs time 
# The correlation coefficient of this two predictors is high but looking at the first 
# graph these predictors seem to have some king of sinusoidal relation.
# We are going to try to prove that time has a trigonometric relation with carsHour
summary(time)
per <- max(time)
plot(carsHour~time)
timePlot <- linspace(1, 24, 1000)
lines(6.2 - 2*sin((2*pi/24)*timePlot+0.5)~timePlot)

## particles vs time
# Difficult to fit a model because time is discrete and has a trigonometric relation 
# with carsHour which is enough reason to not use this predictor.
models("particles","time")

## particles vs day. Already fixed the variable. 
# No insights from the plot
plot(particles, col=no2$day)

##################################BUILDING THE LINEAR MODELS##################################
# -7 is applied in order to delete time because it does not meet linearity assumptions.

mod <- lm(particles ~ ., data = no2[,-7])
summary(mod)
# From this result, we can conclude that day & winDir are not significant variables
anova(mod)
# From this result, we can see that the variable that has a higher weight on the response is carsHour

modBIC <- stepAIC(mod, k=log(length(particles)))
cleanModel <- lm(no2[,-7])
cleanModelBIC <- stepAIC(cleanModel, k = log(length(particles)))
#Previous hints from ANOVA are confirmed using BIC

#Ploting the predictors maintained after applying BIC and response variable.
pairs.panels(no2[,c(1,2,3,4,5)],
             method = "pearson", # correlation method
             hist.col = "#00146E",
             lm = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             #pch = c(21,21,21,21)[as.factor(seasons)],
             #bg=c("green", "red", "yellow", "blue")[as.factor(seasons)],
             rug = FALSE,
             cex.cor = 5,
             scale = TRUE,
             density = TRUE  # show density plots
)

#As we expected, BIC gets rid of the season variable & winDir
# 3 Steps, at first day (which in reality are two variables now) is removed and then winDir, improving the score of the AIC
summary(cleanModelBIC)
cleanModelBIC$anova

# As stated befor, from this result we can see that the variable that has a higher impact is carsHour, 
# and that the windSpeed reduces the number of particles, which makes sense
cleanModelBIC$coefficients

############################CHECKING MODEL ASSUMPTIONS#############################

plot(cleanModelBIC) ### This returns various graphs that might help seeing if the model assumptions are met, specially those concering errors
## Linearity -> Previously checked, also trigonometric relation between carHour & particles

## Error normality -> With the graphs we can check that the residual follow a a normal distribution
par(mfrow=c(1,2))
plot(cleanModelBIC,2)
plot(density(cleanModelBIC$residuals),main="")
lines(col="blue",x = density(rnorm(n = 10000,sd=sd(cleanModelBIC$residuals))))
dev.off()

## Homoscedasticity -> The variance is approximately constant and p-value lets us reject the null hypothesis
# of the variance not being constant
ncvTest(cleanModelBIC)
plot(cleanModelBIC,3)

## Independe -> Hard to check using graphs, so we apply DurbinWatsonTest
lag.plot(cleanModelBIC$residuals, lags = 1, do.lines = FALSE)
durbinWatsonTest(cleanModelBIC)

## Multicollinearity -> as interactions between variables are not easy to see, we apply Variance Inflation Factor
vif(cleanModelBIC)

############################CHECKING NON-LINEAR MODELS#############################

## BIC with first order interactions
modFirstOrder <- stepAIC(object = lm(particles ~ ., data = no2[-7]), scope = particles ~ .^2, k = log(length(particles)), trace = 0)
BIC(modFirstOrder)
summary(modFirstOrder)
# Model performance is not improved in a significant way applying higher order interactions and complexity gets higher, so it might not be worthy.

## BIC with second order interactions
carsHourSq <- poly(x = carsHour, degree = 2, raw = TRUE)
temp2Sq <- poly(x = temp2, degree = 2, raw = TRUE)
windSpeedSq <- poly(x = windSpeed, degree = 2, raw = TRUE)
tempDiff25to2Sq <- poly(x = tempDiff25to2, degree = 2, raw = TRUE)
no2Sq <- data.frame("particles" = particles,
                    "carsHour" = carsHourSq[,1], "carsHourSq" = carsHourSq[,2],
                    "temp2" = temp2Sq[,1], "temp2Sq" = temp2Sq[,2],
                    "windSpeed" = windSpeedSq[,1], "windSpeedSq" = windSpeedSq[,2],
                    "tempDiff25to2" = tempDiff25to2Sq[,1], "tempDiff25to2Sq" = tempDiff25to2Sq[,2])
modFirstOrderWithSq <- stepAIC(object = lm(particles ~ ., data = no2Sq), scope = particles ~ .^2, k = log(length(particles)), trace = 0)
BIC(modFirstOrderWithSq)
summary(modFirstOrderWithSq)
# Taking into account squared predictors does improve the BIC and the adjusted R^2. This model might be the best
# one if we were to carry out some supervised regression and our main objective were improving accuracy. Although
# it might also mean that we are overfitting. For the second report we shall explore more in depth this model.

############################RIDGE AND LASSO REGRESSION######################################
dev.off()

no2Matrix <- model.matrix.lm(particles ~ 0+carsHour+windSpeed+temp2+tempDiff25to2+no2$day+no2$windDir, na.action = "na.pass")
no2Matrix <- scale(no2Matrix)

ridgeMod <- glmnet(x = no2Matrix, y = particles, alpha = 0)
summary(ridgeMod)
# This graph shows the most important variables
plot(ridgeMod, xvar = "norm", label = TRUE)
# This graph shows the maximum adjusted R squared that can be measured
plot(ridgeMod, label = TRUE, xvar = "dev")

kcvRidge <- cv.glmnet(x = no2Matrix, y = particles, alpha = 0, nfolds = 10)
# Lambda that minimizes the error of the model
kcvRidge$lambda.min
# Minimum cross-validation error
min(kcvRidge$cvm)
# Lambda range (The minimum is in the minimum of the range)
range(kcvRidge$lambda)
lambdaGrid <- 10^seq(log10(kcvRidge$lambda[1]), log10(0.0001),
                     length.out = 250) # log-spaced grid
kcvRidge2 <- cv.glmnet(x = no2Matrix, y = particles, nfolds = 10, alpha = 0,
                       lambda = lambdaGrid)
plot(kcvRidge2)

# The glmnet fit is inside the output of cv.glmnet
modRidgeCV <- kcvRidge2$glmnet.fit

# Inspect the best models
plot(modRidgeCV, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvRidge2$lambda.min, kcvRidge2$lambda.1se)))

# Ridge model with lambda 1se
bestRidge <- glmnet(x = no2Matrix, y = particles, lambda = kcvRidge2$lambda.1se, alpha = 0)
bestRidge$beta

##### Lasso
lassoMod <- glmnet(x = no2Matrix, y = particles, alpha = 1)
plot(lassoMod, xvar = "lambda", label = TRUE)
plot(lassoMod, label = TRUE, xvar = "dev")

kcvLasso <- cv.glmnet(x = no2Matrix, y = particles, alpha = 1, nfolds = 10)
kcvLasso$lambda.min
kcvLasso$lambda.1se
# In this case the minimum is not at an extreme
range(kcvLasso$lambda)
plot(kcvLasso)

ncvLasso <- cv.glmnet(x = no2Matrix, y = particles, alpha = 1, nfolds = 12,lambda = lambdaGrid)
plot(ncvLasso)

modLassoCV <- kcvLasso$glmnet.fit
plot(modLassoCV, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvLasso$lambda.min, kcvLasso$lambda.1se)))

# Best Lasso model
bestLasso <- glmnet(x = no2Matrix, y = particles, alpha = 1, lambda = kcvLasso$lambda.1se)
bestLasso$beta
# As expected we should only take into account carsHour, temp2, windSpeed and tempDiff25to2 since they have the 
# highest coefficients both in Ridge and Lasso regression models

simpleBICClassification <- stepAIC(glm(I(particles > 3.85) ~ ., data = no2), k = log(length(particles)))
summary(simpleBICClassification)
yHat <- simpleBICClassification$fitted.values > 0.5
tab <- table(I(particles > 3.85), yHat)
tab
sum(diag(tab)) / sum(tab)
## Not worthy to take into consideration interactions
interactionsBICClassification <- stepAIC(glm(I(particles > 3.85) ~ .^2, data = no2), k = log(length(particles)))
summary(interactionsBICClassification)
yHat <- interactionsBICClassification$fitted.values > 0.5
tab <- table(I(particles > 3.85), yHat)
tab
sum(diag(tab)) / sum(tab)
## Squareing the variables improves the results, althouhg we need 10 predictors for this model
squaredBICClassidication <- stepAIC(glm(I(particles > 3.85) ~ .^2, data = no2Sq), k = log(length(particles)))
summary(squaredBICClassidication)
yHat <- squaredBICClassidication$fitted.values > 0.5
tab <- table(I(particles > 3.85), yHat)
tab
sum(diag(tab)) / sum(tab)

# 0 = Madrugada (0-6); 1 = Ma??ana (6-12); 2 = Tarde (12-18); 3 = Noche (18 - 24)
timeOfDay <- time
for (i in 1:length(time)) {
  if (time[i] >= 0 && time[i] < 6) {
    timeOfDay[i] <- 1
  }
  else if (time[i] >= 6 && time[i] < 12) {
    timeOfDay[i] <- 2
  }
  else if (time[i] >= 12 && time[i] < 18) {
    timeOfDay[i] <- 3
  }
  else if (time[i] >= 18) {
    timeOfDay[i] <- 4
  }
}

no2Aux <- data.frame("particles" = particles, "carsHour" = carsHour, "temp2" = temp2, "tempDiff25to2" = tempDiff25to2, "windSpeed" = windSpeed, "day" = no2$day, "windDir" = no2$windDir, "timeOfDay" = timeOfDay)

# Time of the day separates the amount of cars per hour which makes total sense.
plot(particles ~ carsHour, data = no2Aux, col = timeOfDay)
plot(particles ~ temp2, data = no2Aux, col = timeOfDay)
plot(particles ~ tempDiff25to2, data = no2Aux, col = timeOfDay)
plot(particles ~ windSpeed, data = no2Aux, col = timeOfDay)
plot(particles ~ carsHour, data = no2Aux, col = windDir+1) 
# Wind direction somewhat separates the temperature wich makes sense
plot(particles ~ temp2, data = no2Aux, col = windDir+1)
# There's a wind direction for which the variance of the temperature difference is smaller than for the other
plot(particles ~ tempDiff25to2, data = no2Aux, col = windDir+1)
# There's a direction that gets higher wind speeds but there's not too much of a difference
plot(particles ~ windSpeed, data = no2Aux, col = windDir+1)

## It could be interesting to see when the "PREAVISO" level is reached which is the value for which in Madrid we get "Escenario 1"
## The amount of NO2 needed to reach "PREAVISO" level is 180 mu g/m3
## From the original article: https://www.sciencedirect.com/science/article/pii/S135223100500021X#sec8 we know that
## the concentration of NO2 is measured in the same units as the "PREAVISO" level.
## Due to the small amount of observations measuring above this value we think it's going to be hard to train a good classifier
# DOES NOT WORK...
no2Real = data.frame("particles" = exp(particles), "carsHour" = carsHour, "temp2" = temp2, "tempDiff25to2" = tempDiff25to2, "windSpeed" = windSpeed, "windDir" = no2$windDir)

###
levelModel <- stepAIC(glm((particles > 180) ~ ., data = no2Real, family = "binomial"), k = log(length(particles)))
summary(levelModel)
plot(no2Real$particles>180 ~ tempDiff25to2)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2Real$particles > 180, yHat)
# It misspredicts all the observations above 180
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModel <- stepAIC(glm((particles > 120) ~ ., data = no2Real, family="binomial"), k = log(length(particles)))
summary(levelModel)
plot(no2Real$particles>120 ~ carsHour)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2Real$particles > 120, yHat)
# As for 180, it predicts all the variables as being below 120, once again, the model is NOT right
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModel <- stepAIC(glm((particles > 50) ~ ., data = no2Real, family = "binomial"), k = log(length(particles)))
summary(levelModel)
plot(no2Real$particles>50 ~ carsHour)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2Real$particles > 50, yHat)
# In this case it does a better job obtaining a 75% accuracy (WE COULD TRY SUPERVISED FOR THIS CASE)
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

## What if instead exponentiating, we take the logarithm of the barrier?
levelModel <- stepAIC(glm(particles > log(180) ~ ., data = no2, family = "binomial"), k=log(length(particles)))
summary(levelModel)
plot(no2$particles>log(180) ~ windSpeed)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2$particles > log(180), yHat)
# Same as in previous case
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModel <- stepAIC(glm(particles > log(120) ~ ., data = no2, family = "binomial"), k=log(length(particles)))
summary(levelModel)
plot(no2$particles>log(120) ~ windSpeed)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2$particles > log(120), yHat)
# Same as in previous case 
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModel <- stepAIC(glm(particles > log(50) ~ ., data = no2, family = "binomial"), k=log(length(particles)))
summary(levelModel)
plot(no2$particles>log(50) ~ windSpeed)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2$particles > log(50), yHat)
# Works better than when we exponentiate. Specially when looking at the true negative rate
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModel <- stepAIC(glm(particles > log(68) ~ ., data = no2, family = "binomial"), k=log(length(particles)))
summary(levelModel)
plot(no2$particles>log(68) ~ windSpeed)#, xlim = c(-100, 350))
x <- seq(-100, 350, l = 2000)
y <- exp(-(levelModel$coefficients[1] + levelModel$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

yHat <- levelModel$fitted.values > 0.5
tab <- table(no2$particles > log(68), yHat)
# By making it be higher than the third quartile the results improve
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

## What if we allow for interactions
levelModelInt <- stepAIC(glm(particles > log(180) ~ .^2, data = no2, family = "binomial"), k = log(length(particles)))
summary(levelModelInt)

yHat <- levelModelInt$fitted.values > 0.5
tab <- table(no2$particles > log(180), yHat)
# We get a perfect fit in this case, which means we might be overfitting. TEST-TRAIN SPLIT!!!!!
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModelInt <- stepAIC(glm(particles > log(120) ~ .^2, data = no2, family = "binomial"), k = log(length(particles)))
summary(levelModelInt)

yHat <- levelModelInt$fitted.values > 0.5
tab <- table(no2$particles > log(120), yHat)
# We obtain the same results as when we do not allow for interactions
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModelInt <- stepAIC(glm(particles > log(50) ~ .^2, data = no2, family = "binomial"), k = log(length(particles)))
summary(levelModelInt)

yHat <- levelModelInt$fitted.values > 0.5
tab <- table(no2$particles > log(50), yHat)
# We obtain worse results than when we do not allow for interactions
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr

###
levelModelInt <- stepAIC(glm(particles > log(68) ~ .^2, data = no2, family = "binomial"), k = log(length(particles)))
summary(levelModelInt)

yHat <- levelModelInt$fitted.values > 0.5
tab <- table(no2$particles > log(68), yHat)
# Pretty much the same results
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy
tpr <- tab[1]/(tab[1]+tab[2])
tpr
tnr <- tab[4]/(tab[3]+tab[4])
tnr



### Try to obtain the season with the rest of the predictors
summary(day)
warmCold <- day
for (i in 1:length(day)) {
  if ((day[i] >= 0 && day[i] <= 91) || (day[i] >= 366 && day[i] <= 457)) {
    warmCold[i] <- as.double(1)
  }
  else if ((day[i] >= 92 && day[i] <= 183) || (day[i] >= 458 && day[i] <= 549)) {
    warmCold[i] <- as.double(1)
  }
  else if ((day[i] >= 184 && day[i] <= 275) || (day[i] >= 549 && day[i] <= 608)) {
    warmCold[i] <- as.double(0)
  }
  else if (day[i] >= 276 && day[i] <= 365) {
    warmCold[i] <- as.double(0)
  }
}

no2WarmCold <- data.frame("particles" = particles, "carsHour" = carsHour, "temp2" = temp2, "tempDiff25to2" = tempDiff25to2, "windSpeed" = windSpeed, "windDir" = no2$windDir, "timeOfDay" = timeOfDay, "warmCold" = warmCold)
## Unfinished
seasonModel <- stepAIC(glm(warmCold ~ ., data = no2WarmCold, family = "binomial"), k = log(length(particles)))
summary(seasonModel)





######### TO DO:
# Poisson regression on seasons
# See what happens when we take the best model for the previous report and "convert" it into a logistic model
# Do a Train-Test Split for the model that might be overfitting
# Check the model correctness
