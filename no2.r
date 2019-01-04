##################################Importing necessary libraries ###########
library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")

##################################Loading the dataset##################################
no2 = read.csv("NO2.csv ", col.names = c("particles", "carsHour", "temp2", "windSpeed", "tempDiff25to2", "windDir", "time", "day"))
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

summary(day)
seasons <- day
for (i in 1:length(day)) {
  if ((day[i] >= 0 && day[i] <= 91) || (day[i] >= 366 && day[i] <= 457)) {
    seasons[i] <- "Autumn"
  }
  else if ((day[i] >= 92 && day[i] <= 183) || (day[i] >= 458 && day[i] <= 549)) {
    seasons[i] <- "Winter"
  }
  else if ((day[i] >= 184 && day[i] <= 275) || (day[i] >= 549 && day[i] <= 608)) {
    seasons[i] <- "Spring"
  }
  else if (day[i] >= 276 && day[i] <= 365) {
    seasons[i] <- "Summer"
  }
}

no2$day <- as.factor(seasons)
no2$day <- relevel(no2$day, ref = "Spring")

## Fixing wind direction by splitting it into two clusters since there seems to be two clearly different wind directions
clusterWindDir <- kmeans(windDir,2)$cluster
no2$windDir<-as.factor(clusterWindDir)

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
