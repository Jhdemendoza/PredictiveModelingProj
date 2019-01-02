setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project1/")
setwd("C:/Users/alvaro/Desktop/Segundo_Semicuatrimestre/PredictiveModelling/Group_project")

library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")
dev.off()

no2 = read.csv("NO2.csv ", col.names = c("particles", "carsHour", "temp2", "windSpeed", "tempDiff25to2", "windDir", "time", "day"))
attach(no2)
head(no2)

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
## Clean day by season. The day variable is measured as number of days from October 1. 2001
## October, November, December --> Autumn (2) 0-91; 366-457
## January, February, March --> Winter (3) 92-183; 458-549
## April, May, June --> Spring (0) 184-275; 549-608
## July, August, September --> Summer (1) 276-365
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
head(seasons)
no2$day <- as.factor(seasons)
no2$day <- relevel(no2$day, ref = "Spring")

## WindDir Solucionado
clusterWindDir <- kmeans(windDir,2)$cluster
no2$windDir<-as.factor(clusterWindDir)

#Ploting dividing by factors:
pairs(no2[c(sapply(no2,class)!="factor")],col=no2$windDir)
pairs(no2[c(sapply(no2,class)!="factor")],col=no2$day)

#END OF FIXING THE VARIABLES#


############################# MODELS ##################################

mod <- lm(particles ~ ., data = no2[,c(1,2,3,4,5,6,8)])
summary(mod)
modBIC <- stepAIC(mod, k=log(length(particles)))
cleanModel <- lm(no2[,c(1,2,3,4,5,6,8)])
cleanModelBIC <- stepAIC(cleanModel, k = log(length(particles)))
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
# 3 Steps, at first day is removed and then winDir, improving the score of the AIC
summary(cleanModelBIC)
cleanModelBIC$anova

# From this result we can realize that the variable that has a higher impact is carsHour, 
#and that the windSpeed reduces the number of particles, which makes sense
cleanModelBIC$coefficients

## Check model assuptions
plot(cleanModelBIC) ### This returns various graphs that might help seeing if the model assumptions are met
##### Linearity -> Difficult to check, but we've been able to prove the trigonometric relation between
##### carsHour and time and we have got rid of it.

##### Error normality -> With the two graphs we can check that the residual errors are almos normal.
plot(density(cleanModelBIC$residuals),col="red")
lines(col="blue",x = density(rnorm(n = 10000,sd=sd(cleanModelBIC$residuals))))
## The residual errors are "normal enough"

##### Homoscedasticity -> The variance is approximately constant
ncvTest(cleanModelBIC)
## Don't know how to check for independence, we might have to "see" it from the graph

## Non-linear models
### BIC with first order interactions
modFirstOrder <- stepAIC(object = lm(particles ~ ., data = no2), scope = particles ~ .^2, k = log(length(particles)), trace = 0)
BIC(modFirstOrder)
summary(modFirstOrder)
## Mejora el modelo al tener en cuenta interacciones de primer orden
## Modelo teniendo en cuenta cuadrados tambien, aunque no aumenta demasiado el adjusted R^2
## asi que yo creo que no merece la pena utilizar los cuadrados de las variables. Ademas el BIC no se reduce mucho
summary(no2)
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

###################### ANALYZING LINEARITY PREDICTORS VS RESPONSE ###################
models<-function(tobepredict="temp2",predictor="particles",data=no2){
  temp_1<-sapply(no2[names(no2)==tobepredict],as.numeric)
  temp_2<-sapply(no2[names(no2)==predictor],as.numeric)
  mod_temp <- lm(temp_1 ~ temp_2, data = no2)
  print(summary(mod_temp))
  plot(temp_2,temp_1,xlab = predictor,ylab=tobepredict)
  abline(coef=mod_temp$coefficients, col = "red")
  plot(density(mod_temp$residuals))
  lines(density(rnorm(n=10000,sd=sd(mod_temp$residuals))),col="blue")
  return(mod_temp)
}


## particles vs carsHour (I could see a linear relation)
# From this analysis we can conclude that we can fix both variables using
# the other cause it fits well a linear regression model
models("particles","carsHour")

mod1 <- lm(particles ~ carsHour, data = no2)
summary(mod1)
plot(carsHour,particles)
# I like this graph better
scatterplot(particles ~ carsHour, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod1$coefficients, col = "red")

#abline(modBIC$coefficients[1:2], col = "green")

##### particles VS temp2 (Cannot see a linear relation)

models("particles","temp2")
#Negative skewed - not large
skew(models("particles","temp2")$residuals)

mod2 <- lm(particles ~ temp2, data = no2)
summary(mod2)
scatterplot(particles ~ temp2, col = 1, regLine = F, smooth = FALSE)
abline(mod2$coefficients, col = "red")

## Particles vs windSpeed (There is a linear relation)
# Ok, residuals follow a normal distribution (mean = 0.0916 could be assumed to be 0)
models("particles","windSpeed")

## particles VS windSpeed (There is a linear relation)
models("particles","windSpeed")
mod3 <- lm(particles ~ windSpeed)
summary(mod3)
scatterplot(particles ~ windSpeed, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod3$coefficients, col = "red")

## particles vs tempDiff25to2 (Not linear relation)
models("particles","tempDiff25to2")
#Most of the points close to 0 -> low variance of the predictor, not useful 
models("tempDiff25to2","particles")
mod4 <- lm(particles ~ tempDiff25to2)
summary(mod4)

scatterplot(particles ~ tempDiff25to2, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod4$coefficients, col = "red")

# particles VS windDir
## WinDir transformed into binary variable, there is no relation!
plot(particles,col=clusterWindDir,pch=19,cex=0.8)

## particles vs time (This one might have sense just because the number of cars increases with the time)
## We are going to try to prove that time has a trigonometric relation with carsHour
summary(time)
per <- max(time)
plot(carsHour~time)
timePlot <- linspace(1, 24, 1000)
lines(6.2 - 2*sin((2*pi/24)*timePlot+0.5)~timePlot)
cl=c(1:24)
for (i in 1:24){
  l[i]<-mean(no2$carsHour[time==i])
  print(l[i])
}
points(l,col="red",pch=19,cex=1.3)
## Difficult to fit a model, with this I think we can show the trigonometric relation between time and carsHour
## which is enough to not use it

# Here there is no linear relation, we have already tried to fit a wave
models("particles","time")
mod6 <- lm(particles ~ time)
summary(mod6)
scatterplot(particles ~ time, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod6$coefficients, col = "red")
scatterplot(carsHour ~ time, col = 1, regLine = FALSE, smooth = FALSE)

## particles vs day. Already fixed the variable. 
# No insights from the plot
plot(particles, col=no2$day)

################################### DESCRIPTIVE ANALYSIS######################
dev.off()
head(no2)

str(no2)

box_plot<-function(data){
  par(mfrow=c(2,4))
  for (i in 1:length(data)){
    boxplot(data[,i],main=names(data)[i])
    text(x = 1.4,labels = round(boxplot.stats(data[,i])$stats,1),y = boxplot.stats(data[,i])$stats)
  }
}
box_plot(no2[,sapply(no2,class)!="factor"])

variable_analysis<-function(data,chart_name="chart_name"){
  par(mfcol=c(1,2),oma=c(0,0,2,0))
  
  boxplot(data,main=names(data),title="Boxplot")
  text(x = 1.4,labels = round(boxplot.stats(data)$stats,1),y = boxplot.stats(data)$stats)
  
  #boxplot(scale(data),main=names(data))
  #text(x = 1.4,labels = round(boxplot.stats(scale(data))$stats,1),y = boxplot.stats(scale(data))$stats)
  
  plot(density(boxplot.stats((data))$stats),main = "",xlab = "")  
  abline()
  mtext(chart_name,outer = T,cex=2)
}

#To be repeated with all variables (Not for windDir and day because they are categorical)
variable_analysis(carsHour,"Cars per Hour")
variable_analysis(particles, "Particles")
variable_analysis(temp2, "Temperature at 2 meters")
variable_analysis(windSpeed, "Wind Speed")
variable_analysis(tempDiff25to2, "Difference in Temperature")
variable_analysis(time, "Time of the day")

############################################# NEW ##########################################
### Ridge and Lasso Regression

summary(no2)
## 
no2Matrix <- model.matrix.lm(particles ~ 0+., data = no2[,c(1,2,3,4,5,6,8)], na.action = "na.pass")

ridgeMod <- glmnet(x = no2Matrix, y = particles, alpha = 0)
summary(ridgeMod)
# This graph shows the most important variables
plot(ridgeMod, xvar = "norm", label = TRUE)
# This graph shows the maximum adjusted R squared that can be measured
plot(ridgeMod, label = TRUE, xvar = "dev")

kcvRidge <- cv.glmnet(x = no2Matrix, y = particles, alpha = 0, nfolds = 10)
# Lambda that minimizes the cross-validation error
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

ncvLasso <- cv.glmnet(x = no2Matrix, y = particles, alpha = 1, nfolds = 10,lambda = lambdaGrid)
plot(ncvLasso)

modLassoCV <- kcvLasso$glmnet.fit
plot(modLassoCV, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvLasso$lambda.min, kcvLasso$lambda.1se)))

# Best Lasso model
bestLasso <- glmnet(x = no2Matrix, y = particles, alpha = 1, lambda = kcvLasso$lambda.1se)
bestLasso$beta
# As expected we should only take into account carsHour, temp2, windSpeed and tempDiff25to2 since it has the highest betas
# both in ridge and Lasso regression models

## TODO:
# Split WindDir in two groups *****DONE*****
# Fix or get rid of day since it has a very weird shape ****DONE****
# Redo the multiple linear model with the fixed variables and only taking into account those that seem important (stepAIC)*** DONE***
# Test that all model assumptions are met (i.e. error normality, etc)
# Try to fit a sin/cos regression function between carsHour and Time ****DONE****
# 1.- Descripcion general del dataset (Sacar estadisticos de cada variable) ******DONE*****
# 2.- Preprocesado -> windDir y day y time ****************DONE*****************
# 3.- Descripcion "Asi ha quedado el dataset" ***** Almost Done ****** 
# 4.- Probar modelo lineal (Comprobar que se cumplen las hipotesis del modelo lineal) ******** DONE *******
# 5.- Probar modelos no lineales (x^2+xy+y^2+x+y+intercept etc) ******** DONE *******
# 6.- Lasso y ridge regression ****** DONE *******

######################Descripcion General Dataset############



