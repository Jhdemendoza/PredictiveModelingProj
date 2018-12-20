setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project1/")

library("psych")
library("MASS")
library("scatterplot3d")

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

## WindDir Solucionado
clusterWindDir <- kmeans(windDir,2)
#car::scatterplotMatrix(no2, col = 1, regLine = list(col = 2), smooth = list(col.smooth = 4, col.spread = 4))

mod <- lm(particles ~ ., data = no2)
modBIC <- stepAIC(mod, k=log(length(particles)))

hist(modBIC$residuals)

no2BIC <- no2[,c(1,2,3,4,5,7)]
head(no2BIC)

pairs.panels(no2BIC, 
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

summary(modBIC)

## particles vs carsHour (I could see a linear relation)
mod1 <- lm(particles ~ carsHour, data = no2)
summary(mod1)

scatterplot(particles ~ carsHour, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod1$coefficients, col = "red")
#abline(modBIC$coefficients[1:2], col = "green")

## particles vs temp2 (Cannot see a linear relation)
mod2 <- lm(particles ~ temp2, data = no2)
summary(mod2)

scatterplot(particles ~ temp2, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod2$coefficients, col = "red")

## particles vs windSpeed (There is a linear relation)
mod3 <- lm(particles ~ windSpeed)
summary(mod3)

scatterplot(particles ~ windSpeed, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod3$coefficients, col = "red")

## particles vs tempDiff25to2 (Cannot see a linear relation)
mod4 <- lm(particles ~ tempDiff25to2)
summary(mod4)

scatterplot(particles ~ tempDiff25to2, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod4$coefficients, col = "red")

## particles vs windDir (Cannot see a linear relation) -> Probably should divide this varibale into two groups.
mod5 <- lm(particles ~ windDir)
summary(mod5)

scatterplot(particles ~ windDir, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod5$coefficients, col = "red")

## particles vs time (This one might have sense just because the number of cars increases with the time)
mod6 <- lm(particles ~ time)
summary(mod6)

scatterplot(particles ~ time, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod6$coefficients, col = "red")

scatterplot(carsHour ~ time, col = 1, regLine = FALSE, smooth = FALSE)

## particles vs day (This model does not even make sense at all)
mod7 <- lm(particles ~ day)
summary(mod7)

scatterplot(particles, day, col = 1, regLine = FALSE, smooth = FALSE)
abline(mod7$coefficients, col = "red")

## TODO:
# Split WindDir in two groups
# Fix or get rid of day since it has a very weird shape
# Redo the multiple linear model with the fixed variables and only taking into account those that seem important
# Test that all model assumptions are met (i.e. error normality, etc)
# Try to fit a sin/cos regression function between carsHour and Time
# 1.- Descripcion general del dataset (Sacar estadisticos de cada variable)
# 2.- Preprocesado -> windDir y day y time
# 3.- Descripcion "Asi ha quedado el dataset"
# 4.- Probar modelo lineal (Comprobar que se cumplen las hipotesis del modelo lineal)
# 5.- Probar modelos no lineales (x^2+xy+y^2+x+y+intercept etc)
# 6.- Lasso y ridge regression