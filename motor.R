# Module 7

setwd("C:/Users/hklau/Desktop/DataScienceClass/Module 7 - Regression Models/Assignment 1/")

# Answer: 
# https://rpubs.com/fabian/Reg_Models_Proj
# http://rstudio-pubs-static.s3.amazonaws.com/20376_91fae3fbc83042f7a2dc47f7540b7e5f.html
# https://rstudio-pubs-static.s3.amazonaws.com/26724_bf073fbc1f5c439887d3ea58e5824c60.html
# http://rstudio-pubs-static.s3.amazonaws.com/37680_64c25b37c21b4ff79e17d92a5fb751d5.html


# loading data
library(datasets)
data("mtcars")

# Set below variable as factor
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c('Automatic','Manual'))
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

# data summary
str(mtcars)
summary(mtcars)
table(mtcars$am, mtcars$am)

# linear regression
fit <- lm(mpg ~ ., data = mtcars)
summary(fit)

pairs(mtcars)


library(GGally)
library(ggplot2)
g = ggpairs(mtcars)
g

library(caret)
featurePlot(x = mtcars[names(mtcars)[-1]], y = mtcars[names(mtcars)[1]], plot = "pairs")

featurePlot(x = mtcars[names(mtcars)[2:6]], y = mtcars[names(mtcars)[1]], plot = "pairs")
featurePlot(x = mtcars[names(mtcars)[7:11]], y = mtcars[names(mtcars)[1]], plot = "pairs")


# linear regression for automatic vs manual car
fit_0 <- lm(mpg ~ ., data = mtcars)
summary(fit_0)

# Choose a model by AIC in a Stepwise Algorithm
fit_best <- step(fit_0, direction = "both")
summary(fit_best)

fit_1 <- lm(mpg ~ am, data = mtcars)
summary(fit_1)

fit_2 <- lm(mpg ~ am + wt, data = mtcars)
summary(fit_2)

fit_3 <- lm(mpg ~ am + wt + hp, data = mtcars)
summary(fit_3)

fit_4 <- lm(mpg ~ am + wt + hp + disp, data = mtcars)
summary(fit_4)

fit_5 <- lm(mpg ~ am + wt + hp + disp + cyl, data = mtcars)
summary(fit_5)

anova(fit_0, fit_best, fit_1, fit_2, fit_3, fit_4, fit_5)

par(mfrow = c(2, 2))
plot(fit_0)
plot(fit_1)

plot(x = mtcars$am, y = mtcars$mpg)


# Testing from Xiao Dan
fullModel <- lm(mpg ~ ., data=mtcars)
summary(fullModel) # results hidden

stepModel <- step(fullModel, k=log(nrow(mtcars)))
summary(stepModel) # results hidden

amIntWtModel<-lm(mpg ~ wt + qsec + am + wt:am, data=mtcars)
summary(amIntWtModel) # results hidden

amModel<-lm(mpg ~ am, data=mtcars)
summary(amModel) # results hidden

anova(amModel, stepModel, fullModel, amIntWtModel) 
confint(amIntWtModel) # results hidden




library(ggplot2)
g <- ggplot(data = mtcars, aes(x = am, y = mpg, colour = wt))
g <- g + geom_point(colour="grey50", size = 5)
g <- g + geom_smooth(method = lm, se = FALSE, colour = "red")
#g <- g + theme(legend.position="none") 
g <- g + facet_grid(.~cyl)
g <- g + geom_point(size = 4)
g

g <- ggplot(data = mtcars, aes(x = am, y = mpg, colour = wt))
g <- g + geom_point(colour="grey50", size = 5)
g <- g + geom_smooth(method = lm, se = FALSE, colour = "red")
#g <- g + theme(legend.position="none") 
g <- g + facet_grid(.~gear)
g <- g + geom_point(size = 4)
g

g <- ggplot(data = mtcars, aes(x = am, y = mpg, colour = wt))
g <- g + geom_point(colour="grey50", size = 5)
g <- g + geom_smooth(method = lm, se = FALSE, colour = "red")
#g <- g + theme(legend.position="none") 
g <- g + facet_grid(.~carb)
g <- g + geom_point(size = 4)
g

g <- ggplot(data = mtcars, aes(x = am, y = mpg, colour = wt))
g <- g + geom_point(colour="grey50", size = 5)
g <- g + geom_smooth(method = lm, se = FALSE, colour = "red")
#g <- g + theme(legend.position="none") 
g <- g + facet_grid(.~vs)
g <- g + geom_point(size = 4)
g





