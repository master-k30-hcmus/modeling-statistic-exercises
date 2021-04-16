setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Final - 120421")

library(MASS)
library(ggplot2)
library(plyr)
library(broom)
library(car)
library(corrplot)

## loading data and explore
data3 <-read.csv("data/data3.csv")
attach(data3)

head(data3)
dim(data3)
names(data3)

NA_values=data.frame(no_of_na_values=colSums(is.na(data3))) #check missing data
head(NA_values,100)

summary(data3)
str(data3)

ggplot(data=data3) + geom_histogram(aes(x=x_i.10), binwidth = .5) + labs(x = "Tong so lan duong (X10)", y = "So luong")
ggplot(data=data3) + geom_histogram(aes(x=x_i.4), binwidth = .5) + labs(x = "Toc do gioi han cho phep (X4)", y = "So luong")
ggplot(data=data3) + geom_histogram(aes(x=y_i), binwidth=2) + labs(x = "Ty le % xay ra tai nan", y = "So luong")

aggregate(y_i ~ x_i.11 + x_i.12 + x_i.13,data3,mean)
aggregate(y_i ~ x_i.4,data3,mean)
aggregate(y_i ~ x_i.10,data3,mean)


## Determining the association between variables
cor_data=data.frame(data3)
correlation=cor(cor_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")

## Create full model
mod_full = lm(y_i ~ ., data3) #full model
summary(mod_full)
coef(mod_full)
par(mfrow=c(2,2))
plot(mod_full)
vif(mod_full)

## model selection with BIC criteria, stepwise
mod_BIC <- MASS::stepAIC(mod_full, direction = "both", k = log(nrow(data3)))
summary(mod_BIC)
mod_BIC$anova
par(mfrow=c(2,2))
plot(mod_BIC)

vif(mod_BIC)
mmps(mod_BIC)
avPlots(mod_BIC)
coef(mod_BIC)
