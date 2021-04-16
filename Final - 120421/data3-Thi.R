setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Final - 120421")

library(MASS)
library(ggplot2)
library(plyr)
library(broom)
library(car)
library(corrplot)

## loading data and explore
data3 = read.csv("data/data3.csv")
attach(data3)

head(data3)
dim(data3)
names(data3)

NA_values = data.frame(no_of_na_values = colSums(is.na(data3))) #check missing data
head(NA_values,100)

summary(data3)
str(data3)

ggplot(data = data3) + geom_histogram(aes(x = x_i.10), binwidth = .5) + labs(x = "Tong so lan duong (X10)", y = "So luong")
ggplot(data = data3) + geom_histogram(aes(x = x_i.4), binwidth = .5) + labs(x = "Toc do gioi han cho phep (X4)", y = "So luong")
ggplot(data = data3) + geom_histogram(aes(x = y_i), binwidth = 2) + labs(x = "Ty le % xay ra tai nan", y = "So luong")

aggregate(y_i ~ x_i.11 + x_i.12 + x_i.13,data3,mean)
aggregate(y_i ~ x_i.4,data3,mean)
aggregate(y_i ~ x_i.10,data3,mean)

## Determining the association between variables
cor_data = data.frame(data3)
correlation = cor(cor_data)
par(mfrow = c(1, 1))
corrplot(correlation,method = "color")

processModelSelection = function(model, data){
  par(mfrow = c(2,2))
  plot(model)

  ## model selection with BIC criteria, using Stepwise regression
  model_BIC = MASS::stepAIC(model, direction = "both", k = log(nrow(data)))
  par(mfrow = c(2,2))
  plot(model_BIC)
  mmps(model_BIC)
  avPlots(model_BIC)

  return(model_BIC)
}

## Create full model
mod_full = lm(y_i ~ ., data3)
summary(mod_full)
coef(mod_full)
vif(mod_full)

mod_BIC = processModelSelection(model = mod_full, data = data3)
summary(mod_BIC)
mod_BIC$anova
vif(mod_BIC)
coef(mod_BIC)

data_res = residuals(mod_BIC)
data_crit = 2*sd(data_res)
data_outlier = ifelse(abs(data_res > data_crit),1,0); data_outlier
data_outlier[data_outlier == 1]
new_data = data3[-c(26,27),]
dim(new_data)

new_mod_full = lm(y_i ~ ., new_data)
summary(mod_full)
coef(mod_full)
vif(mod_full)

new_mod_BIC = processModelSelection(model = new_mod_full, data = new_data)
summary(mod_BIC)
mod_BIC$anova
vif(mod_BIC)
coef(mod_BIC)
