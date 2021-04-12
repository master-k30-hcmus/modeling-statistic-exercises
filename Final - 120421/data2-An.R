setwd("D:/DS-HCMUS-K30/HP1_2020/MHHTK/FINAL/data")

library(MASS)
library(ggplot2)
library(plyr)
library(broom)
library(car)
library(corrplot)

## loading data and explore
mydata <-read.csv("data2.csv")
data2 <- subset(mydata, select = -c(id,date))
dim(data2)
head(data2)
vars = names(data2)[c(2:length(names(data2)))] #show variables' name
print(vars)

NA_values=data.frame(no_of_na_values=colSums(is.na(data2))) #check missing data
head(NA_values,19)

summary(data2)
str(data2)
hist(data2$price)

## Determining the association between variables
cor_data=data.frame(data2[,1:19])
correlation=cor(cor_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")

## Create full model
mod_full_1 = lm(price ~ ., data2) #full model
summary(mod_full_1)
coef(mod_full_1)
par(mfrow=c(2,2))
plot(mod_full_1)

## model selection with BIC criteria, stepwise backward
mod_BIC_1 <- MASS::stepAIC(mod_full_1, direction = "backward", k = log(nrow(data2)))
summary(mod_BIC_1)
mod_BIC_1$anova
par(mfrow=c(2,2))
plot(mod_BIC_1)

## transform PRICE
mod_full_2 = lm(log(price) ~ . , data2)
summary(mod_full_2) 
par(mfrow=c(2,2))
plot(mod_full_2)

## transform PRICE and remove BASEMENT AREA
data2_new <- subset(mydata, select = -c(id,date,sqft_basement)) #remove sqft_basement
mod_full_3 = lm(log(price) ~ . , data2_new)
summary(mod_full_3) 
coef(mod_full_3)
par(mfrow=c(2,2))
plot(mod_full_3)

#model selection with BIC criteria, stepwise backward
mod_BIC_2 <- MASS::stepAIC(mod_full_3, direction = "backward", k = log(nrow(data2_new)))
summary(mod_BIC_2)
mmps(mod_BIC_2)
avPlots(mod_BIC_2)