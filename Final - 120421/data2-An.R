setwd("D:/DS-HCMUS-K30/HP1_2020/MHHTK/FINAL/data")

library(MASS)
library(ggplot2)
library(dplyr)
library(broom)
library(car)
library(corrplot)

# loading data and explore
mydata <-read.csv("data2.csv")
head(mydata)

data2 <- subset(mydata, select = -c(id,date))
dim(data2)
## check missing data
NA_values=data.frame(no_of_na_values=colSums(is.na(data2))) 
head(NA_values,19)
## observe data
head(data2)
summary(data2)
str(data2)
hist(data2$price)

# Create variable log of house price
# data2 <- data2 %>%  mutate(log_price = log(price))
# Plot histogram of log of house price
# ggplot(data2, aes(x = log_price)) +   geom_histogram()

## Determining the association between variables
cor_data=data.frame(data2[,1:19])
correlation=cor(cor_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")

# Create full model
mod_full_1 = lm(price ~ ., data2) #full model
summary(mod_full_1)

## model selection with BIC criteria, stepwise backward
mod_BIC_1 <- MASS::stepAIC(mod_full_1, direction = "backward", k = log(nrow(data2)))
summary(mod_BIC_1)
mod_BIC_1$anova
par(mfrow=c(2,2))
plot(mod_BIC_1)

## transform PRICE
mod_2 = lm(log(price) ~ bedrooms + bathrooms + sqft_living + waterfront + view + 
             condition + grade + sqft_above + yr_built + yr_renovated + 
             zipcode + lat + long + sqft_living15 + sqft_lot15, data2)
summary(mod_2) 

mod_2_full = lm(log(price)~.,data2)
mod_BIC_2 <- MASS::stepAIC(mod_2_full, direction = "backward", k = log(nrow(data2)))
summary(mod_BIC_2)
mod_BIC_2$anova

vif(mod_BIC_2)
mmps(mod_BIC_2)
avPlots(mod_BIC_2)
par(mfrow=c(2,2))
plot(mod_BIC_2)
coef(mod_BIC_2)
