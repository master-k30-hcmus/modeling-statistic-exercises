setwd("D:/DS-HCMUS-K30/HP1_2020/MHHTK/FINAL/data")

library(MASS)
library(ggplot2)
library(dplyr)
library(broom)
library(car)
library(corrplot)
library(readxl)
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

# Split into training and testing set with ratio 80:20 (Stable size)
k_train = round(nrow(data2)*0.8,0)
k_train
train = data2[1:k_train,]
test = data2[(k_train + 1):nrow(data2), ]

nrow(train)
nrow(test)
names(train)
str(train)

# Create full model
mod_full_1 = lm(price ~ ., train) #full model
summary(mod_full_1)

## model selection with BIC criteria, stepwise backward
mod_BIC_1 <- MASS::stepAIC(mod_full_1, direction = "backward", k = log(nrow(train)))
summary(mod_BIC_1)
mod_BIC_1$anova
par(mfrow=c(2,2))
plot(mod_BIC_1)

## transform PRICE
mod_2 = lm(log(price) ~ bedrooms + bathrooms + sqft_living + waterfront + view + 
             condition + grade + sqft_above + yr_built + yr_renovated + 
             zipcode + lat + long + sqft_living15 + sqft_lot15, train)
summary(mod_2) 

mod_2_full = lm(log(price)~.,data2)
mod_BIC_2 <- MASS::stepAIC(mod_2_full, direction = "backward", k = log(nrow(train)))
summary(mod_BIC_2)
mod_BIC_2$anova

vif(mod_BIC_2)
mmps(mod_BIC_2)
avPlots(mod_BIC_2)
par(mfrow=c(2,2))
plot(mod_BIC_2)
coef(mod_BIC_2)


#Predict for test set with final model:
X_test = subset(test, select = -c(price))
y_test = test[c("price")]
head(X_test)
head(y_test)
pred_test = predict(mod_BIC_2, X_test)
pred_test
y_pred = exp(pred_test) #convert do model lay log(price)
SE = sum((y_pred-y_test) ^2)
RMSE = sqrt(SE/nrow(test))
print(RMSE)

