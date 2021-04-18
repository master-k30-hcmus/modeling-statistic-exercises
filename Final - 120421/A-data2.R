### loading library ###
library(MASS)
library(corrplot)
library(ggplot2)
library(stats)
library(plyr)
library(reshape)
library(gam)
library(dummies)
library(fastDummies)
library(car)
library("factoextra")
library(magrittr)
library(dplyr)


### loading data and exploration ###
setwd("D:/DS-HCMUS-K30/HP1_2020/MHHTK/FINAL/data")
bike = read.csv("SeoulBikeData.csv", header = TRUE)
head(bike)
dim(bike)
names(bike)
str(bike)
summary(bike)
NA_values=data.frame(no_of_na_values=colSums(is.na(bike))) #check missing data
head(NA_values,100)

bike <- subset(bike, select = -c(Date))
names(bike) <- c("count", "hour", "temp", "humidity", "wind", "visibility",
                 "dew", "solar", "rain", "snow","season", "holiday", 
                 "workingday")

## visualization ##

h <- hist(bike$count, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )
xfit <- seq(min(bike$count),max(bike$count), length = 50)
yfit <- dnorm(xfit, mean =mean(bike$count),sd=sd(bike$count))
yfit <- yfit*diff(h$mids[1:2])*length(bike$count)
lines(xfit,yfit, col='red', lwd= 3)

par(mfrow=c(1,1))
plot(bike$hour, bike$count ,type = 'h', col= 'blue', xlab = 'hour', ylab = 'Total Bike Rentals')

par(mfrow=c(2,2 ))
plot(bike$temp, bike$count ,type = 'h', col= 'blue', xlab = 'Temperature', ylab = 'Total Bike Rentals')
plot(bike$humidity, bike$count ,type = 'h', col= 'blue', xlab = 'Humidity', ylab = 'Total Bike Rentals')
plot(bike$wind, bike$count ,type = 'h', col= 'blue', xlab = 'Windspeed', ylab = 'Total Bike Rentals')
plot(bike$visibility, bike$count ,type = 'h', col= 'blue', xlab = 'visibility', ylab = 'Total Bike Rentals')
plot(bike$dew, bike$count ,type = 'h', col= 'blue', xlab = 'dew', ylab = 'Total Bike Rentals')
plot(bike$solar, bike$count ,type = 'h', col= 'blue', xlab = 'solar', ylab = 'Total Bike Rentals')
plot(bike$rain, bike$count ,type = 'h', col= 'blue', xlab = 'rain', ylab = 'Total Bike Rentals')
plot(bike$snow, bike$count ,type = 'h', col= 'blue', xlab = 'snow', ylab = 'Total Bike Rentals')

## Determining the association between variables
par(mfcol=c(2,2))
cor_data=data.frame(bike[,1:10])
correlation=cor(cor_data)
corrplot(correlation, method="number")

## create dummy variables
bike$season=as.factor(bike$season)
bike$holiday <- revalue(bike$holiday, c("Holiday" = 1,"No Holiday" = 0))
bike$holiday <- as.numeric(as.character(bike$holiday))
bike$workingday <- revalue(bike$workingday, c("No" = 0, "Yes" = 1))
bike$workingday <- as.numeric(as.character(bike$workingday))
head(bike)
str(bike)
table(bike$holiday)
table(bike$workingday)

boxplot(bike$count ~ bike$season,
        data = bike,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 

boxplot(bike$count ~ bike$holiday,
        data = bike,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1")) 

boxplot(bike$count ~ bike$workingday,
        data = bike,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Working day",
        ylab = "Total Bike Rentals",
        col = c("blue", "blue1")) 

# create dummy data frame
data_dum = dummy.data.frame(bike, names = c ("hour","season")) # "Seasons
str(data_dum)
dim(data_dum)
head(data_dum)

# Build linear model and check multicollinearity
mod1 <- lm(count ~ ., bike)
summary(mod1)
vif(mod1)

# Create PCA
X = subset(data_dum, select = -c(count))
X_count = subset(data_dum, select = c(count))

res.pca <- prcomp(X,  center = TRUE, scale = TRUE)

fviz_pca_var(res.pca, axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")    # Avoid text overlapping
             ,
             repel = TRUE )
summary(res.pca)

#----------------

std_dev <- res.pca$sdev
pr_var <- std_dev^2
#propotion of variance explained
prop_varex = pr_var/(sum(pr_var))
prop_varex
#scree plot
par(mfrow=c(1,2))
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

res.pca$rotation[,1:26]

pcamodel1 <- data.frame(X_count, res.pca$x[,1:26])
mod1 =lm(formula = pcamodel1$count~.,data = pcamodel1)
summary(mod1)

pcamodel2 =  subset(pcamodel1 , select = -c(PC7,PC13, PC14))
mod2 =lm(formula = pcamodel2$count~.,data = pcamodel2)
summary(mod2)

