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
library("factoextra")
library(magrittr)
library(dplyr)

setwd("D:/DS-HCMUS-K30/HP1_2020/MHHTK/FINAL/data")
bike = read.csv("SeoulBikeData.csv", header = TRUE)
head(bike)
dim(bike)
names(bike)
str(bike)
summary(bike)
NA_values=data.frame(no_of_na_values=colSums(is.na(bike))) #check missing data
head(NA_values,100)

#create dummy
bike <- subset(bike, select = -c(Date))
names(bike) <- c("count", "hour", "temp", "humidity", "wind", "visibility",
                 "dew", "solar", "rain", "snow","season", "holiday", 
                 "workingday")
bike$season=as.factor(bike$season)
bike$holiday=as.factor(bike$holiday)
bike$workingday=as.factor(bike$workingday)
str(bike)

bike$holiday <- revalue(bike$holiday, c("Holiday" = 1,"No Holiday" = 0))
bike$holiday <- as.numeric(as.character(bike$holiday))
bike$workingday <- revalue(bike$workingday, c("No" = 0, "Yes" = 1))
bike$workingday <- as.numeric(as.character(bike$workingday))
head(bike)
table(bike$holiday)
table(bike$workingday)

h <- hist(bike$count, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )
xfit <- seq(min(bike$count),max(bike$count), length = 50)
yfit <- dnorm(xfit, mean =mean(bike$count),sd=sd(bike$count))
yfit <- yfit*diff(h$mids[1:2])*length(bike$count)
lines(xfit,yfit, col='red', lwd= 3)

par(mfrow=c(1, 1))#divide graph area in 1 columns and 1 rows
boxplot(bike$count,main='Total_count',sub=paste(boxplot.stats(bike$count)$out))

par(mfcol=c(1,1))
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
par(mfcol=c(1,1))
cor_data=data.frame(bike[,2:11])
correlation=cor(cor_data)
corrplot(correlation, method="number")

data_dum = dummy.data.frame(bike, names = c ("hour","season")) # "Seasons
str(data_dum)
dim(data_dum)
library(car)
mod1 <- lm(count ~ ., data_dum)
summary(mod1)
vif(mod1)

plot(aggregate(count ~ hour,bike,mean))
X = subset(data_dum, select = -c(count))
X_count = subset(data_dum, select = c(count))

res.pca <- prcomp(X,  center = TRUE, scale = TRUE)
fviz_pca_var(res.pca, axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
summary(res.pca)
res.pca$rotation[,1:24]

pcamodel1 <- data.frame(X_count, res.pca$x[,1:24])
mod1 =lm(formula = pcamodel1$count~.,data = pcamodel1)
summary(mod1)

pcamodel2 =  subset(pcamodel1 , select = -c(PC7,PC13, PC14))
mod2 =lm(formula = pcamodel2$count~.,data = pcamodel2)
summary(mod2)

