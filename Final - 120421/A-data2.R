### loading library ###
library(MASS)
library(ggplot2)
library(plyr)
library(reshape)
library(gam)
library(corrplot)
library(dummies)
library(fastDummies)
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
bike$Day = as.numeric(substr(bike$Date,1,2))
bike$Month = as.numeric(substr(bike$Date,4,5))
names(bike) <- c("date", "count", "hour", "temp", "humidity", "wind", "visibility",
                 "dew", "solar", "rain", "snow","season", "holiday", 
                 "workingday", "day", "month")
head(bike)

bike$season=as.factor(bike$season)
bike$holiday=as.factor(bike$holiday)
bike$workingday=as.factor(bike$workingday)
str(bike)

bike$season <- revalue(bike$season, c("Spring" = 1, 
                                      "Summer" = 2,
                                      "Autumn" = 3,
                                      "Winter" = 4))
bike$season <- as.numeric(as.character(bike$season))

bike$holiday <- revalue(bike$holiday, c("Holiday" = 1,"No Holiday" = 0))
bike$holiday <- as.numeric(as.character(bike$holiday))
bike$workingday <- revalue(bike$workingday, c("No" = 0, "Yes" = 1))
bike$workingday <- as.numeric(as.character(bike$workingday))
head(bike)

table(bike$holiday)
table(bike$workingday)

#aggregate(bike$Count ~ bike$Temp,bike,mean)
#aggregate(bike$Count ~ bike$Humidity,bike,mean)
#aggregate(bike$Count ~ bike$Wind,bike,mean)

h <- hist(bike$count, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )

xfit <- seq(min(bike$count),max(bike$count), length = 50)
yfit <- dnorm(xfit, mean =mean(bike$count),sd=sd(bike$count))
yfit <- yfit*diff(h$mids[1:2])*length(bike$count)
lines(xfit,yfit, col='red', lwd= 3)

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
par(mfrow=c(1,1))
plot(bike$temp, bike$count ,type = 'h', col= 'yellow', xlab = 'Temperature', ylab = 'Total Bike Rentals')
plot(bike$humidity, bike$count ,type = 'h', col= 'yellow', xlab = 'Humidity', ylab = 'Total Bike Rentals')
plot(bike$wind, bike$count ,type = 'h', col= 'yellow', xlab = 'Windspeed', ylab = 'Total Bike Rentals')
plot(bike$visibility, bike$count ,type = 'h', col= 'yellow', xlab = 'visibility', ylab = 'Total Bike Rentals')
plot(bike$dew, bike$count ,type = 'h', col= 'yellow', xlab = 'dew', ylab = 'Total Bike Rentals')
plot(bike$solar, bike$count ,type = 'h', col= 'yellow', xlab = 'solar', ylab = 'Total Bike Rentals')
plot(bike$rain, bike$count ,type = 'h', col= 'yellow', xlab = 'rain', ylab = 'Total Bike Rentals')
plot(bike$snow, bike$count ,type = 'h', col= 'yellow', xlab = 'snow', ylab = 'Total Bike Rentals')

## Determining the association between variables
cor_data=data.frame(bike[,2:11])
correlation=cor(cor_data)
corrplot(correlation, method="number")


## PCA
data <- subset(bike, select = -c(date,month))
pca = princomp(data, fix_sign = TRUE)
summary(pca)
plot(pca, type = "l")
pcaStd =
biplot(pcaStd, choices = c(1,3))
biplot(pcaStd, choices = c(1,3))
library("factoextra")
res.pca <- prcomp(data, scale = TRUE)
summary(res.pca)
fviz_pca_var(res.pca, axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
pca_res = princomp(data, center = TRUE,scale. = TRUE)
summary(pca_res)
