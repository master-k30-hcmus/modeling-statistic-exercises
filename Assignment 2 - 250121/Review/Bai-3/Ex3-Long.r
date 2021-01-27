X1 <- c(2,1,3,6,7,8,8,5,5,8,4,9,12,7)
X2 <- c(45,43,43,47,42,41,32,33,41,38,32,31,35,29)
X3 <- c(121,132,154,145,129,156,132,147,128,163,161,172,174,180)
Y <- c(12,14,10,16,14,19,21,19,21,16,19,21,25,21)

##1
##Xem thu cac bien co doc lap tuyen tinh
library("PerformanceAnalytics")
data <- data.frame(X1, X2, X3)
chart.Correlation(data, histogram=TRUE, pch="+")


linearMod1 <- lm(Y ~ X1 + X2)
summary(linearMod1)$coefficient
linearMod2 <- lm(Y ~ X1 + X3)
summary(linearMod2)$coefficient
linearMod3 <- lm(Y ~ X2 + X3)
summary(linearMod3)$coefficient

## 3
confint(linearMod1, level=0.95)

## 4
Rsquare_1 = summary(linearMod1)$r.squared
Rsquare_2 = summary(linearMod2)$r.squared
Rsquare_3 = summary(linearMod3)$r.squared

## 5
if (max(Rsquare_1,Rsquare_2,Rsquare_3) == Rsquare_1){
  print("Mo hinh voi 2 bien doc lap X1 va X2 la tot nhat")
} else if(max(Rsquare_1,Rsquare_2,Rsquare_3) == Rsquare_2){
  print("Mo hinh voi 2 bien doc lap X1 va X3 la tot nhat")
} else {
  print("Mo hinh voi 2 bien doc lap X2 va X3 tot nhat") 
}

## 6

## 7
linearMod4 <- lm(Y ~ X1 + X2 + X3)
summary(linearMod4)$coefficients

## 8
s = sqrt(sum(linearMod4$residuals^2)/14-3)
var_epsilon = diag(3)*s
X <- matrix(c(X1,X2,X3), nrow=14)
var_beta = var_epsilon*solve(t(X)%*%X)

## 9
(upper = s*(14-3)/qchisq(0.025,14-3))
(lower = s*(14-3)/qchisq(1-0.025,14-3))


## 10
linearMod5 <- lm(Y ~ X1)
if(summary(linearMod5)$adj.r.squared < summary(linearMod4)$adj.r.squared){
  print("Mo hinh khi them bien x2 va x3 tot hon x1")
}
