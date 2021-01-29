## Tao bo du lieu
X1 <- c(2, 1, 3, 6, 7, 8, 8, 5, 5, 8, 4, 9, 12, 7)
X2 <- c(45, 43, 43, 47, 42, 41, 32, 33, 41, 38, 32, 31, 35, 29)
X3 <- c(121, 132, 154, 145, 129, 156, 132, 147, 128, 163, 161, 172, 174, 180)
Y <- c(12, 14, 10, 16, 14, 19, 21, 19, 21, 16, 19, 21, 25, 21)

n <- length(Y)
p <- 3

## 1
##Xem thu cac bien co doc lap tuyen tinh
library("PerformanceAnalytics")
data <- data.frame(X1, X2, X3)
chart.Correlation(data, histogram=TRUE, pch="+")

## Cac mo hinh tuyen tinh voi 2 bien doc lap
linearMod3 <- lm(Y ~ X1 + X2)
summary(linearMod3)$coefficient
linearMod2 <- lm(Y ~ X1 + X3)
summary(linearMod2)$coefficient
linearMod1 <- lm(Y ~ X2 + X3)
summary(linearMod1)$coefficient

## 3
confint(linearMod3, level=0.95)

## 4
Rsquared_3 = summary(linearMod3)$r.squared; Rsquared_3
Rsquared_2 = summary(linearMod2)$r.squared; Rsquared_2
Rsquared_1 = summary(linearMod1)$r.squared; Rsquared_1

## 5
if (max(Rsquared_1, Rsquared_2, Rsquared_3) == Rsquared_3) {
  print("Mo hinh voi 2 bien doc lap X1 va X2 la tot nhat")
} else if (max(Rsquared_1, Rsquared_2, Rsquared_3) == Rsquared_2) {
  print("Mo hinh voi 2 bien doc lap X1 va X3 la tot nhat")
} else {
  print("Mo hinh voi 2 bien doc lap X2 va X3 tot nhat")
}

## 7
linearModFull <- lm(Y ~ X1 + X2 + X3)
summary(linearModFull)$coefficients
anova(linearModFull)

## 8
SD_hat = sqrt(sum(linearModFull$residuals ^ 2) / (n - (p + 1)))
var_epsilon = diag(p + 1) * (SD_hat ^ 2)
X <- matrix(c(rep(1, n), X1, X2, X3), nrow = 14)
var_beta = var_epsilon * solve(t(X) %*% X)

## 9
alpha = 0.05
(upper = (SD_hat ^ 2) * (n - (p + 1)) / qchisq(alpha / 2, n - (p + 1)))
(lower = (SD_hat ^ 2) * (n - (p + 1)) / qchisq(1 - alpha / 2, n - (p + 1)))

## 10
linearMod_X1 <- lm(Y ~ X1)
if(summary(linearMod_X1)$adj.r.squared < summary(linearModFull)$adj.r.squared) {
  print("Khi them 2 bien doc lap X3 và X2 vao mo hinh thi chat luong mo hinh tot hon")
} else {
  print("Khi them 2 bien doc lap X3 va X2 vao mo hinh thi chat luong khong tot hon")
}
