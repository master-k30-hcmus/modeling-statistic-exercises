setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Assignment 2 - 250121/Review/Bai-2")

# Bai 2

Y = c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
X1 = c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
X2 = c(4.0, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

data <- data.frame(Y, X1, X2)
print(data)

## Visualize:
plot(density(Y))
plot(data)
summary(data)

## Cau 1: Xay dung cac mo hinh Y~X1, Y~X2 va Y~X1+X2

### Mo hinh Y~X1:
model_Y_X1 = lm(Y ~ X1)
print(model_Y_X1)
### => Y = 3.523 + 6.036 * X1

### Mo hinh Y~X2:
model_Y_X2 = lm(Y ~ X2)
print(model_Y_X2)
### => Y = -36.37 + 17.46 * X2

### Mo hinh Y~X1+X2
model_Y_X1X2 = lm(Y ~ X1 + X2)
print(model_Y_X1X2)
### => Y = -30.081 + 4.905 * X1 + 11.072 * X2


## Cau 2: Xac dinh ti le phan tram bien thien cua bien phu thuoc cho tung mo hinh co the co

### Mo hinh Y~X1:
summary(model_Y_X1) # mo hinh 1 Y bien thien 69% boi X1

### Mo hinh Y~X2:
summary(model_Y_X2) # mo hinh 2 Y bien thien 45% boi X2

### Mo hinh Y~X1+X2:
summary(model_Y_X1X2) # mo hinh 2 Y bien thien 84.8% boi X1,X2

## Cau 3: Lap bang ANOVA cho mo hinh hoi tuy 2 bien X1,X2

anova(model_Y_X1X2)

## Cau 4: Kiem dinh cho gia thiet H0: B1=B2=0 voi muc y nghia alpha=0.05

n = length(Y)
p = 2

lm_sum = summary(model_Y_X1X2); lm_sum
F_obs = lm_sum$fstatistic["value"]
p_value = pf(F_obs, p, n - p - 1, lower.tail = FALSE); p_value
alpha = 0.05
if (p_value < alpha) {
  sprintf("Vi p_value = %s < alpha = %s", format(p_value), format(alpha))
  sprintf("Suy ra bac bo H0 voi muc y nghia %s.", format(alpha))
} else {
  sprintf("Vi p_value = %s < alpha = %s", format(p_value), format(alpha))
  sprintf("Chua du co so de bac bo H0 voi muc y nghia %s.", format(alpha))
}


## Cau 5: Xac dinh khoang tin cay B1 voi muc y nghia 5% voi mo hinh chi co bien la X1

confintB1 = confint(model_Y_X1)[2,]

sprintf("Khoang tin cay B1 voi mo hinh Y ~ X1 la [%s,%s]",
        format(confintB1[1]),
        format(confintB1[2]))

## Cau 6:
## Kiem dinh cho gia thuyet:
## H_0 : B2 = 0 hay Y = B0 + B1X1 + e
## H_1 : B2 != 0 hay Y = B0 + B1X1 + B2X2 + e

lm_anova = anova(model_Y_X1X2); lm_anova

SST = sum(lm_anova$`Sum Sq`); SST
SSR1 = lm_anova$`Sum Sq`[1]; SSR1
SSR2 = lm_anova$`Sum Sq`[2]; SSR2

SSE1 = SST - SSR1; SSE1
SSE2 = SST - SSR1 - SSR2; SSE2

F_obs = ((SSE1 - SSE2) / 1) / (SSE2 / (n - p - 1)); F_obs
F_stats = 5.1174 #F_{0.95}(1,9)
alpha = 0.05
if (F_obs > F_stats) {
  sprintf("Vi F_obs = %s > F_{%s}(1,9) = %s",
          format(F_obs),
          format(1 - alpha),
          format(F_stats))
  sprintf("Suy ra bac bo H0 voi muc y nghia %s.", format(alpha))
} else {
  sprintf("Vi F_obs = %s <= F_{%s}(1,9) = %s",
          format(F_obs),
          format(1 - alpha),
          format(F_stats))
  sprintf("Chua du co so de bac bo H0 voi muc y nghia %s.", format(alpha))
}

