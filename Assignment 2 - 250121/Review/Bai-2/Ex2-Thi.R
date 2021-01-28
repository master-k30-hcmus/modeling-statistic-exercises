setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Assignment 2 - 250121/Review/Bai-2")

# Bai 2

Y = c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
X1 = c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
X2 = c(4.0, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

## Cau 3: Bang ANOVA cua cho mo hinh hoi quy 2 bien la

lm_fit = lm(Y ~ X1 + X2)
anova(lm_fit)

## Cau 4: Kiem dinh cho gia thiet H0: B1=B2=0 voi muc y nghia alpha=0.05

n = length(Y)
p = 2

lm_fit = lm(Y ~ X1 + X2)
lm_sum = summary(lm_fit); lm_sum
F_obs = lm_sum$fstatistic["value"]
p_value = pf(F_obs, p, n - p - 1, lower.tail = FALSE); p_value

sprintf("Vi p_value = %s < alpha = %s", format(p_value), format(0.05))
sprintf("Suy ra bac bo H0 voi muc y nghia 0.05.")

## Cau 5: Xac dinh khoang tin cay B1 voi muc y nghia 5% voi mo hinh chi co bien la X1
lmX1_fit = lm(Y ~ X1)

confintB1 = confint(lmX1_fit)[2, ]; confintB1

sprintf("Khoang tin cay B1 voi mo hinh Y ~ X1 la [%s,%s]",
        format(confintB1[1]),
        format(confintB1[2]))

## Cau 6:
## Kiem dinh cho gia thuyet:
## H_0 : B2 = 0 hay Y = B0 + B1X1 + e
## H_1 : B2 != 0 hay Y = B0 + B1X1 + B2X2 + e

lm_anova = anova(lm_fit); lm_anova

SST = sum(lm_anova$`Sum Sq`); SST
SSR1 = lm_anova$`Sum Sq`[1]; SSR1
SSR2 = lm_anova$`Sum Sq`[2]; SSR2

SSE1 = SST - SSR1; SSE1
SSE2 = SST - SSR1 - SSR2; SSE2

F_obs = ((SSE1 - SSE2) / 1) / (SSE2 / (n - p - 1)); F_obs

sprintf("Vi F_obs = %s < F_{0.95}(1,9) = %s", format(F_obs), format(5.1174))
sprintf("Suy ra bac bo H0 voi muc y nghia 0.05.")
