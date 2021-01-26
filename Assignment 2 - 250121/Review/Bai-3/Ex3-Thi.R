setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Assignment 2 - 250121/Review/Bai-3")

# Bai 3
y = c(12,14,10,16,14,19,21,19,21,16,19,21,25,21)
x1 = c(2,1,3,6,7,8,8,5,5,8,4,9,12,7)
x2 = c(45,43,43,47,42,41,32,33,41,38,32,31,35,29)
x3 = c(121,132,154,145,129,156,132,147,128,163,161,172,174,180)

## Cau 1, 2, 4

### Mo hinh y = B0 + B1x1 + B2x2
lm_fit = lm(y~x1+x2)
lm_sum = summary(lm_fit); lm_sum

B0 = lm_sum$coefficients["(Intercept)", "Estimate"]
B1 = lm_sum$coefficients["x1", "Estimate"]
B2 = lm_sum$coefficients["x2", "Estimate"]

lm_anova = anova(lm_fit); lm_anova
SSR = sum(lm_anova$`Sum Sq`[1:2])
SST = sum(lm_anova$`Sum Sq`)
R2 = SSR/SST

sprintf("Cau 1: Mo hinh y = B0 + B1x1 + B2x2")
sprintf("Cau 2: He so hoi quy: B0=%s, B1=%s, B2=%s", format(B0), format(B1), format(B2))
sprintf("Cau 4: He so xac dinh: R2=%s", format(R2))

### Mo hinh y' = B0' + B1'x1 + B3'x3
lm_fit = lm(y~x1+x3)
lm_sum = summary(lm_fit); lm_sum

B0 = lm_sum$coefficients["(Intercept)", "Estimate"]
B1 = lm_sum$coefficients["x1", "Estimate"]
B3 = lm_sum$coefficients["x3", "Estimate"]

lm_anova = anova(lm_fit); lm_anova
SSR = sum(lm_anova$`Sum Sq`[1:2])
SST = sum(lm_anova$`Sum Sq`)
R2 = SSR/SST

sprintf("Cau 1: Mo hinh y' = B0' + B1'x1 + B3'x3")
sprintf("Cau 2: He so hoi quy B0'=%s, B1'=%s, B3'=%s", format(B0), format(B1), format(B3))
sprintf("Cau 4: He so xac dinh: R2'=%s", format(R2))

### Mo hinh y'' = B0'' + B2''x2 + B3''x3
lm_fit = lm(y~x2+x3)
lm_sum = summary(lm_fit); lm_sum

B0 = lm_sum$coefficients["(Intercept)", "Estimate"]
B2 = lm_sum$coefficients["x2", "Estimate"]
B3 = lm_sum$coefficients["x3", "Estimate"]

lm_anova = anova(lm_fit); lm_anova
SSR = sum(lm_anova$`Sum Sq`[1:2])
SST = sum(lm_anova$`Sum Sq`)
R2 = SSR/SST

sprintf("Cau 1: Mo hinh y'' = B0'' + B2''x2 + B3''x3")
sprintf("Cau 2: He so hoi quy B0''=%s, B2''=%s, B3''=%s", format(B0), format(B2), format(B3))
sprintf("Cau 4: He so xac dinh: R2''=%s", format(R2))

## Cau 5

sprintf("Ta co cac he so xac dinh theo thu tu tang dan R2'' < R2' < R2")
sprintf("Vay voi R2, mo hinh y duoc giai thich tot nhat voi hai bien doc lap x1, x2")
