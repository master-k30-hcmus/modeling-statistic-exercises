setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Assignment 2 - 250121/Review/Bai-1")

# Bai 1

n = 22
p = 3

SSR1 = 981.326
SSR2 = 190.232
SSR3 = 129.431

SSE = 442.292
SSE_df = n - p - 1

SST = SSR1 + SSR2 + SSR3 + SSE

## Cau 1: Tong binh phuong hoi quy tren X1, X2, X3
SSR = SSR1 + SSR2 + SSR3
sprintf("Tong binh phuong hoi quy tren X1, X2, X3 la %s", format(SSR))

## Cau 2: He so xac dinh cua Y (muc do lo lang)
R2 = SSR/SST
sprintf("Su bien thien cua muc do lo lang duoc giai thich boi cac bien doc lap co ti le la %s",format(R2))

## Cau 3: Dat gia thuyet H0: B1=B2=B3=0
sprintf("Dat gia thuyet H0: B1=B2=B3=0")
F_obs = (SSR/p)/(SSE/SSE_df); F_obs
p_value = pf(F_obs, p, n - p - 1, lower.tail = FALSE); p_value
sprintf("Vi p_value = %s < alpha = %s",format(p_value), format(0.05))
sprintf("Suy ra bac bo H0, ket luan ton tai it nhat mot bien anh huong den muc do lo lang.")

## Cau 4: Bang ANOVA neu chi xet bien giai thich X1
MSR1 = SSR1/1; MSR1

SSE1 = SST - SSR1; SSE1
MSE1 = SSE1/(n - 2); MSE1

F1 =  MSR1/MSE1; F1

cat(sprintf("|Bien thien\t|\tSS\t\t|\tDF\t|\tMS\t\t|\tFisher\t\t|\n"))
cat(sprintf("|R_X1\t\t|\t%s\t\t|\t%s\t|\t%s\t\t|\t\t\t|\n",format(SSR1), format(1),format(MSR1)))
cat(sprintf("|E_X1\t\t|\t%s\t\t|\t%s\t|\t%s\t|\t%s\t|\n",format(SSE1), format(n - 2), format(MSE1),format(F1)))
cat(sprintf("|Total\t\t|\t%s\t|\t%s\t|\n",format(SST), format(n - 1)))
