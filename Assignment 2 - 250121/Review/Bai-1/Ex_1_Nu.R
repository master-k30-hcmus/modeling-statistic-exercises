#Ex_1:
SSR_X1 <- 981.316
df_X1 <- 1

SSR_X2 <- 190.232
df_X2 <- 1

SSR_X3 <- 129.431
df_X3 <- 1

SSE <- 442.292
df_SSE <- 18

SST<- 1743.281
df_SST <- 21

#1. Tong hoi quy tren X1,X2,X3
SSR<- SSR_X1 +SSR_X2+SSR_X3

print(SSR)


#2. Xac dinh ty le phan tram?su bien thien cua muc do lo lang dc giai thich boi bien doc lap/ Duoc do bang gia tri R square => 74.6% muc do lo lang
#duoc gia thich oi cac bien doc lap X1,X2,va X3

R_square <- SSR/SST
print(R_square)


#3. Chua the ket luan duoc ma phai di kiem dinh Y ?s tung bien doc lap.
#Kiem dinh Fisher duoc thuc hien de ket luan bien doc l???p nao anh huong toi bien phu thuoc.

#4.Lap bang ANOVA khi mo hinh chi xet bien giai thich X1

SSR_X1 <- 981.316
df_SSR_X1 <- 1
#-----
n <- 22
SSE_X1 <- SST - SSR_X1
df_SSE_X1 <- n-2
df_SSE_X1


#5.a Ho: B1 = 0 cho mo hinh Y = Bo + B1X1 +e
#
F_obs <- (SSR_?1/df_SSR_X1)/(SSE_X1/df_SSE_X1)
F_obs

# F(1,21,0.05/2) = 5.827 => Fobs > Fcritical => Bac bo Ho

#5.b H0 :B2 = 0 cho mô h??nh Y = b0 + B1X1 + B2X2 + e
# Gia thuyet: H0: B2 = 0/ H1: Ho ko dung

F_obs_2 <- ((SSE_X1-(SST - SSR_X1 - SSR_X2))/(20 - 19))/((SST - SSR_X1 - SSR_X2)/19)
print(F_obs_2)

#F_critical(2,19,0.025) = 4.508 => Bac bo H0

#5c. H0 : B3 = 0 cho mô h??nh Y = B0 + B1X1 + B2X2 + B3X3 + B4
#Gia thuyet: H0: B3 =0, H1: Ho sai
F_obs_3 <- (((SST - SSR_X3) - SSE)/(20-18))/(SSE/18)
F_obs_3
#F_critical(3,18,0.025) = 3.945 => Bac bo H0

#6. Xac dinh he so cho moi mo hinh cau 5

R_2_adj_X1 = 1 - ((SSR_X1/(21-1-1))/?SST/(21-1)))
R_2_adj_X1
#---

R_2_adj_X2 = 1 -(((SST - SSR_X3-SSE)/(21-2-1))/(SST/(21-1)))
R_2_adj_X2

#-----
R_2_adj_X3 = 1 - ((SSR/(21-3-1))/(SST/(21-1)))
R_2_adj_X3


# Cau 6: Trong 3 mo hinh tren mo hinh 1 co he so dieu chinh R^2 cao nhat => mo hinh to? nhat


