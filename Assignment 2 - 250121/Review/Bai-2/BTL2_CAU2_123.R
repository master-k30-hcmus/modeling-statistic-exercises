#Nhap truc tiep gia tri vao bang
Y<-c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
X1<-c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
X2<-c(4.0, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

#1,2
#phuong trinh duong thang hoi quy
Y1<-lm(Y~X1)
summary(Y1)

Y2<-lm(Y~X2)
summary(Y2)

#phuong trinh sieu phang hoi quy
Y3<-lm(Y~X1+X2)
summary(Y3)

#3
anova(lm(Y~X1+X2))

