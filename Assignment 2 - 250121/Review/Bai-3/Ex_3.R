y <- c(12,14,10,16,14,19,21,19,21,16,19,21,25,21)
x1 <- c(2,1,3,6,7,8,8,5,5,8,4,9,12,7)
x2<- c(45,43,43,47,42,41,32,33,41,38,32,31,35,29)
x3<- c(121,132,154,145,129,156,132,147,128,163,161,172,174,180)

data <- data.frame(y,x1,x2,x3)
print(data)

model = lm(y~x1+x2+x3)

summary(model)

# 7. Uoc luong he so hoi quy trong mo hinh y = 32.8 + 0.8X1 - 0.38X2 - 0.03X3

#8.a Uoc luong variance(e): 0.67
anova(model)

SSE = 6.7
df = 10
v_e = SSE/df
v_e

#8.b Uoc luong V(b) = .257


SSR <- (0.29844  + 0.15658  + 0.05202)
V_b = SSR^2
V_b

#9. Voi do tin cay 95%, tim khoang tin cay cho V(e) 

#t(0.25,12-4) = 

#CI_v(e) = v(e)+-t*s(e) 


#10 Them 2 bien doc lap x3 va x2 vao mo hinh chi co 1 bien doc lap x1 thi lam cho chat luong
#uoc luong cao hon khong---bang ANOVA 2 bien X2,3 và X1
modelX2X3 = lm(y~x2+x3)
summary(modelX2X3)

modelx1 = lm(y~x1)
summary(modelx1)

#  R2_2 bien = 0.3949
# R2_1 bien = 0.4785 => Mo hinh 1 bien cho ket qua tot hon.




