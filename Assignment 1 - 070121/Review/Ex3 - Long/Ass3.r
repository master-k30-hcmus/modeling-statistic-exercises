data <- read.delim("https://gattonweb.uky.edu/sheather/book/docs/datasets/invoices.txt",header = TRUE)

#Gia thiet
x <- data$Invoices
y <- data$Time

#Tinh cac gia tri can thiet
xbar <- mean(x) #Trung bình x
ybar <- mean(y) #Trung bình y
SXY <- sum(((x - xbar)*(y - ybar)))
SXX <- sum((x - xbar)^2)

n <- length(x) #so luong mau
df <- n - 2 #degree of freedom
alpha = 0.05

t_95 <- qt(1 - alpha/2,df= df)


##Cau a
beta1_hat <- SXY/SXX #Tinh beta1_hat
beta0_hat <- ybar - beta1_hat*xbar #Tinh beta0_hat

yhat <- beta0_hat + beta1_hat*x #Tinh y mu
s_square <- sum((y-yhat)^2)/df #MSE = SSE/df

beta0_hat_se <- sqrt(s_square)*sqrt(1/n + (xbar^2)/SXX) #estimated standard error of beta0_hat (se(beta0_hat))
beta0_epsilon <- t_95*beta0_hat_se #Dung sai
(beta0_95 <- c(beta0_hat - beta0_epsilon, beta0_hat + beta0_epsilon))

##Cau b
# H_0: beta_1 = 0.01
# H_1: beta_1 != 0.01

beta1_0 <- 0.01
beta1_hat_se <- sqrt(s_square)/sqrt(SXX) # estimated standard error se(beta1)
(tval <-(beta1_hat - beta1_0)/beta1_hat_se) # Gia tri thong ke
(pvalue <- 2*pt(abs(tval),30-1,lower.tail = FALSE)) # Tinh p-value
#Ta chua du co so de bac bo H_0 voi muc y nghia 0.05 và thuc hien tren mau nay

##Cau c
new_invoices <- 130
# Model: Time = 0.6417099 + Invoices x 0.0112916
# Gia tri y du doan khi dua x = 130
(time <- beta0_hat + beta1_hat * new_invoices) #y_0 = beta0 + beta1*x_0
# Tinh dung sai
mse <- sqrt(s_square)*sqrt(1 + 1/n + ((new_invoices - xbar)^2)/SXX) #estimated standard error of 
y_epsilon <- t_95 * mse
# Khoang tin cay
(upr <- time + y_epsilon)
(lwr <- time - y_epsilon)
