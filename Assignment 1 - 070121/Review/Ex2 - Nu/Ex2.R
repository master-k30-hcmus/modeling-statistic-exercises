# Load dataset

indicators <- read.table("C:/Users/thanh/Desktop/Master/MHH_THONG_KE/Chapter1/Data_exo1_2_3_SimonSheather/indicators.txt", header=TRUE)
View(indicators)


#---------------- Create variables:

priceChange <- indicators$PriceChange
loanPayment <- indicators$LoanPaymentsOverdue

#Create linear model for indicators dataset:

linearModel2 <- lm(priceChange ~ loanPayment)
linearModel2

#a) Find 95% confident interval of B1:
confint(linearModel2, level = 0.95)
# 1. 95% Confident Interval of B1: [-4.16:-0.33]
# 2.Enough evidence to decide of a significant negative linear association. When X increase then y decrease 

# -------------------------------

# b) Predict Y with X = 4 
result <- predict(linearModel2,data.frame(loanPayment = 4),interval = 'confidence')
result
# 1. When X = 4 then y = -4.479585
# 2. 95% CI for E(Y|loanPayment =4 ) in range [-6.6:-2.3]
# 3. 0% is not  feasible value for E(Y|loanPayment=4).Since, 95% Confident Interval for E(Y|loanPayment =4 ) in range [-6.6:-2.3]


