#file path
setwd("D:/DATA SCIENCE/HCMUS-K30/hp1-2020/MohinhoaTK/homework")
# -------------------------------

# EXERCISE 1
## Load data
playbill <-
  read.csv(file = "playbill.csv", header = TRUE, sep = ",")

## Data information
dim(playbill)
playbill[1:5, ]
summary(playbill)
plot(playbill)

## Create linear model
lm_fit = lm(CurrentWeek ~ LastWeek, data = playbill)
summary(lm_fit)
names(lm_fit)
coef(lm_fit)

### a - Confidence interval for B1
confint(lm_fit, level = 0.95)[2, ]

### b - Test null hypothesis where H_0: B0=10000
B0 = 10000 # given
B0_hat = summary(lm_fit)$coefficients["(Intercept)", "Estimate"]
B0_se = summary(lm_fit)$coefficients["(Intercept)", "Std. Error"]

t_obs = (B0_hat - B0) / B0_se
t_obs
p_value = 2 * pt(t_obs, nrow(playbill) - 2)
p_value

### c
predict(lm_fit, data.frame(LastWeek = 400000), interval = "prediction")

### d
par(mfrow = c(2, 2))
plot(lm_fit)

# -------------------------------

# EXERCISE 2
## Load data
indicators <- read.table(file = "indicators.txt", header = TRUE)
View(indicators)

# Create variables:
priceChange <- indicators$PriceChange
loanPayment <- indicators$LoanPaymentsOverdue

# Create linear model
linearModel2 <- lm(priceChange ~ loanPayment)
linearModel2

### a - Find 95% confident interval of B1
confint(linearModel2, level = 0.95)

### b - Predict Y with X = 4
result <- predict(linearModel2, data.frame(loanPayment = 4), interval = 'confidence')
result

# -------------------------------
# EXERCISE 3

## Load data
invoices <- read.table(file = "invoices.txt", header = TRUE)

## Data information
dim(invoices)
head(invoices)
summary(invoices)
plot(invoices$Invoices,
     invoices$Time ,
     xlab = "Number of Invoices for the day",
     ylab = "Processing time in hours")

## Create linear model
fit <- lm(formula = Time ~ Invoices, data = invoices)
abline(fit, lwd = 3, col = "red")
summary(fit)
names(fit)
coef(fit)

### a - Find a 95% confidence interval for the start-up time
confint(fit, "(Intercept)", level = 0.95)

### b - Test the null hypothesis H0: B1 = 0.01 against a two-sided alternative
confint(fit, "Invoices", level = 0.95)

### c - Find a point estimate and a 95% prediction interval for the time taken to process 130 invoices
predict(fit, data.frame(Invoices = 130), interval = "prediction")
