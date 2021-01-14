# Exercise 1
# Given model: Y = B0 + B1x + e

## Load data
playbill <- read.csv(file.path("data", "playbill.csv"))

## Data information
dim(playbill)
playbill[1:5,]
summary(playbill)
plot(playbill)

## LR Model information
lm_fit = lm(CurrentWeek ~ LastWeek, data = playbill)
summary(lm_fit)
names(lm_fit)
coef(lm_fit)

### a - Confidence interval for B1

confint(lm_fit, level = 0.95)[2,]

# Because 1 is in the interval of B1, so it's plausible for B1=1


### b - Test null hypothesis where H_0: B0=10000

B0 = 10000 # given
B0_hat = summary(lm_fit)$coefficients["(Intercept)", "Estimate"]
B0_se = summary(lm_fit)$coefficients["(Intercept)", "Std. Error"]

t_obs = (B0_hat - B0) / B0_se; t_obs
p_obs = 2 * pt(abs(t_obs), nrow(playbill) - 2, lower.tail = FALSE); p_obs

# We accept the null hypothesis, t(16)=-0.32, p=0.75

### c

predict(lm_fit, data.frame(LastWeek = 400000), interval = "prediction")

# Because $450000 is out of the prediction interval so it's not feasible

### d

par(mfrow = c(2, 2))
plot(lm_fit)

# Following the Residuals vs Fitted plot, we'll see that the prediction rule is
# quite reasonable, however, there are at least three bad values outside the LR
# line, therefore, we do need more data for analyzing or another model to have
# more clues to reduce the risky probability.
