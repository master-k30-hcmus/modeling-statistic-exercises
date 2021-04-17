setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Final - 120421")

insurance = read.csv("data/insurance.csv")
head(insurance)
dim(insurance)

NA_values = data.frame(no_of_na_values = colSums(is.na(insurance))) #check missing data
head(NA_values,100)

dup = duplicated(insurance)
dup_data = insurance[dup,]; dup_data
insurance = insurance[-c(582),]
dim(insurance)

library(ggplot2)
library(gridExtra)

plt = {}
plt$region = ggplot(data = insurance) + geom_histogram(aes(x = region), stat = "count") + labs(x = "Region", y = "Count")
plt$smoker = ggplot(data = insurance) + geom_histogram(aes(x = smoker), stat = "count") + labs(x = "Smoker", y = "Count")
plt$sex = ggplot(data = insurance) + geom_histogram(aes(x = sex), stat = "count") + labs(x = "Sex", y = "Count")
plt$bmi = ggplot(data = insurance) + geom_histogram(aes(x = bmi)) + labs(x = "BMI", y = "Count")
plt$age = ggplot(data = insurance) + geom_histogram(aes(x = age)) + labs(x = "Age", y = "Count")
plt$children = ggplot(data = insurance) + geom_histogram(aes(x = children)) + labs(x = "Children", y = "Count")
plt$charges = ggplot(data = insurance) + geom_histogram(aes(x = charges)) + labs(x = "Charges", y = "Count")

grid.arrange(plt$bmi, plt$region, plt$smoker, plt$age, plt$sex, plt$children, plt$charges, nrow = 3)

insurance$smoker = ifelse(insurance$smoker == "yes", 1, 0)
insurance$region_ne = ifelse(insurance$region == "northeast", 1, 0)
insurance$region_se = ifelse(insurance$region == "southeast", 1, 0)
insurance$region_sw = ifelse(insurance$region == "southwest", 1, 0)

insurance = subset(insurance, select = -c(region))

head(insurance)
dim(insurance)

attach(insurance)

aggregate(charges ~ region_se + region_ne + region_sw, insurance, mean)
aggregate(charges ~ smoker, insurance, mean)
aggregate(charges ~ sex, insurance, mean)
aggregate(charges ~ children, insurance, mean)

bmi_ranges <- cut(age, c(seq(15, 55, by = 5)), include.lowest = TRUE)
aggregate(charges ~ bmi_ranges, insurance, mean)

age_ranges <- cut(age, c(seq(min(age), max(age), by = 10)), include.lowest = TRUE)
aggregate(charges ~ age_ranges, insurance, mean)


cor_data = data.frame(insurance)
correlation = cor(cor_data)
par(mfrow = c(1, 1))
corrplot(correlation, method = "color")

processModelSelection = function(model, data){
  par(mfrow = c(2,2))
  plot(model)

  ## model selection with BIC criteria, using Stepwise regression
  model_BIC = MASS::stepAIC(model, direction = "both", k = log(nrow(data)))
  par(mfrow = c(2,2))
  plot(model_BIC)
  mmps(model_BIC)
  avPlots(model_BIC)

  return(model_BIC)
}

mod_full = lm(charges ~ ., insurance)
summary(mod_full)
coef(mod_full)
vif(mod_full)

mod_BIC = processModelSelection(model = mod_full, data = insurance)
summary(mod_BIC)
mod_BIC$anova
vif(mod_BIC)
coef(mod_BIC)

# library(MASS)
# library(plyr)
# library(broom)
# library(car)
# library(corrplot)

