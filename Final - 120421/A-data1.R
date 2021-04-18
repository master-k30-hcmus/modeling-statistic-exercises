setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Final - 120421")


# Read data
insurance = read.csv("data/insurance.csv")
head(insurance)
dim(insurance)


# Check missing data
NA_values = data.frame(no_of_na_values = colSums(is.na(insurance)))
head(NA_values,100)


# Remove duplicate data
dup = duplicated(insurance)
dup_data = insurance[dup,]; dup_data
insurance = insurance[-c(582),]

dim(insurance)
attach(insurance)


library(ggplot2)
library(gridExtra)


# Draw histogram of 7 vars
plt = {}
plt$sex = ggplot(data = insurance) + geom_histogram(aes(x = sex), stat = "count") + labs(x = "Sex", y = "Count")
plt$smoker = ggplot(data = insurance) + geom_histogram(aes(x = smoker), stat = "count") + labs(x = "Smoker", y = "Count")
plt$region = ggplot(data = insurance) + geom_histogram(aes(x = region), stat = "count") + labs(x = "Region", y = "Count")
plt$bmi = ggplot(data = insurance) + geom_histogram(aes(x = bmi)) + labs(x = "BMI", y = "Count")
plt$age = ggplot(data = insurance) + geom_histogram(aes(x = age))F
plt$children = ggplot(data = insurance) + geom_histogram(aes(x = children)) + labs(x = "Children", y = "Count")
plt$charges = ggplot(data = insurance) + geom_histogram(aes(x = charges)) + labs(x = "Charges", y = "Count")

grid.arrange(plt$sex, plt$smoker, plt$region, plt$bmi, plt$age, plt$children, plt$charges, nrow = 3)


# Draw boxplot of 6 vars by charges
bmi_ranges <- cut(bmi, c(seq(15, 55, by = 5)), include.lowest = TRUE)
age_ranges <- cut(age, c(seq(min(age), max(age), by = 2)), include.lowest = TRUE)

bxplt = {}
bxplt$sex = ggplot(data = insurance) + geom_boxplot(aes(x = sex, y = charges), outlier.colour = "black")
bxplt$smoker = ggplot(data = insurance) + geom_boxplot(aes(x = factor(insurance$smoker), y = charges), outlier.colour = "black") + labs(x = "smoker")
bxplt$region = ggplot(data = insurance) + geom_boxplot(aes(x = factor(insurance$region), y = charges), outlier.colour = "black") + labs(x = "region")
bxplt$children = ggplot(data = insurance) + geom_boxplot(aes(x = factor(insurance$children), y = charges), outlier.colour = "black") + labs(x = "children")
bxplt$bmi = ggplot(data = insurance) + geom_boxplot(aes(x = bmi_ranges, y = charges), outlier.colour = "black")
bxplt$age = ggplot(data = insurance) + geom_boxplot(aes(x = age_ranges, y = charges), outlier.colour = "black") + theme(axis.text.x = element_text(angle = 90))

grid.arrange(bxplt$sex, bxplt$smoker, bxplt$region, bxplt$bmi, bxplt$age, bxplt$children, nrow = 3)


# Transform dummy data
insurance$sex = ifelse(insurance$sex == "male", 1, 0)
insurance$smoker = ifelse(insurance$smoker == "yes", 1, 0)
insurance$region_ne = ifelse(insurance$region == "northeast", 1, 0)
insurance$region_se = ifelse(insurance$region == "southeast", 1, 0)
insurance$region_sw = ifelse(insurance$region == "southwest", 1, 0)
insurance = subset(insurance, select = -c(region))


library(corrplot)
cor_data = data.frame(insurance[,1:9])
correlation = cor(cor_data)
corrplot(correlation, method = "number")


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


library(car)

# Fit full model
mod_full = lm(charges ~ ., insurance)
summary(mod_full)
coef(mod_full)
vif(mod_full)

mod_BIC = processModelSelection(model = mod_full, data = insurance)
summary(mod_BIC)
mod_BIC$anova
vif(mod_BIC)
coef(mod_BIC)


# Try to remove outliers
data_res = residuals(mod_BIC)
data_crit = 2*sd(data_res)
data_outlier = ifelse(abs(data_res > data_crit),1,0); data_outlier
data_outlier[data_outlier == 1]
new_data = insurance[-c(as.numeric(names(data_outlier[data_outlier == 1]))),]
dim(new_data)

new_mod_full = lm(charges ~ ., insurance)
summary(new_mod_full)
coef(new_mod_full)
vif(new_mod_full)

new_mod_BIC = processModelSelection(model = new_mod_full, data = insurance)
summary(new_mod_BIC)
new_mod_BIC$anova
vif(new_mod_BIC)
coef(new_mod_BIC)


# Try to scale data
scaled <- as.data.frame(scale(insurance[,c("bmi","charges")]))
insurance_scaled = insurance
insurance_scaled$bmi = scaled$bmi
insurance_scaled$charges = scaled$charges

scaled_mod_full = lm(charges ~ ., insurance_scaled)
summary(scaled_mod_full)
coef(scaled_mod_full)
vif(scaled_mod_full)

scaled_mod_BIC = processModelSelection(model = scaled_mod_full, data = insurance_scaled)
summary(scaled_mod_BIC)
scaled_mod_BIC$anova
vif(scaled_mod_BIC)
coef(scaled_mod_BIC)


detach(insurance)
