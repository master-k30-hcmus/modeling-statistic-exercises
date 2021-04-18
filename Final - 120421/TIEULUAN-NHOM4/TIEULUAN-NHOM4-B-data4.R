setwd("D:/1. STUDYING/#Cao hoc/MHTK/Together/modeling-statistic-exercises/Final - 120421")

install.packages("readxl")
library(readxl)

#Read dataset
data4 <- read_excel("data/data4.xls")
head(data4)

summary(data4)
describe(data4)

install.packages("Hmisc")
library(Hmisc)
de <- describe(data4)

deinstall.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)
ggplot(data = data4) + geom_bar(mapping = aes(x = tenure))


quanti_var = list() #Bien dinh tinh
quali_var = list() #Bien dinh luong

for (var in de){
  if(as.integer(de[[var$descript]][["counts"]][["distinct"]]) < 3){
   quanti_var <- append(quanti_var, var$descript)
  }
  else{
    quali_var <- append(quali_var, var$descript)
  }
}
  

# Visulization data
install.packages("gcookbook")
library(gcookbook)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

install.packages("dplyr")
library(dplyr)
data4_quanti <- select(data4, unlist(quanti_var))

#Ve cac bien dinh luong
data4_quali <- select(data4, unlist(quali_var))
pairs(
  data4_quali,
  upper.panel = panel.cor,
  diag.panel  = panel.hist,
  lower.panel = panel.smooth,
  pch = "o"
)

#Ve cac bien dinh tinh
data4_quanti <- select(data4, c(unlist(quanti_var),"lwage"))
pairs(
  data4_quanti,
  upper.panel = panel.cor,
  diag.panel  = panel.hist,
  lower.panel = panel.smooth,
  pch = "o"
)
# Cac bien khong co do tuong quan cao

# Xay dung mo hinh
model1 = lm(wage ~ .- lwage - expersq - tenursq,data = data4)
summary(model1)

model1.stdres = rstandard(model1)

# Ve thu ket qua du doan
plot(predict(model1),data4$wage,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

#Variable Selection

library(MASS)
install.packages("leaps")
library(leaps)
## i) All Possible Subsets 
all<-regsubsets(wage~ . , data=data4[1:21], nbest=1, nvmax=24)
info <- summary(all)
re <- cbind(info$which, round(cbind(adjr2=info$adjr2, cp=info$cp, bic=info$bic), 4))

## ii) Stepwise Subset (based on AIC, BIC)
stepAIC(model1, direction = "backward")

stepAIC(model1, direction = "backward", k=2) #BIC


## iii) Stage wise
install.packages("lars")
library(lars)
x <- data.matrix(data4[2:21])
y <- lapply(data4[1],as.numeric)
k = lars(x,y[[1]], type = "forward.stagewise", trace = TRUE, max.steps = 100)
plot(k)
predict(k, type="coef", mode="norm")

# Hoi quy lai
model_R <- lm(wage ~ . - nonwhite - south - construc - clerocc - servocc, data= data4[1:21])
summary(model_R)

model_Cp <- lm(wage ~ . - nonwhite, data= data4[1:21])
summary(model_Cp)

model_BIC <- lm(wage ~ educ + tenure + female + smsa + west + trade + services +profocc, data = data4[1:21])
summary(model_BIC)

model_stepwise_AIC = lm(wage ~ educ + tenure + female + married + smsa + northcen + west + ndurman + 
                          trcommpu + trade + services + profserv + profocc, data = data4)
summary(model_stepwise_AIC)

model_stagewise = lm(wage ~ educ + tenure + female + married + smsa + west + trade + services + 
                       profserv + profocc, data = data4)
summary(model_stagewise)

model_log_BIC <- lm(lwage ~ educ + tenure + female + smsa + west + trade + services +profocc, data = data4[2:22])
summary(model_log_BIC)

install.packages("caret")
library(caret)
preproc1 <- preProcess(data4, method=c("center", "scale"))
norm1 <- predict(preproc1, data4)

library(car)
par(mfrow = c(2,2))
plot(model_log_BIC)

var_select =  c("educ",  "tenure"  , "female",     "smsa"  ,   "west"  ,  "trade", "services",  "profocc")
apply(data4[var_select],2,shapiro.test)
