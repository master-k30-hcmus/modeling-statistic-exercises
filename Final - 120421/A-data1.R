setwd("D:/Personal/HCMUS/master-k30/semester-1/code/modeling-statistic-exercises/Final - 120421")

insurance = read.csv("data/insurance.csv")
head(insurance)
dim(insurance)

dup = duplicated(insurance)
dup_data = insurance[dup,]; dup_data
insurance = insurance[-c(582),]
dim(insurance)

insurance$smoker = ifelse(insurance$smoker == "yes", 1, 0)
insurance$region_ne = ifelse(insurance$region == "northeast", 1, 0)
insurance$region_se = ifelse(insurance$region == "southeast", 1, 0)
insurance$region_sw = ifelse(insurance$region == "southwest", 1, 0)

insurance = subset(insurance, select = -c(region))

head(insurance)
dim(insurance)

attach(insurance)

library(MASS)
library(ggplot2)
library(plyr)
library(broom)
library(car)
library(corrplot)

