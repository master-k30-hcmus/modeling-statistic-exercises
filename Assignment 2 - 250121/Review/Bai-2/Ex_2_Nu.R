#Import directly data

do_ben_deo_nhua <- c(37.8,22.5,17.1,10.8,7.2,42.3,30.2,19.4,14.8,9.5,32.4,21.6)
do_day_vat_lieu <- c(4,4,3,2,1,6,4,4,1,1,3,4)
mat_do_vat_lieu <- c(4,3.6,3.1,3.2,3.0,3.8,3.8,2.9,3.8,2.8,3.4,2.8)


data <- data.frame(do_ben_deo_nhua,do?day_vat_lieu,mat_do_vat_lieu)

print(data)


#Visualize:
plot(density(do_ben_deo_nhua))

plot(data)

summary(data)

#---------1. Buil model Y|X1, Y|X2 and Y(X1,X2)

#Model Y|X1:

mode_Y_X1 = lm(do_ben_deo_nhua ~ do_day_vat_lieu)
print(mode_Y_X1)

# => Y = ?.523 + 6.036 * do_day_vat_lieu

#Model Y|X2:

model_Y_X2 = lm(do_ben_deo_nhua~mat_do_vat_lieu)
print(model_Y_X2)

# => Y = -36.37 + 17.46 * mat_do_vat_lieu

# Model Y|(X1,X2) or multiple linear regression

model_Y_X1X2 = lm(do_ben_deo_nhua~do_day_vat_lieu+?at_do_vat_lieu)
print(model_Y_X1X2)

# => Y = -30.081 + 4.905 * do_day_vat_lieu + 11.072 * mat_do_vat_lieu


# 2.  Xac ð???nh t??? l??? ph???n trãm bi???n thiên c???a bi???n ph??? thu???c cho t???ng mô h??nh có th??? có
#Model 1: do_ben_deo_nhua ~ do_day_vat_lieu 

summary(mode_Y_X1) # mo hinh 1 Y bien thien 69% boi x

summary(model_Y_X2) # mo hinh 2 Y bien thien 45% boi x

summary(model_Y_X1X2) # mo hinh 2 Y bien thien 84.8% boi x

#3. Lap bang ANOVA cho mo hinh hoi ?uy 2 bien X1,X2

anova(model_Y_X1X2)

simple(model_Y_X1X2)

