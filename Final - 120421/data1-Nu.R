library(readxl)

#Read dataset

data = read_excel("C:\\Users\\Administrator\\Desktop\\MASTER\\THONG_KE\\FINAL_ASSIGMENT_4\\data\\data1.xls")
data
str(data)

#factorize 2 colums "college" and "grad":
data$college = factor(data$college)
data$grad = factor(data$grad)

#Observe data 
plot(data[c( "salary" , "age" ,"comten", "ceoten","lsales","lmktval","profmarg")])
cor(data[c("salary", "lsales","lmktval","profmarg")])
dataProcess = data[c("salary","age","college","grad" , "comten","ceoten" ,"lmktval","profmarg")]


# Split into training and testing set with ratio 80:20 (Stable size)
k_train = round(nrow(dataProcess)*0.8,0)
k_train
train = dataProcess[1:k_train,]
test = dataProcess[(k_train + 1):nrow(dataProcess), ]

nrow(train)
nrow(test)
names(train)
str(train)

#Create 2 models: 
l0 = lm(formula = train$salary ~ 1, data = train) # non independence variable
l1 = lm(formula =  train$salary ~ ., data = train) # full independence variable

#StepWise_forward
modbest_Fow = step(l0, scope = list(lower = l0,
                                upper = l1), direction = 'forward', k =2)

summary(modbest_Fow)

#Discard variable "ceoten" b/c P_value > 0.05 and then regression model with remained variables

new_train = train[c("salary", "lmktval", "profmarg")]
newModel = lm(formula =new_train$salary ~ ., data = new_train )
summary(newModel)

#Predict for testset with newModel:

X_test = test[c("age" ,"college","grad","comten","ceoten","lmktval","profmarg")]
y_test = test[c("salary")]

pred_test = predict(newModel, X_test)
SE = sum((pred_test-y_test) ^2)
MSE = SE / nrow(test)
print(MSE)


#Implement ANOVA testing:
anova(newModel)

#Calculate F_obs
MSR = (11242276 + 1319152 )/2
F_obs = MSR/296129
print(F_obs)

F_crital = qf(.95, df1=2, df2=139) 
F_crital

# F_obs > F_crital => Bac bo H_0 hay tat ca cac bien deu co y nghia thong ke.








