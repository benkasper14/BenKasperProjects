#Load Data
train_og = read.csv("C:/Users/benoi/Desktop/School/STAT1361/train.csv")
test_og = read.csv("C:/Users/benoi/Desktop/School/STAT1361/test.csv")

library(dplyr)
library(randomForest)
library(leaps)
library(glmnet)
library(gbm)
library(caret)
library(pls)
set.seed(100)

#Find outliers
boxplot(train_og$price, ylab="Price")
max = max(train_og$price)

#Create new variables based on observations
train_og = train_og %>% 
  mutate(age = (2025-model_year)) %>%
  filter(price!=max) %>% 
  mutate(
    brand = as.factor(brand),              
    fuel_type = as.factor(fuel_type),     
    ext_col = as.factor(ext_col),         
    int_col = as.factor(int_col),          
    accident = as.factor(accident),        
    transmission_type = as.factor(transmission_type)
  ) %>% 
  select(-c(id, model_year))

test_og = test_og %>% 
  mutate(age = (2025 - model_year)) %>%
  mutate(
    brand = as.factor(brand),              
    fuel_type = as.factor(fuel_type),     
    ext_col = as.factor(ext_col),         
    int_col = as.factor(int_col),          
    accident = as.factor(accident),        
    transmission_type = as.factor(transmission_type)
  ) %>% 
  select(-model_year)

#View(train_og)

#Split into train and test
sample = sample(nrow(train_og), floor(nrow(train_og)*.75))
train = train_og[sample,]
test = train_og[-sample,]

any(is.na(train))
any(is.na(test))

#Simple Linear Model w all variables
lin_model = lm(price ~ ., data=train)
pred = predict(lin_model, newdata=test)
mean((pred - test$price)^2)
summary(lin_model)
#MSE = .1149

#Check for possible squared/cubed terms
par(mfrow = c(2, 2))
plot(train_og$price, train_og$mileage, ylab="Price", xlab="Mileage")
plot(train_og$price, train_og$horsepower, ylab="Price", xlab="Horsepower")
plot(train_og$price, train_og$age, ylab="Price", xlab="Age")
plot(train_og$price, train_og$liters, ylab="Price", xlab="Liters")

lin_model2 = lm(price ~ mileage + I(mileage^2) + horsepower + I(horsepower^2) + ., data=train)
pred = predict(lin_model2, newdata=test)
mean((pred - test$price)^2)
summary(lin_model2)
#MSE = .1118, slight improvement

#Check for any irrelevant variables using LASSO
x_train = model.matrix(price ~ ., data = train)[,-1]
y_train = train$price

x_test = model.matrix(price ~ ., data = test)[,-1]
y_test = test$price 

lasso_model = glmnet(x_train, y_train, alpha = 1)
cv_lasso = cv.glmnet(x_train, y_train, alpha = 1)

best_lambda = cv_lasso$lambda.min
best_lambda

pred = predict(lasso_model, s=best_lambda, newx=x_test)
mean((pred - y_test)^2)
#MSE = .1102, slight improvement

#Forward selection, try to simplify model
fwd_model = regsubsets(price ~ ., data=train, nvmax=70, method="forward")
summary(fwd_model)$adjr2
#~24 vars is where it starts to level off
#The adj r^2 max is ~.85

#Trial and error interaction variables
interaction_model = lm(price ~ . + ext_col:age + int_col:age + brand:age + brand:accident, data=train)
summary(interaction_model)

pred = predict(interaction_model, newdata=test)
mean((pred - test$price)^2)
#.103, new best
coefficients = summary(interaction_model)$coefficients 


#Random forests with importance
#Tune parameters
ntrees = c(300,500,700,1000)
mtrys = c(2,3,4,5)

for(i in 1:4) {
  for(j in 1:4) {
    RF = randomForest(price ~ ., data=train, ntree=ntrees[i], mtry=mtrys[j], importance=T)
    importance(RF)[order(-importance(RF)[,1]),]
    
    RF_pred = predict(RF, test)
    cat(paste(ntrees[i], " ", mtrys[j], "\n"))
    print(mean((test$price - RF_pred)^2))
  }
}
#MSE = about .091 or .092 for most

#RF with important varaibles:
train_importance = train %>% 
  select(mileage, horsepower, age, brand, liters, cylinders, fuel_type, int_col,
         transmission_type, accident, ext_col, price)
test_importance = test %>% 
  select(mileage, horsepower, age, brand, liters, cylinders, fuel_type, int_col,
         transmission_type, accident, ext_col, price)

for(i in 1:4) {
  for(j in 1:4) {
    RF_imp = randomForest(price ~ ., data=train_importance, ntree=ntrees[i], mtry=mtrys[j])
    RF_pred_imp = predict(RF_imp, test)
    
    cat(paste(ntrees[i], " ", mtrys[j], "\n"))
    print(mean((test_importance$price - RF_pred_imp)^2))
  }
}
#Did not improve MSE

#Boosting
par(mfrow = c(1, 1))
#Tune parameters
ntrees = c(100,250,500,1000,2000,3000,5000,7000,10000)
depth = c(2,3,4,5,6)
MSEs = rep(0,9)

for(i in 1:9) {
  for(j in 1:5) {
    boost = gbm(price ~ ., data = train, distribution = "gaussian", n.trees = ntrees[i], 
                interaction.depth = depth[j], shrinkage=.01)
    
    summary(boost)
    
    pred = predict(boost, newdata = test, n.trees = ntrees[i])
    
    MSE = mean((test$price - pred)^2)
    cat(paste("ntrees= ", ntrees[i], " depth= ", depth[j], " MSE= ", MSE, "\n"))
    
    cat(paste("Done j: ", depth[j], "\n"))
    
  }
  
  cat(paste("Done ntrees: ", ntrees[i], "\n"))
}

#Several models with MSE of .071, ntrees=2000, depth=5 is the simpliest

#Run boosting again for only most important variables:
MSEs = rep(0,9)

for(i in 1:9) {
  for(j in 1:5) {
    boost = gbm(price ~ ., data = train_importance, distribution = "gaussian", n.trees = ntrees[i], 
                interaction.depth = depth[j], shrinkage=.01)
    
    summary(boost)
    
    pred_imp = predict(boost, newdata = test_importance, n.trees = ntrees[i])
    
    MSE = mean((test_importance$price - pred_imp)^2)
    cat(paste("ntrees= ", ntrees[i], " depth= ", depth[j], " MSE= ", MSE, "\n"))
    
    cat(paste("Done j: ", depth[j], "\n"))
    
  }
  
  cat(paste("Done ntrees: ", ntrees[i], "\n"))
}
#Lowest MSE .075 with ntrees=3000, depth = 5 (no improvement)


#PCR and PLS
pcr_model = pcr(price ~ ., data=train_og, scale=T, validation="CV")
summary(pcr_model)
validationplot(pcr_model, val.type="MSEP")

pls_model = plsr(price ~ ., data=train_og, scale=T, validation="CV")
summary(pls_model)
validationplot(pls_model, val.type = "MSEP")

#Both gave MSEs around .1, not as good as RF/Boosting

#Final predictions based on best model:
boost_final = gbm(price ~ ., data = train_og, distribution = "gaussian", n.trees = 2000, 
                  interaction.depth = 5, shrinkage = 0.01, verbose = FALSE)

final_pred = predict(boost_final, newdata = test_og, n.trees = 2000)
output = data.frame(id=test_og$id, price=final_pred)
write.csv(output, "testing_predictions_Kasper_Benjamin_BRK105.csv", row.names=F)
