## clear working space
rm(list=ls()) 

## set working directory
setwd("E:/Sales_Revenue/data")


## Read data into R (i.e) both train and test data
train_data<-read.csv("train.csv",header = T,sep = ",")
test_data<- read.csv("test.csv", header = T, sep = ",")

## Understanding train data

## get dimensions of the data set 
dim(train_data)
# List out the variables in the train dataset
names(train_data)
# Get summary of the train dataset
summary(train_data)
str(train_data)

## Understanding test data
dim(test_data)
names(test_data)
summary(test_data)
str(test_data)

#  Distribution of revenue
# hist(train_data$revenue,right = FALSE, col = black, xlab = Revenue)
Revenue_ = train_data$revenue
hist(Revenue_,col = "blue")
# see the distribution after applying log on revenue
ss = log(train_data$revenue)
Revenue = as.numeric(ss)
hist(Revenue,col = "blue")
# Plotting Type data from Test and Train data 
par(mfrow=c(1,2))
plot(train_data$Type)
plot(test_data$Type)
# Removing revenue,Id and City from Train data
revenue <- train_data$revenue 
train_data <- train_data[,-c(1,3,43)]
# Removing Id and City from Test data 
test_data <- test_data[,-c(1,3)]
lgrevenue <- log10(revenue) 
# Combine train and test dataset 
# for analysis without target variable "revenue"
newer_data <- rbind(train_data,test_data)
str(newer_data)
names(newer_data)

# calculating number of years restaurant is open
today=Sys.Date()
newer_data=cbind(newer_data,today)
class(today)
head(newer_data$Open.Date)
newer_data$Open.Date=as.Date(newer_data$Open.Date, format = "%m/%d/%Y")

library(lubridate)
newer_data$open_year=as.numeric(year(newer_data$Open.Date))

newer_data$current_year=as.numeric(year(today))
class(newer_data$open_year)
newer_data$diffyears=(newer_data$current_year)-(newer_data$open_year)
head(newer_data$diffyears)
str(newer_data)
names(newer_data)

# newdata=newdata[,-c(1,2,3,45,44,43)]
par(mfrow=c(1,2))
plot(newer_data$Type)
plot(newer_data$Type)

library(car)
# levelling on "Type"  variable as DT = FC, MB = IL
newer_data$Type<- recode(newer_data$Type,"'DT'='FC'")
newer_data$Type<- recode(newer_data$Type,"'MB'='IL'")
 
str(newer_data$Type)
levels(newer_data$Type)

plot(newer_data$Type)
plot(newer_data$Type)

sum(is.na(newer_data))
str(newer_data)

## removing prexisted variable which do not contribute much to model
newdataremove = newer_data[,-c(1,41,42,43)] 

## settin categorical variables as new data frame and 
## numerical as another data frame
data_Cat = newdataremove[,c(1,2)]
data_Num = newdataremove[,c(3:40)]

## Standardize numerical variables by "Range" 
library(vegan)
data_Num <- decostand(data_Num,"range")
summary(data_Num)

## create dummy variables for City.Group & Type
library(dummies)
function(x){
  dataCatDummy=data.frame(dummy(data_Cat$x))
  dataCat=subset(data_Cat,select=-c(x))
  dataCat=data.frame(data_Cat,dataCatDummy)}
dataCatDummy=dummy.data.frame(data_Cat)  ### Creating Dummies

str(dataCatDummy)
## Combine nuerical data after standardize and dummy into a new data frame

dataFinal=data.frame(data_Num,dataCatDummy)

## converted all data into numeric
qq = data.frame(sapply(dataFinal, as.numeric)) 
sum(is.na(qq))

## Seperate train and test datasets
Train <- qq[1:137, ]
Test <- qq[138:nrow(qq), ]

## Binding the revenue column and forming new data frame
Train = data.frame(Train,lgrevenue) 

## Anti log code
antilog<-function(lx,base)
{
  lbx<-lx/log(exp(1),base=base)
  result<-exp(lbx)
  result
}

## Applying K-Fold Cross validation

# load libraries
library(caret)
library(rpart)

# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model <- lm(Train$lgrevenue~., data=Train, trControl=train_control,method = "rpart")
# make predictions
predictions<- predict(model,newdata = Test)
predictions_Train = predict(model,newdata = Train[,-c(43)])
summary(model)

## Model Evaluation using different metrics like mae,mse,rmse,mape for linear regression

library(DMwR)
regr.eval(Train$lgrevenue,predictions_Train,train.y = Train$lgrevenue)

# append predictions

ssq <- predictions
data_Predictions = data.frame(ssq)

sos = antilog(data_Predictions$predictions,10)

revenuetest <- predict(model,newdata = Test)
ssa = antilog(revenuetest,10)
## see anti-log predictions on test data

# write.csv( ssa, file = "predictdata12_Q_lm")
# write.csv(ssa, "predict_Q_lm_002.csv", row.names=F)


####### Applying Random Forest ###########
library(randomForest)
revenue_rf <- randomForest(Train$lgrevenue ~ ., data=Train, 
                             keep.forest=TRUE, ntree=500, mtry=10) 
summary(revenue_rf)

revenuetest_rf <- predict(revenue_rf,newdata = Test)
revenuetest_rf_train = predict(revenue_rf,newdata = Train)
ss_rf = antilog(revenuetest_rf,10)

## write.csv( ss_rf, file = "predictdata12_rf")
## write.csv(ss_rf, "predict_rf_002.csv", row.names=F)

## This is for evaluating Random forest
library(DMwR)
regr.eval(Train$lgrevenue,revenuetest_rf_train,train.y = Train$lgrevenue)


Train_n = cbind.data.frame(Train,revenue)
Train_n = Train_n[,-c(43)] 


######## Applying PCA ##########
Train_pca = Train[,-c(43)]
Test_pca = Test

Test_Train_pca = rbind(Train_pca,Test_pca)
prinCompnents <- prcomp(Test_Train_pca,scale. = TRUE,center = TRUE)
names(prinCompnents)
summary(prinCompnents)
prinCompnents$rotation[1:10,1:10]
##compute the std dev of each principal component
stdDev <- prinCompnents$sdev
##compute the variance of each principal component
prinCompVar <- stdDev ^ 2
prinCompVar[1:20]
propVarExp <- prinCompVar/sum(prinCompVar)
propVarExp[1:10]
plot(propVarExp, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# cumulative scree plot
plot(cumsum(propVarExp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

## We are intrested in first 20 pca's
RevenueTrainData = data.frame(prinCompnents$x)
## select First 20 pca's
RevenueTrainData <- RevenueTrainData[,1:20]
head(RevenueTrainData)
dim(RevenueTrainData)

# Split  data into test and train after applying pca
Train_after_pca = RevenueTrainData[c(1:137),]
Test_after_pca = RevenueTrainData[c(137:100000),]
lg_revenue = Train[,c(43)]
Revenue_Train = cbind(Train_after_pca,lgrevenue)
## Apply lm on PCA train Data
revenuePCAModel <- lm(Revenue_Train$lgrevenue  ~  .,data = Revenue_Train)
summary(revenuePCAModel)

predictRevenue <- predict(object = revenuePCAModel, 
                          newdata = Revenue_Train )
head(predictRevenue)

## Model Evaluation using different metrics like mae,mse,rmse,mape

library(DMwR)
regr.eval(Revenue_Train$lgrevenue,predictRevenue,train.y = Revenue_Train$lgrevenue)


## for test Data
predictRevenue_on_test <- predict(object = revenuePCAModel, 
                          newdata = Test_after_pca )
### Rmse == 0.18 on train Data
saas = antilog(predictRevenue_on_test,10)

## write.csv( saas, file = "predictdata_newPCA__lm")
## write.csv(saas, "predict_new_PCA_with__lm.csv", row.names=F)

## PCA on Random Forest

library(randomForest)
Revenue_rf <- randomForest(lgrevenue ~ ., data=Revenue_Train,
                           keep.forest=TRUE, ntree=500, mtry=5)

pred = predict(Revenue_rf,Revenue_Train )
## Error metrics for pca on rf
library(DMwR)
regr.eval(Revenue_Train$lgrevenue,pred,train.y = Revenue_Train$lgrevenue)

sas_Rf = antilog(pred,10)
# write.csv( sas_Rf, file = "predictdata_PCA_0002")
# write.csv(sas_Rf, "predictPCA_with_RF_20.csv", row.names=F)



############### Apply Random Forest to select 
######          imoprtant attributes and apply SVM ##############
Revenue_rf_rf <- randomForest(Train$lgrevenue ~ ., data=Train, 
                              keep.forest=TRUE, ntree=30, mtry=4)

Revenue_rf_rf$importance

## plot(Revenue_rf_rf$importance)

revenueImp <- Revenue_rf_rf$importance

Imp_attributes_rf <- data.frame(Revenue_rf_rf$importance)
Imp_attributes_rf <- data.frame(row.names(Imp_attributes_rf),Imp_attributes_rf[,1])
colnames(Imp_attributes_rf) = c('Attributes','Importance')
Imp_attributes_rf <- Imp_attributes_rf[order(Imp_attributes_rf$Importance , decreasing = TRUE),]
Imp_attributes_rf <- Imp_attributes_rf[1:9,]
Imp_attributes_rf

Train_for_svm = Train[,c(38,1,28,29,2,11,20,6,17,43)]

## Applying K-Fold Cross validation

# load libraries
library(caret)
library(rpart)

# define training control
train_control<- trainControl(method="cv", number=10)
# train the model 
library(e1071)
model.SVM <- svm(Train_for_svm$lgrevenue~., data=Train_for_svm,trControl=train_control,method = "rpart")
model <- lm(Train$lgrevenue~., data=Train, trControl=train_control,method = "rpart")
# make predictions
predictions_rf_svm<- predict(model.SVM,newdata = Test)
predictions_Train = predict(model.SVM,newdata = Train_for_svm[,-c(43)])
summary(model.SVM)
rf_s_v_m = antilog(predictions_rf_svm,10)

##write.csv( rf_s_v_m, file = "predictdation_sv")
##write.csv(rf_s_v_m, "predictions_rf_svm.csv", row.names=F)

# Ensemble of all models
 
### SVM for regression
library(e1071)
model.SVM <- svm(Train$lgrevenue~., Train)
summary(model.SVM)
## Predicting using Svm model for test Data
pred.SVM<- predict(model.SVM, newdata=Test)
s_v_m = antilog(pred.SVM,10)

## Neural Network for regression
library(nnet)

nn = nnet(Train$lgrevenue ~ ., data = Train,size = 8, rang = 0.1,
          decay = 5e-4, maxit = 500,linout=T)

pred_nn = predict(nn, Test)

s_v_m = antilog(pred.SVM,10) # Result from svm only

neural = antilog(pred_nn,10) ## Result from neural net

ssa = antilog(revenuetest,10)#Result From linear regression 

rf_s_v_m = antilog(predictions_rf_svm,10) # Result from rf_svm

ss_rf = antilog(revenuetest_rf,10) # Result from Rf only


Newform_ensemble = s_v_m  + ss_rf + ssa

Newform_ensemble_1 = Newform_ensemble / 3

## write.csv( Newform_ensemble_1, file = "predictdata_ensemble")
## write.csv(Newform_ensemble_1, "predict_ensemble__14.csv", row.names=F)











