#rm enviroinment
rm(list = ls(all = T))

#setting working directory
getwd()
setwd("/Users/piyushmishra/Downloads/Criminal1")

#loading data set

#loading data set
criminal= read.csv("criminal_train.csv", header = T, sep = ",")
str(criminal)


summary(criminal)
sum(is.na(criminal))

library(caret)
x=cor(criminal[,!(names(criminal) %in% "Criminal")])
findCorrelation(x,names=T,cutoff = 0.8)
criminal=criminal[,!(names(criminal)%in% c("HLCNOTYR","IIOTHHLT","GRPHLTIN","IRINSUR4","IROTHHLT","HLCLAST","IIINSUR4","PRVHLTIN","IICHMPUS","IIMEDICR","IIMCDCHP","IRFAMIN3","HLNVREF","HLNVOFFR","HLNVNEED","HLNVCOST","GOVTPROG","IIWELMOS","IRFAMSVC","TOOLONG","HLCALL99","AIIND102","PDEN10"))]


summary(criminal)

library(ggplot2)

qplot(criminal$ANALWT_C, criminal$Criminal, main = "With Outliers")
qplot(criminal$TROUBUND, criminal$Criminal, main = "With Outliers")
qplot(criminal$CELLWRKNG, criminal$Criminal, main = "With Outliers")
qplot(criminal$CELLNOTCL, criminal$Criminal, main = "With Outliers")
qplot(criminal$ANYHLTI2, criminal$Criminal, main = "With Outliers")
qplot(criminal$MEDICARE, criminal$Criminal, main = "With Outliers")

qplot(Criminal, data = criminal, bins = 50, main = "distribution")

criminal <- criminal[-which(criminal$ANALWT_C > 60000), ]
criminal <- criminal[-which(criminal$MEDICARE < 0), ]
##criminal <- criminal[-which(criminal$POVERTY3 < 0),]
##criminal <- criminal[-which(criminal$NRCH17_2 < 0),]
library(ROSE)

#check table
table(criminal$Criminal)

str(criminal$Criminal)
#check classes distribution
prop.table(table(criminal$Criminal))

##require(ggplot2)

##require(reshape2)
##dat.m <- melt(criminal,id.vars='PERID')
##ggplot(data = dat.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))

criminal$PERID<-NULL
##criminal$Criminal<-as.factor(criminal$Criminal)

test1 <- read.csv("criminal_test.csv", header = T, sep = ",")

sum(is.na(test1))

library(caret)
a=cor(test1[,!(names(test1) %in% "Criminal")])
findCorrelation(a,names=T,cutoff = 0.8)
test1=test1[,!(names(test1)%in% c("HLCNOTYR","IIOTHHLT","GRPHLTIN","IRINSUR4","IROTHHLT","HLCLAST","IIINSUR4","PRVHLTIN","IICHMPUS","IIMEDICR","IIMCDCHP","IRFAMIN3","HLNVREF","HLNVOFFR","HLNVNEED","HLNVCOST","GOVTPROG","IIWELMOS","IRFAMSVC","TOOLONG","HLCALL99","AIIND102","PDEN10"))]


test1$PERID<-NULL



criminal$Criminal<-as.numeric(criminal$Criminal)
criminal1 <- as.data.frame( scale(criminal[1:48] ))
summary(criminal1)
test1 <- as.data.frame( scale(test1[1:47] ))
criminal1$Criminal<-as.factor(criminal1$Criminal)

#test train split
library(caret)
set.seed(123)
trainrows <- createDataPartition(y = criminal1$Criminal, p = 0.8, list = F)
train <- criminal1[trainrows,]
test <- criminal1[-trainrows,]

library(caret)
set.seed(123)
trainrows <- createDataPartition(y = criminal$Criminal, p = 0.8, list = F)
train <- criminal[trainrows,]
test <- criminal[-trainrows,]

library(ROSE)

#check table
table(train$Criminal)

#check classes distribution
prop.table(table(train$Criminal))

library(rpart)
treeimb <- rpart(Criminal ~ ., data = train)
pred.treeimb <- predict(treeimb, newdata = test)


pred.treeimb<- ifelse(pred.treeimb > 0.5,1,0)

accuracy.meas(test$Criminal, pred.treeimb[,2])

roc.curve(test$Criminal, pred.treeimb[,2], plotit = F)

#over sampling
data_balanced_over <- ovun.sample(Criminal ~ ., data = train, method = "over",N = 68054)$data
table(data_balanced_over$Criminal)

data_balanced_under <- ovun.sample(Criminal ~ ., data = train, method = "under", N = 5080, seed = 1)$data
table(data_balanced_under$Criminal)

data_balanced_both <- ovun.sample(Criminal ~ ., data = train, method = "both", p=0.18, N=36567, seed = 1)$data
table(data_balanced_both$Criminal)

data.rose <- ROSE(Criminal ~ ., data = train, seed = 1)$data
table(data.rose$Criminal)

##library(DMwR)
##data.smote<-SMOTE(Criminal~., data = train,perc.over = 200,perc.under = 100)
##table(data.smote$Criminal)


#build decision tree models
tree.rose <- rpart(Criminal ~ ., data = data.rose)
tree.over <- rpart(Criminal ~ ., data = data_balanced_over)
tree.under <- rpart(Criminal ~ ., data = data_balanced_under)
tree.both <- rpart(Criminal ~ ., data = data_balanced_both)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)

pred.tree.rose <- ifelse(pred.tree.rose > 0.5,1,0)
pred.tree.over <- ifelse(pred.tree.over > 0.5,1,0)
pred.tree.under <- ifelse(pred.tree.under > 0.5,1,0)
pred.tree.both <- ifelse(pred.tree.both > 0.5,1,0)

#AUC ROSE
roc.curve(test$Criminal, pred.tree.rose[,2])

#AUC Oversampling
roc.curve(test$Criminal, pred.tree.over[,2])


#AUC Undersampling
roc.curve(test$Criminal, pred.tree.under[,2])


#AUC Both
roc.curve(test$Criminal, pred.tree.both[,2])
roc.curve(test$Criminal, test_prediction1[,1])
test1=predict(std_data,test1)
pred2<-predict(tree.both, newdata = test1)
predicted <- ifelse(predicted >= 1.1,1,0)

confusionMatrix(test$Criminal, pred.tree.over[,2]) 

confusionMatrix(test$Criminal, pred.tree.both) 



library(caret)
std_data=preProcess(train[,!(names(train)%in% "Criminal")],method = c("center","scale"))
train_data=predict(std_data,train)
test_data=predict(std_data,test)

table(data_balanced_over)

library(randomForest)


fit<- randomForest(Criminal~.,data=data_balanced_both,ntrees = 1500, mtries = 15, max_depth = 10, seed = 21)
##fit<- randomForest(Criminal~.,data=data.smote,ntrees = 2500, mtries = 8, max_depth = 10, seed = 21)
summary(fit)
importance(fit)
predicted<-predict(fit, newdata = test)

test_prediction1 <- matrix(pred3)
pred3<-predict(fit, newdata = test1)


predicted1 <- ifelse(test_prediction1 >= 0.5,1,0)
confusionMatrix(test$Criminal,predicted) 


library(caret)
std_data=preProcess(data_balanced_both[,!(names(data_balanced_both)%in% "Criminal")],method = c("center","scale"))
train_data=predict(std_data,data_balanced_both)
test_data=predict(std_data,test)

library(xgboost)
train_xgb=xgb.DMatrix(data=as.matrix(train_data[,!(names(train_data) %in% "Criminal")]),
                     label=as.matrix(train_data[,names(train_data)%in%"Criminal"]))

test_xgb=xgb.DMatrix(data=as.matrix(test_data[,!(names(test_data) %in% "Criminal")]),
                    label=as.matrix(test_data[,names(test_data) %in% "Criminal"]))


# train a model using our training data
model <- xgboost(data = train_xgb, # the data   
               nround = 100, # max number of boosting iterations
              objective = "binary:logistic")  # the objective function

pred2 <- predict(model, test_xgb)
test_pred<-matrix(pred2)
pred2 <- ifelse(pred2 > 0.6,1,0)
confusionMatrix(test_data$Criminal, pred2) 

test1=predict(std_data,test1)
test2=xgb.DMatrix(data=as.matrix(test1))
pred=predict(model,test2)
pred <- ifelse(pred > 0.6,1,0)

roc.curve(test_data$Criminal, pred2)
## create submission file
#testId = test1$PERID
#test1=predict(std_data,test1)
#submission = data.frame(PERID = testId)
#ubmission$Criminal = as.integer(pred1)
#write.csv(submission, "Submission.csv", row.names = FALSE)
write.csv(pred, "testpred.csv", row.names = FALSE)

write.csv(predicted1, "testpred1.csv", row.names = FALSE)
