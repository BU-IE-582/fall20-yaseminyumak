setwd("C:/Users/lenovo/Desktop/IE582/HW4")
library("readxl")
data1=read_excel("DataSet1/Absenteeism.xls")
set.seed(1)
data1$ID <- NULL
names(data1) <- gsub(" ", ".", names(data1))
colnames(data1)[colnames(data1) == "Absenteeism.time.in.hours"] <- "AbsenteeismTimeInHours"
colnames(data1)[colnames(data1) == "Work.load.Average/day"] <- "Workload"

head(data1)

str(data1)

names <- c(1,2,3,4,11,12,14,15)
data1[,names] <- lapply(data1[,names] , factor)
str(data1)

any(is.na(data1)==TRUE) ##controlling if there is any missing value

colnames(data1)[colnames(data1) == "Absenteeism time in hours"] <- "AbsenteeismTimeInHours"
library(glmnet)

sample <- sample.int(n = nrow(data1), size = floor(nrow(data1)*0.77), replace = F)
train1 <- data1[sample, ]
test1  <- data1[-sample, ]

folds1 <- createMultiFolds(train1$AbsenteeismTimeInHours,k=5,times = 1)
control1<-trainControl(method="cv", verboseIter=TRUE,index=folds2,allowParallel=TRUE)


lasso_grid1 <- expand.grid(alpha= 1, lambda = c(seq(1,400,length=3)))
set.seed(1)
reg1 <- train(AbsenteeismTimeInHours ~., data=train1, method="glmnet", tuneGrid= lasso_grid1, 
                 trControl= control1, preProcess=c("center","scale"))

lasso_pred1 <- predict(reg1,test1)

lasso_error1 <- RMSE(test1$AbsenteeismTimeInHours, lasso_pred1)
lasso_error1



library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

dt_grid1 <- expand.grid(cp=c(0.001,0.005,0.01))
set.seed(1)
dt1.1 <- train(AbsenteeismTimeInHours~., data=train1,method ="rpart",tuneGrid = dt_grid1,
             trControl=control2, control = rpart.control(minbucket=c(4)))
dt_pred1.1 <- predict(dt1.1,test1)
dt_error1.1 = RMSE(test1$AbsenteeismTimeInHours,dt_pred1.1)
dt_error1.1

dt1.2 <- train(AbsenteeismTimeInHours~., data=train1,method ="rpart",tuneGrid = dt_grid1,
             trControl=control2, control = rpart.control(minbucket=c(6)))

dt_pred1.2 <- predict(dt1.2,test1)
dt_error1.2 = RMSE(test1$AbsenteeismTimeInHours,dt_pred1.2)
dt_error1.1

dt1.3 <- train(AbsenteeismTimeInHours~., data=train1,method ="rpart",tuneGrid = dt_grid1,
             trControl=control2, control = rpart.control(minbucket=c(8)))
dt_pred1.3 <- predict(dt1.3,test1)

dt_error1.3=RMSE(test1$AbsenteeismTimeInHours,dt_pred1.3)
dt_error1.3

install.packages("cowplot")
library(cowplot)
require(randomForest)

forest_grid1 <- expand.grid(mtry = c(2,4,6) ,min.node.size=5, splitrule = "variance")
set.seed(1)
rf1 <- train(AbsenteeismTimeInHours ~., data=train1, method="ranger", tuneGrid= forest_grid1, trControl= control1)
rf_pred1 <- predict(rf1,test1)
rf_error1 = RMSE(test1$AbsenteeismTimeInHours,rf_pred1)
rf_error1

install.packages("gbm")
library(caret)
require(gbm)

grid_sgb1 <- expand.grid(interaction.depth=c(1,3,5), n.trees = c(100,200,300),shrinkage=c(0.05,0.01,0.02), n.minobsinnode=10)
set.seed(1)
sgb1 <- train(AbsenteeismTimeInHours~., data=train1, method="gbm", tuneGrid= grid_sgb1 , trControl= control1)

sgb_pred1 <- predict(sgb1,test1)
sgb_error1 <- RMSE(test1$AbsenteeismTimeInHours,sgb_pred1)
sgb_error1

Model = c("Lasso Reg", "Decision Tree 1","Decision Tree 2", "Decision Tree 3",
                "Random Forest", "Gradient Boosting")
RMSE_Value = c(lasso_error1, dt_error1.1, dt_error1.2, dt_error1.3, rf_error1, sgb_error1)

table1 = data.frame(Model,RMSE_Value)
table1

data2 <-read.csv("DataSet2/OnlineNewsPopularity.csv",sep=",",header=T)
data2[,1] <-NULL
head(data2)

str(data2)

set.seed(1)
sample2 <- sample.int(n = nrow(data2), size = floor(nrow(data2)*0.77), replace = F)
train2 <- data2[sample, ]
test2  <- data2[-sample, ]
head(train2)

folds2 <- createMultiFolds(train2$shares,k=10,times = 1)
control2<-trainControl(method="cv", verboseIter=TRUE,index=folds2,allowParallel=TRUE)

lasso_grid2 <- expand.grid(alpha= 1, lambda = c(seq(1,400,length=3)))
set.seed(1)
reg2 <- train(shares ~., data=train2, method="glmnet", tuneGrid= lasso_grid2, 
                 trControl= control2, preProcess=c("center","scale"))

pred2 <- predict(reg2,test2)

lasso_error2 <- RMSE(test2$shares, pred2)
lasso_error2


grid_dt2 <- expand.grid(cp=c(0.001,0.005,0.01))
set.seed(1)
dt2.1 <- train(shares~., data=train2,method ="rpart",tuneGrid = grid_dt2,
             trControl=control2, control = rpart.control(minbucket=c(2)))
pred2.1 <- predict(dt2.1,test2)
dt_error2.1=RMSE(test2$shares,pred2.1)

dt2.2 <- train(shares~., data=train2,method ="rpart",tuneGrid = grid_dt2,
             trControl=control2, control = rpart.control(minbucket=c(4)))
pred2.2 <- predict(dt2.2,test2)
dt_error2.2 <- RMSE(test2$shares,pred2.2)

dt2.3 <- train(shares~., data=train2,method ="rpart",tuneGrid = grid_dt2,
             trControl=control2, control = rpart.control(minbucket=c(6)))
pred2.3 <- predict(dt2.3,test2)
dt_error2.3 <- RMSE(test2$shares,pred2.3)

install.packages('e1071', dependencies=TRUE)
install.packages("ranger")

forest_grid2 <- expand.grid(mtry = c(2,4,6) ,min.node.size=5, splitrule = "variance")
set.seed(123)
rf2 <- train(shares ~., data=train2, method="ranger", tuneGrid= forest_grid2, trControl= control2)
pred2 <- predict(rf2,test2)
rf_error2 = RMSE(test2$shares,pred2)

grid_sgb2 <- expand.grid(interaction.depth=c(1,3,5), n.trees = c(100,200,300),shrinkage=c(0.05,0.01,0.02), n.minobsinnode=10)
set.seed(1)
sgb2 <- train(shares ~., data=train2, method="gbm", tuneGrid= grid_sgb2 , trControl= control2)
pred2 <- predict(sgb2,test2)
sgb_error2 <- RMSE(test2$shares,pred2)

RMSE_Value_2 = c(lasso_error2, dt_error2.1, dt_error2.2, dt_error2.3, rf_error2, sgb_error2)

table2 = data.frame(Model,RMSE_Value_2)
table2

install.packages("MLmetrics")

data3 <-read.csv("DataSet3/DrugConsumption.csv",sep=",",header=T)
data3[,1]<- NULL
head(data3)

any(is.na(data3)==TRUE)

str(data3)

table(data3$VSA)

data3$VSA <- as.character(data3$VSA) 

data3$VSA[data3$VSA != "CL0"] <- "VSAused"
data3$VSA[data3$VSA == "CL0"] <- "VSAneverused"
head(data3$VSA)

table(data3$VSA) ## class imbalance problem (a ratio of 3:1)

names <- c(1,2,3,4,5,31)
data3[,names] <- lapply(data3[,names] , factor)


colnames(data3) <- make.names(colnames(data3))

nearZeroVar(data3, saveMetrics = TRUE)

set.seed(1)
sample <- sample.int(n = nrow(data3), size = floor(nrow(data3)*0.77), replace = F)
train3 <- data3[sample, ]
test3  <- data3[-sample, ]

set.seed(1)
fold3 <- createMultiFolds(train3$VSA,k=5,times =1)

control3<-trainControl(method="cv",verboseIter=TRUE,classProbs=TRUE,savePredictions=TRUE,index=fold3,allowParallel=TRUE)

str(train3)

lasso_grid3 <- expand.grid(alpha= 1, lambda = c(seq(0.001,0.1,length=6)))
reg3<-train(VSA~.,data=train3,method="glmnet",tuneGrid=lasso_grid3,
               trControl=control3)

lasso_pred3 <- predict(reg3,test3, type="prob")

lasso_error3 = roc(test3$VSA,lasso_pred3[,1])$auc


dt_grid3 <- expand.grid(cp=c(0.001,0.01,0.1))
dt3.1 <- train(VSA~., data=train3,method ="rpart",tuneGrid = dt_grid3 ,
             trControl=control3, control = rpart.control(minbucket=c(2)))

dt_pred3.1 <- predict(dt3.1,test3, type="prob")

dt_error3.1 = roc(test3$VSA,dt_pred3.1[,1])$auc

dt3.2 <- train(VSA~., data=train3,method ="rpart",tuneGrid = dt_grid3 ,
             trControl=control3, control = rpart.control(minbucket=c(8)))
dt_pred3.2 <- predict(dt3.2,test3, type="prob")

dt_error3.2 = roc(test3$VSA,dt_pred3.2[,1])$auc

dt3.3 <- train(VSA~., data=train3,method ="rpart",tuneGrid = dt_grid3 ,
             trControl=control3, control = rpart.control(minbucket=c(6)))
dt_pred3.3 <- predict(dt3.3,test3, type="prob")

dt_error3.3 = roc(test3$VSA,dt_pred3.3[,1])$auc

rf3 <- train(VSA ~., data=train3, method="ranger", tuneGrid= rf_grid3,
                 trControl= control3)
rf_pred3 <- predict(rf3,test3, type="prob")

rf_error3 <- roc(test3$VSA,rf_pred3[,1])$auc

sgb_grid3 <- expand.grid(interaction.depth=c(1,3,5), n.trees = c(100,200,300),shrinkage=c(0.1,0.01, 0.001), n.minobsinnode=10)
sgb3 <- train(VSA~., data=train3, method="gbm", tuneGrid= sgb_grid3,
              trControl= control3)
sgb_pred3 <- predict(sgb3,test3, type="prob")

sgb_error3 <- roc(test3$VSA,sgb_pred3[,1])$auc

AUC_Value = c(lasso_error3, dt_error3.1, dt_error3.2, dt_error3.3, rf_error3, sgb_error3)

table3 = data.frame(Model,AUC_Value)
table3

data4=read_excel("DataSet4/objectivity.xlsx")
head(data4)

data4$TextID <- NULL
data4$URL <- NULL

str(data4)

data4["Label"] <- lapply(data4["Label"] , factor)
colnames(data4) <- make.names(colnames(data4))

## "These variables have zero variances: NNP, WRB, ellipsis" 

data4$NNP <- NULL
data4$WRB <- NULL
data4$ellipsis <- NULL

set.seed(1)
sample <- sample.int(n = nrow(data4), size = floor(nrow(data4)*0.77), replace = F)
train4 <- data4[sample, ]
test4  <- data4[-sample, ]

set.seed(1)
fold4 <- createMultiFolds(train4$Label,k=10,times = 1)

control4<-trainControl(method="cv",verboseIter=TRUE,summaryFunction=twoClassSummary,
                              classProbs=TRUE,savePredictions=TRUE,index=fold4,allowParallel=TRUE)

lasso_grid4 <- expand.grid(alpha= 1, lambda = c(seq(0.001,0.1,length=5)))
reg4<-train(Label~.,data=train4,method="glmnet",tuneGrid=lasso_grid4,
               trControl=control4,preProcess=c("center","scale"))

install.packages("pROC")
library("pROC")

lasso_pred4 <- predict(reg4,test4,type="prob")
lasso_error4 <- roc(test4$Label,lasso_pred4[,1])$auc

set.seed(1)
dt_grid4 <- expand.grid(cp=c(0.001,0.01,0.1))
dt4.1 <- train(Label~., data=train4,method ="rpart",tuneGrid = dt_grid4,trControl=control4,
             control = rpart.control(minbucket=c(2)))

dt_pred4 <- predict(dt4.1,test4,type="prob")
dt_error4.1 <- roc(test4$Label,dt_pred4[,1])$auc

set.seed(1)
dt4.2 <- train(Label~., data=train4,method ="rpart",tuneGrid = dt_grid4,trControl=control4, 
             control = rpart.control(minbucket=c(4)))

dt_pred4.2 <- predict(dt4.2,test4,type="prob")
dt_error4.2 <- roc(test4$Label,dt_pred4.2[,1])$auc

set.seed(1)
dt4.3 <- train(Label~., data=train4,method ="rpart",tuneGrid = dt_grid4,trControl=control4, 
             control = rpart.control(minbucket=c(6)))

dt_pred4.3 <- predict(dt4.3,test4,type="prob")
dt_error4.3 <- roc(test4$Label,dt_pred4.3[,1])$auc

set.seed(1)
rf_grid4 <- expand.grid(mtry = c(2,4,6) ,min.node.size=5, splitrule = "gini")
rf4 <- train(Label ~., data=train4, method="ranger", tuneGrid= rf_grid4, 
                 trControl= control4)

rf_pred4 <- predict(rf4,test4,type="prob")
rf_error4 <- roc(test4$Label,rf_pred4[,1])$auc

set.seed(1)
sgb_grid4 <- expand.grid(interaction.depth=c(2,4,6), n.trees = c(100,200,300), shrinkage=c(0.001,0.01, 0.02), 
                         n.minobsinnode=10)
sgb4 <- train(Label~., data=train4, method="gbm", tuneGrid= sgb_grid4, trControl= control4)

sgb_pred4 <- predict(sgb4,test4,type="prob")
sgb_error4 <- roc(test4$Label,sgb_pred4[,1])$auc

AUC_Value = c(lasso_error4, dt_error4.1, dt_error4.2, dt_error4.3, rf_error4, sgb_error4)

table4 = data.frame(Model,AUC_Value)
table4


