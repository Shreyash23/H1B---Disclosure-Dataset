library(lubridate)
library(readr)
library(PerformanceAnalytics)
library(sqldf)
library(DMwR)
library(mice)
library(Hmisc)
library(unbalanced)
library(caret)
library(glmnet)
library(data.table)
library(dplyr)
library(fastDummies)
library(ROCR)
library(ROSE)
library(corrr)
library(keras)
library(tidyverse) 
library(tibble)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(rsample)
library(corrplot)
install.packages("devtools")  ## get newest
devtools::install_github("rstudio/keras")  ## you need to install keras from here
library(keras)
install_keras()

#Data Preparation
X1_Master_H1B_Dataset <- read_csv("C:/Users/shrey/Desktop/h1b-disclosure-dataset (1)/H1B Disclosure Dataset Files/H1B Disclosure Dataset Files/1. Master H1B Dataset.csv")
summary(X1_Master_H1B_Dataset)

X1_Master_H1B_Dataset$CASE_SUBMITTED_DATE <- as.Date(paste(X1_Master_H1B_Dataset$CASE_SUBMITTED_YEAR,X1_Master_H1B_Dataset$CASE_SUBMITTED_MONTH,X1_Master_H1B_Dataset$CASE_SUBMITTED_DAY,sep = '-'))
X1_Master_H1B_Dataset$DECISION_DATE <- as.Date(paste(X1_Master_H1B_Dataset$DECISION_YEAR,X1_Master_H1B_Dataset$DECISION_MONTH,X1_Master_H1B_Dataset$DECISION_DAY,sep = '-'))

str(X1_Master_H1B_Dataset$CASE_SUBMITTED_DATE)
str(X1_Master_H1B_Dataset$DECISION_DATE)

#ANALYSIS OF EMPLOYER NAME
str(X1_Master_H1B_Dataset$EMPLOYER_NAME)
DATA_2016 <- X1_Master_H1B_Dataset[X1_Master_H1B_Dataset$DECISION_YEAR==2016,]
DATA_2017 <- X1_Master_H1B_Dataset[X1_Master_H1B_Dataset$DECISION_YEAR==2017,]
head(sort(table(DATA_2016$EMPLOYER_NAME),decreasing = T),n=40)
Top_emp <- as.data.frame(head(sort(table(DATA_2017$EMPLOYER_NAME),decreasing = T),n=40))

#Making Dummies for Top employers
DATA_2017[which(!(DATA_2017$EMPLOYER_NAME %in% Top_emp$Var1)),8] <- "Others"
DATA_2017 <- dummy_cols(DATA_2017,select_columns = "EMPLOYER_NAME",remove_most_frequent_dummy=T)


#ANALYSIS OF VISA CLASS
str(DATA_2017$VISA_CLASS)
DATA_2017$VISA_CLASS <- as.factor(DATA_2017$VISA_CLASS)
str(DATA_2017$VISA_CLASS)
table(DATA_2017$VISA_CLASS)

#ANALYSIS OF EMPLOYER COUNTRY AND STATE
DATA_2017$EMPLOYER_COUNTRY <- as.factor(DATA_2017$EMPLOYER_COUNTRY)
DATA_2017$EMPLOYER_STATE <- as.factor(DATA_2017$EMPLOYER_STATE)
nrow(table(DATA_2017$EMPLOYER_COUNTRY))
nrow(table(DATA_2017$EMPLOYER_STATE))
sort(table(DATA_2017$EMPLOYER_COUNTRY),decreasing = T)
sort(table(DATA_2017$EMPLOYER_STATE),decreasing = T)
#Making dummy variables for states
DATA_2017 <- dummy_cols(DATA_2017,select_columns = "EMPLOYER_STATE",remove_most_frequent_dummy=T)

#ANALYSIS OF OCCUPATIONAL NAME(SOC_NAME)
DATA_2017$SOC_NAME <- as.factor(DATA_2017$SOC_NAME)
nrow(table(DATA_2017$SOC_NAME))
head(sort(table(DATA_2017$SOC_NAME),decreasing = T),n=10)

#ANALYSIS OF TOTAL_WORKER
str(DATA_2017$TOTAL_WORKERS)
table(DATA_2017$TOTAL_WORKERS)

#ANALYSIS OF FULL TIME INDICATOR
str(DATA_2017$FULL_TIME_POSITION)
DATA_2017$FULL_TIME_POSITION <- as.factor(DATA_2017$FULL_TIME_POSITION)
table(DATA_2017$FULL_TIME_POSITION)

#ANALYSIS OF PREVAILING_WAGE, PW_UNIT_OF_PAY, PW_SOURCE , PW_SOURCE_YEAR
str(DATA_2017$PREVAILING_WAGE)

str(DATA_2017$PW_UNIT_OF_PAY)
DATA_2017$PW_UNIT_OF_PAY <- as.factor(DATA_2017$PW_UNIT_OF_PAY)
str(DATA_2017$PW_UNIT_OF_PAY)
table(DATA_2017$PW_UNIT_OF_PAY)

str(DATA_2017$PW_SOURCE)
DATA_2017$PW_SOURCE <- as.factor(DATA_2017$PW_SOURCE)
str(DATA_2017$PW_SOURCE)
table(DATA_2017$PW_SOURCE)

str(DATA_2017$PW_SOURCE_YEAR)
DATA_2017$PW_SOURCE_YEAR <- as.factor(DATA_2017$PW_SOURCE_YEAR)
str(DATA_2017$PW_SOURCE_YEAR)
table(DATA_2017$PW_SOURCE_YEAR)

#ANALYSIS OF H1B_DEPENDENT INDICATOR
colnames(DATA_2017)[23] <- "H1B_DEPENDENT"
str(DATA_2017$H1B_DEPENDENT)
DATA_2017$H1B_DEPENDENT <- as.factor(DATA_2017$H1B_DEPENDENT)
str(DATA_2017$H1B_DEPENDENT)
table(DATA_2017$H1B_DEPENDENT)

#ANALYSIS OF WILLFUL_VIOLATOR
str(DATA_2017$WILLFUL_VIOLATOR)
DATA_2017$WILLFUL_VIOLATOR <- as.factor(DATA_2017$WILLFUL_VIOLATOR)
str(DATA_2017$WILLFUL_VIOLATOR)
table(DATA_2017$WILLFUL_VIOLATOR)

#ANALYSIS OF CASE_STATUS
str(DATA_2017$CASE_STATUS)
DATA_2017$CASE_STATUS <- as.factor(DATA_2017$CASE_STATUS)
str(DATA_2017$CASE_STATUS)
table(DATA_2017$CASE_STATUS)

#REMOVING REDUNDANT COLUMNS
DATA_2017$EMPLOYER_STATE <- NULL
DATA_2017$EMPLOYER_NAME <- NULL
DATA_2017$EMPLOYER_COUNTRY <- NULL
DATA_2017$CASE_SUBMITTED_DATE <- NULL
DATA_2017$DECISION_DATE <- NULL
DATA_2017$DECISION_DAY <- NULL
DATA_2017$DECISION_MONTH <- NULL
DATA_2017$DECISION_YEAR <- NULL
DATA_2017$NAICS_CODE <- NULL
DATA_2017$PW_UNIT_OF_PAY <- NULL
DATA_2017$PW_SOURCE_OTHER <- NULL
DATA_2017$WAGE_RATE_OF_PAY_TO <- NULL
DATA_2017$WAGE_UNIT_OF_PAY <- NULL
DATA_2017$WORKSITE_STATE <- NULL
DATA_2017$WORKSITE_POSTAL_CODE <- NULL

summary(DATA_2017)
str(DATA_2017)
#ALL THE FEATURES ARE NOW IN CORRECT FORM

#CHECKING THE NULL VALUES
apply(DATA_2017,2,function(x) round(sum(is.na(x))/nrow(DATA_2017)*100,2))
#As per the above values, the missing values percent with respect to the entire dataset is negligible.
#Hence we delete the rows with missing values

#Omit the rows with null values
DATA_2017 <- na.omit(DATA_2017)
apply(DATA_2017,2,function(x) round(sum(is.na(x))/nrow(DATA_2017)*100,2))
apply(DATA_2017,2,function(x) round(sum(is.null(x))/nrow(DATA_2017)*100,2))

#Having a look at our target variable again.
str(DATA_2017$CASE_STATUS)
barplot(table(DATA_2017$CASE_STATUS))
#As we are only interested in whether the request will be certified or not.
#We club the certified and certified withdrawn factors and delete teh rows which were withdrawn as they dont add any meaning to the analysis.
for(i in 1:nrow(DATA_2017)){
  if (DATA_2017$CASE_STATUS[i]=="CERTIFIEDWITHDRAWN"){
    DATA_2017$CASE_STATUS[i]="CERTIFIED"
  }
}

DATA_2017 <- DATA_2017[-which(DATA_2017$CASE_STATUS=="WITHDRAWN"),]

#Droping the unused factors
DATA_2017$CASE_STATUS <- factor(DATA_2017$CASE_STATUS)
#The "Certified" factor is dominating
#Hence we need to balance the dataset
#We will do this while training the model using the caret package using SMOTE
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE)
ctrl$sampling <- 'smote'

#Splitting the data into training and testing set
set.seed(1234)
ind = sample(2, nrow(DATA_2017), replace = T, prob = c(0.6, 0.4))
TrainData = DATA_2017[ind == 1, ]
TestData = DATA_2017[ind == 2, ]
TrainData <- as.data.table(TrainData)
TestData <- as.data.table(TestData)

#Temporary Deletion of Test Data
save.image(file="Temp.RData")

Sample_TrainData <- sample_n(TrainData,size=(nrow(TrainData)/100)*5,replace = T)
#DELETING DATASETS WHICH ARE OF NO USE FOR FREEING UP THE MEMORY
rm(X1_Master_H1B_Dataset)
rm(DATA_2016)
rm(DATA_2017)
rm(i)
rm(ind)
rm(Top_emp)
rm(Sample_TrainData)

#Saving Data Image before proceeding
save.image(file="TrainAndTestData.RData")
rm(list=ls())
load(file="TrainAndTestData.RData")

# #Checking the significant variables
# set.seed(1234)
# null = glm(CASE_STATUS~1, data= TrainData,family = "binomial") # only includes intercept
# full = glm(CASE_STATUS~., data= TrainData,family = "binomial") # includes all the
# # We can perform step-wise selection using the command:
# step(null, scope=list(lower=null, upper=full), direction="both")

#Building models
glmStepAIC_Model <- train(CASE_STATUS~.,TrainData,method="glmStepAIC",metric="ROC",trControl=ctrl)
saveRDS(glmStepAIC_Model,file="glmStepAIC_Model.RData")
rm(glmStepAIC_Model)

RF_Model <- train(CASE_STATUS~.,TrainData,method="rf",metric="ROC",trControl=ctrl)
saveRDS(RF_Model,file="RF_Model.RData")
rm(RF_Model)

glm_Model <- train(CASE_STATUS~.,TrainData,method="glm",metric="ROC",trControl=ctrl)
saveRDS(glm_Model,file="glm_Model.RData")
rm(glm_Model)

svmLinear_Model <- train(CASE_STATUS~.,TrainData,method="svmLinear",metric="ROC",trControl=ctrl)
saveRDS(svmLinear_Model,file="svmLinear_Model.RData")
rm(svmLinear_Model)

adaboost_Model <- train(CASE_STATUS~.,TrainData,method="adaboost",metric="ROC",trControl=ctrl)
saveRDS(adaboost_Model,file="adaboost_Model.RData")
rm(adaboost_Model)

nnet_Model <- train(CASE_STATUS~.,TrainData,method="nnet",metric="ROC",trControl=ctrl)
saveRDS(nnet_Model,file="nnet_Model.RData")
rm(nnet_Model)

xgbDART_Model <- train(CASE_STATUS~.,TrainData,method="xgbDART",metric="ROC",trControl=ctrl)
saveRDS(xgbDART_Model,file="xgbDART_Model.RData")
rm(xgbDART_Model)

svmradial_Model <- train(CASE_STATUS~.,TrainData,method="svmRadial",metric="ROC",trControl=ctrl)
saveRDS(svmRadial_Model,file="svmRAdial__Model.RData")
rm(svmRadial_Model)

#Model Comparison
nnet_Model <- readRDS("nnet_Model.rds")
svmLinear_Model <- readRDS("svmLinear_Model.rds")
glm_Model <- readRDS("glm_Model.rds")
RF_Model <- readRDS("RF_Model.rds")
xgbDART_Model <- readRDS("xgbDART_Model.rds")
svmRadial_Model <- readRDS("svmRadial_Model.rds")

adaboost_Model <- readRDS("adaboost_Model.rds")

#Model Summary
nnet_Model
svmLinear_Model
glm_Model
RF_Model
xgbDART_Model
svmRadial_Model

#Model Evaluation
nnet.pred <- predict(nnet_Model,newdata = TestData)
svm.pred <- predict(svmLinear_Model,newdata = TestData)
glm.pred <- predict(glm_Model,newdata = TestData)
rf.pred <- predict(RF_Model,newdata = TestData)
xgb.pred <- predict(xgbDART_Model,newdata = TestData)
svmRBF.pred <- predict(svmRadial_Model,newdata = TestData)

#ROC curves
roc.curve(TestData$CASE_STATUS,nnet.pred)
roc.curve(TestData$CASE_STATUS,svm.pred)
roc.curve(TestData$CASE_STATUS,glm.pred)
roc.curve(TestData$CASE_STATUS,rf.pred)
roc.curve(TestData$CASE_STATUS,xgb.pred)
roc.curve(TestData$CASE_STATUS,svmRBF.pred)

confusionMatrix(TestData$CASE_STATUS,nnet.pred)
#Accuracy : 0.9142
confusionMatrix(TestData$CASE_STATUS,svm.pred)
#Accuracy : 0.9874
confusionMatrix(TestData$CASE_STATUS,glm.pred)
#Accuracy : 0.915
confusionMatrix(TestData$CASE_STATUS,rf.pred)
#Accuracy : 0.9873
confusionMatrix(TestData$CASE_STATUS,xgb.pred)
#Accuracy : 0.9641
confusionMatrix(TestData$CASE_STATUS,svmRBF.pred)
#Accuracy : 0.8633

#svmLinear>rf>xgb>glm>nnet>svmRBF
#Clearing the memory before moving on
rm(ctrl)
rm(glm_Model)
rm(glm.pred)
rm(nnet_Model)
rm(nnet.pred)
rm(RF_Model)
rm(rf.pred)
rm(svmLinear_Model)
rm(svm.pred)
rm(xgbDART_Model)
rm(xgb.pred)
rm(svmRadial_Model)
rm(svmRBF.pred)

#Part 2
#Implementing keras
load(file="TrainAndTestData.RData")
#Removing spaces from the column names
names(TrainData) <- gsub(" ", "_", names(TrainData))
names(TestData) <- gsub(" ", "_", names(TestData))

#Balancing the data
table(TrainData$CASE_STATUS)
#TrainSplit <- SMOTE(CASE_STATUS ~ ., TrainData, perc.over = 100, perc.under=100)
TrainSplit <- ROSE(CASE_STATUS~.,TrainData, seed = 123,p = 0.45)$data
table(TrainSplit$CASE_STATUS)

#Using TrainSplit hencforth
DF = as.matrix(as.data.frame(lapply(TrainSplit, as.numeric)))
DF_Test = as.matrix(as.data.frame(lapply(TestData, as.numeric)))
# Create recipe
rec_obj <- recipe(CASE_STATUS ~ ., data = DF) %>%
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = DF)

# Predictors
x_train_tbl <- bake(rec_obj, newdata = DF) %>% select(-CASE_STATUS)
x_test_tbl  <- bake(rec_obj, newdata = DF_Test) %>% select(-CASE_STATUS)

# Response variables for training and testing sets
# y_train_vec <- ifelse(pull(TrainSplit, CASE_STATUS) == "CERTIFIED", 1, 0)
# y_test_vec  <- ifelse(pull(TestData, CASE_STATUS) == "CERTIFIED", 1, 0)
y_train_vec <- ifelse(TrainSplit$CASE_STATUS== "CERTIFIED", 1, 0)
y_test_vec  <- ifelse(TestData$CASE_STATUS== "CERTIFIED", 1, 0)
table(y_train_vec)
table(y_test_vec)
#Clearing memory before proceeding
rm(DF)
rm(DF_Test)
rm(TrainSplit)
rm(rec_obj)
rm(TrainData)
rm(TestData)

#To retrive the original Train and test data
#load(file="TrainAndTestData.RData")
#Checking the correlation
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_mat <- rcorr(as.matrix(x_train_tbl))
flattenCorrMatrix(cor_mat$r, cor_mat$P)

#We do observe correlation
rm(flattenCorrMatrix)
rm(cor_mat)
#Getting rif of the correlation to avoid multi-colinearity - Computing PCA's
prin_comp <- prcomp(x_train_tbl,scale. = T)
pr_var <- (prin_comp$sdev)^2
prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

x_train_tbl_pca <- prin_comp$x
x_train_tbl_pca <- as.data.frame(x_train_tbl_pca)
#Converting test data set to pca
x_test_tbl_pca <- as.data.frame(predict(prin_comp,newdata = x_test_tbl))

#glimpse(x_train_tbl_pca)
#Removing redundant variables
rm(pr_var)
rm(prin_comp)
rm(prop_varex)

# Build an Artificial Neural Network and trying on both pca and non-pca datasets
model_keras <- keras_model_sequential()
set.seed(123)
model2 <- model_keras %>%
  layer_dense(units = 32, kernel_initializer = "uniform", activation = "relu",
              input_shape = ncol(x_train_tbl)) %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 256, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.30) %>%
  layer_dense(units = 256, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 16, kernel_initializer = "uniform", activation = "relu",kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 1, kernel_initializer = "uniform", activation = "sigmoid")
# Compile 
compile2 <- model_keras %>%
  compile(optimizer = "adam", loss = "binary_crossentropy", metrics = c("binary_accuracy"))

compile2
#Decreasing the learning rate
k_set_value(model2$optimizer$lr,0.00001)

kemodel <- fit(object = model2,
               x = as.matrix(x_train_tbl),
               y = y_train_vec,
               batch_size = 128,
               epochs = 20,
               validation_split = 0.30,
               shuffle=TRUE)


#model summary
kemodel
plot(kemodel)
# Predicted class
pred_class <- predict_classes(object = model2,
                              x = as.matrix(x_test_tbl)) %>%
  as.vector()
# Predicted class probability

pred_prob <- predict_proba(object = model2,
                           x = as.matrix(x_test_tbl)) %>%
  as.vector()

estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec),
  estimate   = as.factor(pred_class),
  class_prob = pred_prob
)

# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)


# Accuracy
estimates_keras_tbl %>% confus(truth, estimate)
#Accuracy=0.3383836
# AUC (Area Under Curve)
estimates_keras_tbl %>% roc_auc(truth, class_prob)

###
####Trying on the PCA data
###
model3 <- model_keras %>%
  layer_dense(units = 128, kernel_initializer = "uniform", activation = "relu",
              input_shape = ncol(x_train_tbl_pca)) %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 64, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 32, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 32, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 1, kernel_initializer = "uniform", activation = "sigmoid")
# Compile 
compile3 <- model_keras %>%
  compile(optimizer = "adam", loss = "binary_crossentropy", metrics = c("binary_accuracy"))

compile3
#Decreasing the learning rate
k_set_value(model3$optimizer$lr,0.01)

kemodel <- fit(object = model3,
               x = as.matrix(x_train_tbl_pca),
               y = y_train_vec,
               batch_size = 64,
               epochs = 20,
               validation_split = 0.30,
               shuffle=TRUE)


#model summary
kemodel
plot(kemodel)
# Predicted class
pred_class <- predict_classes(object = model3,
                              x = as.matrix(x_test_tbl_pca)) %>%
  as.vector()
# Predicted class probability

pred_prob <- predict_proba(object = model3,
                           x = as.matrix(x_test_tbl_pca)) %>%
  as.vector()

estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec),
  estimate   = as.factor(pred_class),
  class_prob = pred_prob
)

# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)


# Accuracy
estimates_keras_tbl %>% metrics(truth, estimate)
#Accuracy=0.3383836
# AUC (Area Under Curve)
estimates_keras_tbl %>% roc_auc(truth, class_prob)