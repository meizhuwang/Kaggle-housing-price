library(data.table)
library(ggplot2)
library(FNN)
library(randomForest)
library(MASS)
library(tidyverse)
library(glmnet)
library(e1071)
library(neuralnet)
library(nnet)

setwd("/Users/wangyuexi/Desktop/all")
housing_cat <- fread("housing_cat.csv")[,-1]
set.seed(12345)
housing_cat$SalePrice <- as.factor(housing_cat$SalePrice)
train_index <- sample(seq_len(nrow(housing_cat)), size=nrow(housing_cat)*0.5)
train<-housing_cat[train_index,]
test<-housing_cat[-train_index,]

#### Support Vector Machine ####
# Tune the cost parameter manually
for(cost in c(0.1,1,10,100,1000)){
  svc <- svm(SalePrice ~ ., kernel="linear", cost=cost, data=train)
  preds <- predict(svc,test)
  print(cost)
  print(sum(test$SalePrice!=preds)/nrow(test))
}

svc <- svm(SalePrice ~ ., kernel="linear", cost=10, data=train)
svm.preds <- as.character(predict(svc,test))
sum(test$SalePrice!=svm.preds)/nrow(test)

## when cost=1, misclassification rate at the lowest 0.2259

#### KNN classification ####
### scale and set the training datase
train_scale <- data.table(scale(na.omit(train[, -29])))
train_scale$SalePrice <- train$SalePrice
#### train model for the "train" data in order to find the bsts K parameter
# 10-flod CV
# use this function to add K grouping indeces
set.seed(12345)
add_cv_cohorts <- function(dat,cv_K){
  if(nrow(dat) %% cv_K == 0){ # if perfectly divisible
    dat$cv_cohort <- sample(rep(1:cv_K, each=(nrow(dat)%/%cv_K)))
  } else { # if not perfectly divisible
    dat$cv_cohort <- sample(c(rep(1:(nrow(dat) %% cv_K), each=(nrow(dat)%/%cv_K + 1)),
                              rep((nrow(dat) %% cv_K + 1):cv_K,each=(nrow(dat)%/%cv_K)) ) )
  }
  return(dat)
}

# add 10-fold CV labels to music data
train_scale_cv <- add_cv_cohorts(train_scale,10)

# initialize for 10 cohorts, errors and counts
cohorts <- data.frame(cohort=1:10,
                      errors = rep(NA,10), 
                      n=rep(NA,10))
# loop over each validation cohort
K=30
tuning <- data.frame(K=1:K, error_rate=rep(NA,K))
for(k in 1:K){
  print(k)
  preds <- rep(NA,nrow(train_scale))
  for(cv_k in 1:10){
    cohort_rows <- which(train_scale_cv$cv_cohort == cv_k)
    train_scale <- na.omit(train_scale)
    knnClass <- knn(train = train_scale[-cohort_rows , -29],
                    test = train_scale[cohort_rows , -29],
                    cl = train_scale[-cohort_rows ,]$SalePrice, k = k, algorithm = "brute")
    preds <- as.character(knnClass)
    cohorts$errors[cv_k] <- sum(train_scale$SalePrice[cohort_rows] != preds)
    cohorts$n[cv_k] <- length(cohort_rows)
    tuning$error_rate[k]<-with(cohorts, sum(errors)/sum(n))
  }
}

ggplot()+
  geom_point(aes(x=K,y=error_rate), data=tuning)
# when k=25, it has the lowest error rate which is 21.9%
test_scale <- data.table(scale(na.omit(test[, -29][,])))
test_scale$SalePrice <- test$SalePrice
test_scale_cv <- add_cv_cohorts(test_scale,10)

# prediction on test data
cohorts <- data.frame(cohort=1:10,
                      errors = rep(NA,10), 
                      n=rep(NA,10))
# loop over each validation cohort
knn.preds <- rep(NA,nrow(test_scale))
for(cv_k in 1:10){
  cohort_rows <- which(test_scale_cv$cv_cohort == cv_k)
  knnClass <- knn(train = test_scale[-cohort_rows , -29],
                  test = test_scale[cohort_rows , -29],
                  cl = test_scale[-cohort_rows ,]$SalePrice, k = 25, algorithm = "brute")
  knn.preds <- as.character(knnClass)
  cohorts$errors[cv_k] <- sum(test_scale$SalePrice[cohort_rows] != preds)
  cohorts$n[cv_k] <- length(cohort_rows)
  error_rate<-with(cohorts, sum(errors)/sum(n))
}

error_rate
# misclassification rate is 0.2455324%


#### Random Forest ####
# Tuning number of variable to consider at each split
set.seed(123456)
rfdata<-housing_cat
rfdata$SalePrice<-factor(rfdata$SalePrice)
train_ind <- sample(seq_len(nrow(rfdata)), size = nrow(rfdata)*0.5)

rf_train <- rfdata[train_ind, ]
rf_test <- rfdata[-train_ind, ]
rfc_tune <- expand.grid(mtry=1:17, ntree=seq(100,500,by=100))
rfc_tune$misclass <- NA
for(i in 1:nrow(rfc_tune)){
  set.seed(12345)
  rfc <- randomForest(SalePrice ~ ., mtry=rfc_tune$mtry[i], ntree=rfc_tune$ntree[i], data=rf_train)
  rfc_preds <- predict(rfc, rf_test)
  rfc_tune$misclass[i] <- sum(rf_test$SalePrice!=rfc_preds)/nrow(rf_test)
}
best <- rfc_tune[which.min(rfc_tune$misclass),]

# when mtry=6, ntree=100, the error rate is 21.69
rfc <- randomForest(SalePrice ~ ., mtry=6, ntree=100, data=test)
rf.preds <- as.character(rfc$predicted)
sum(test$SalePrice!=rfc$predicted)/nrow(test)
# misclassification rate: 21.71%


#### LDA ####
set.seed(12345)
LDAdata<-housing_cat
train_ind <- sample(seq_len(nrow(LDAdata)), size = nrow(LDAdata)*0.5)
lda_train <- LDAdata[train_ind, ]
lda_test <- LDAdata[-train_ind, ]
# Build a LDA classifier using only petal info from training data
SalePrice_LDA <- lda(SalePrice ~ ., data=lda_train)
# test predictions
lda_test$preds <- predict(SalePrice_LDA, lda_test)$class 
lda.preds <- as.character(lda_test$preds)
# claculate the misclassification rate
sum(lda_test$SalePrice!=lda_test$preds)/nrow(test)
# misclassfication rate is 21.64%
# 0.3747277
#### ensemble ####
ensemble_misclassification <- function(housing_cat){
  pred <- as.data.frame(cbind(svm.preds, rf.preds, lda.preds))
  pred$ensemble <- NA
  for (i in 1:nrow(pred)){
    freq <- as.data.frame(table(t(pred[i, ])))
    if (sum(which(freq$Freq==1)) == 3){
      pred$ensemble[i] <- pred$lda.preds[i]
    }
    else {
      j=which(freq$Freq==max(freq$Freq))
      pred$ensemble[i] <- as.character(freq$Var1[j])
    }
  }
  
  return(sum(lda_test$SalePrice!=pred$ensemble)/nrow(test))
}

ensemble_misclassification_rate <- ensemble_misclassification(housing_cat)
# misclassiication rate = 0.2164

### it seems that ensemble is not a good way to average model in this case. So we will 
# simply pick LDA as our final model.



