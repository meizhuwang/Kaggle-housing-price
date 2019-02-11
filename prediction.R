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
library(dummies)


setwd('/Users/Desktop/all')
housing_con <- fread("housing_con.csv")[, -c(1, 4)]
housing_cat <- fread("housing_cat.csv")[, -c(1, 4)]

housing <- cbind(housing_cat, housing_con$SalePrice)
names(housing)[29] <- "SalePrice_con"


set.seed(12345)
train_index <- sample(seq_len(nrow(housing)), size=2*(nrow(housing)/3))
temp<-housing[train_index,]
test1_index <- sample(seq_len(nrow(temp)), size=nrow(temp)/2)
train<-temp[test1_index,]
test1 <- temp[-test1_index,]
housing_con <- rbind(train, test1)
test2 <- housing[-train_index,]


### When price range less than 100,000, we use ensemble method, otherwose we will use random forest

# previously we picked LDA as our model to predict which price range the test data falls into

pred <- c()
train_cat <- rbind(train, test1)

for (i in 1:nrow(test2)){
  print(i)
  test_cat <- test2[i, ]
  SalePrice_LDA <- lda(SalePrice ~ ., data=train_cat)
  # test predictions
  level <- as.character(predict(SalePrice_LDA, test_cat)$class)
  train <- dummy.data.frame(train_cat[, -c(28, 29)], sep = ".")
  test <- dummy.data.frame(test_cat[, -c(28, 29)], sep = ".")
  colnames(train)[1] <- "MSZoning.C"
  colnames(test)[1] <- "MSZoning.C"
  
  
  train <- cbind(train, train_cat$SalePrice_con)
  names(train)[ncol(train)] <- "SalePrice"
  test <- cbind(test, test_cat$SalePrice_con)
  names(train)[ncol(test)] <- "SalePrice"
  
  
  if (level=="less than 100,000"){
    ## ridge
    ridgemod <- glmnet(data.matrix(train[, -ncol(train)]),train$SalePrice,
                         alpha=0,lambda=0.1)
    newX <- data.matrix(test[, -ncol(test)])
    ridge.pred <- predict(ridgemod, newX)    
    ## lasso
    lasso_mods <- glmnet(data.matrix(train[, -ncol(train)]),train$SalePrice,
                         alpha=1,lambda=0.1)
    newX <- data.matrix(test[, -ncol(test)])
    lasso.pred <- predict(lasso_mods, newX, s="lambda.min")
    
    ## elastic net
    elastic_mod <- glmnet(data.matrix(train[, -ncol(train)]),train$SalePrice,
                          alpha=0.5,lambda=0.1)
    newX <- data.matrix(test[, -ncol(test)])
    elastic.pred <- predict(elastic_mod, newX)
    
    ## random forest
    housing_con.rf <- randomForest(SalePrice ~ ., mtry=2, ntree=100, data=train)
    rf.pred <- predict(housing_con.rf, test[, -ncol(test)])

    ## ensemble
    Ridge_weight <- 0.2259679
    Lasso_weight <- 0.2541327
    Elastic_weight <- 0.2541327
    RandomF_weight <- 0.2657668
    ensemble.pred <- ridge.pred*Ridge_weight + lasso.pred*Lasso_weight + 
      elastic.pred*Elastic_weight + rf.pred*RandomF_weight
    pred <- c(pred, ensemble.pred)
  } else if (level=="100,000 to 150,000"){
    housing_con.rf <- randomForest(SalePrice ~ ., mtry=6, ntree=300, data=train)
    rf.pred <- predict(housing_con.rf, test[, -ncol(test)])
    pred <- c(pred, rf.pred)
  } else if (level=="more than 200,000"){
    housing_con.rf <- randomForest(SalePrice ~ ., mtry=10, ntree=200, data=train)
    rf.pred <- predict(housing_con.rf, test[, -ncol(test)])
    pred <- c(pred, rf.pred)
  } else if (level=="150,000 to 200,000"){
    housing_con.rf <- randomForest(SalePrice ~ ., mtry=15, ntree=400, data=train)
    rf.pred <- predict(housing_con.rf, test[, -ncol(test)])
    pred <- c(pred, rf.pred)
  }
}

pred_final <- as.data.frame(pred)
sqrt(sum((test2$SalePrice_con-pred_final$pred)^2)/nrow(test2))
# 0.216
write.csv(pred_final, file="pred_final.csv")





