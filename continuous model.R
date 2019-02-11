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

#### susbet data into three portion.
set.seed(1)
train_index <- sample(seq_len(nrow(housing_cat)), size=2*(nrow(housing_cat)/3))
temp<-housing_cat[train_index,]
test1_index <- sample(seq_len(nrow(temp)), size=nrow(temp)/2)
train_housing<-temp[test1_index,]
test1 <- temp[-test1_index,]
test2 <- housing_cat[-train_index,]

levels <- unique(test1$SalePrice)
MSE <- as.data.frame(matrix(NA, ncol=6, nrow=4))
colnames(MSE) <- c("level", "Ridge", "Lasso", "Elastic Net", "Random Forest", "Ensemble")
loop = 0
for (level in levels){
  set.seed(12345)
  index <- which(train_housing$SalePrice==level)
  housing <- housing_con[index, ]
  train_index <- sample(seq_len(nrow(housing)), size=nrow(housing)*0.5)
  print(level)
  loop = loop + 1
  MSE$level[loop] <- level
  housing.new <- dummy.data.frame(housing, sep = ".")
  colnames(housing.new)[1] <- "MSZoning.C"
  train<-housing.new[train_index,]
  train$SalePrice <- housing$SalePrice[train_index]
  test<-housing.new[-train_index,]
  test$SalePrice <- housing$SalePrice[-train_index]
  # delete variable if it has less than two level
  del <- c()
  for (i in names(train[, -ncol(train)])){
    if (length(table(train[[i]]))==1){
      del <- c(del, i)
    }
  }
  index <- which(colnames(train) %in% del)
  if (length(index)!=0){
    train <- train[, -index]
    test <- test[, -index]
  }
  
  ## ridge, tune parameter first. 
  ridge_mods <- cv.glmnet(as.matrix(train[,-ncol(train)]),train$SalePrice,
                          alpha=0,lambda=seq(.1,10,by=.1))
  ridge_lambda <- ridge_mods$lambda[which.min(ridge_mods$cvm)]
  ridgemod <- lm.ridge(SalePrice ~ . , data=train, lambda=ridge_lambda)
  ridge.pred <- as.matrix(cbind(const=1,test[,-ncol(train)])) %*% coef(ridgemod)
  MSE$Ridge[loop] <- sqrt(sum((test$SalePrice-ridge.pred)^2)/nrow(test))
  
  ## lasso, tune parameter first. 
  lasso_mods <- cv.glmnet(as.matrix(train[,-ncol(train)]),train$SalePrice,
                          alpha=1,lambda=seq(.1,10,by=.1))
  lasso_lambda <- lasso_mods$lambda[which.min(lasso_mods$cvm)]
  lasso_mods <- glmnet(data.matrix(train[, -ncol(train)]),train$SalePrice,
                       alpha=1,lambda=lasso_lambda)
  newX <- data.matrix(test[, -ncol(train)])
  lasso.pred <- predict(lasso_mods, newX, s="lambda.min")
  MSE$Lasso[loop] <- sqrt(sum((test$SalePrice-lasso.pred)^2)/nrow(test))
  
  ## elastic net, tune parameter first. 
  elastic_mods <- cv.glmnet(as.matrix(train[,-ncol(train)]),train$SalePrice,
                            alpha=0.5,lambda=seq(.1,10,by=.1))
  elastic_lambda <- elastic_mods$lambda[which.min(elastic_mods$cvm)]
  elastic_mod <- glmnet(data.matrix(train[, -ncol(train)]),train$SalePrice,
                        alpha=0.5,lambda=elastic_lambda)
  newX <- data.matrix(test[, -ncol(train)])
  elastic.pred <- predict(elastic_mod, newX)
  MSE$`Elastic Net`[loop] <- sqrt(sum((test$SalePrice-elastic.pred)^2)/nrow(test))
  
  ## random forest, tune paratemer. 
  train_ind <- sample(seq_len(nrow(train)), size = nrow(train)*0.5)
  rf_train <- train[train_ind, ]
  rf_test <- train[-train_ind, ]
  
  rfc_tune <- expand.grid(mtry=1:17, ntree=seq(100,500,by=100))
  rfc_tune$MSE <- NA
  for(i in 1:nrow(rfc_tune)){
    set.seed(1234)
    rfc <- randomForest(SalePrice ~ ., mtry=rfc_tune$mtry[i], ntree=rfc_tune$ntree[i], data=rf_train)
    rfc_preds <- predict(rfc, rf_test)
    rfc_tune$MSE[i] <- sqrt(sum((rf_test$SalePrice-rfc_preds)^2)/nrow(rf_test))
  }
  best <- rfc_tune[which.min(rfc_tune$MSE),]
  housing_con.rf <- randomForest(SalePrice ~ ., mtry=best$mtry, ntree=best$ntree, data=train)
  rf.pred <- predict(housing_con.rf, test[, -ncol(test)])
  MSE$`Random Forest`[loop] <- sqrt(sum((test$SalePrice-rf.pred)^2)/nrow(test))
  
  ## ensemble, weighted ensemble of the above four methods
  Ridge_W <- 1/(MSE$Ridge[loop])
  Lasso_W <- 1/(MSE$Lasso[loop])
  Elastic_W <- 1/(MSE$`Elastic Net`[loop])
  RandomF_W <- 1/(MSE$`Random Forest`[loop])
  
  normalizer <- 1/(Ridge_W + Lasso_W + Elastic_W + RandomF_W)
  Ridge_weight <- normalizer * Ridge_W
  Lasso_weight <- normalizer * Lasso_W
  Elastic_weight <- normalizer * Elastic_W
  RandomF_weight <- normalizer * RandomF_W
  pred <- ridge.pred*Ridge_weight + lasso.pred*Lasso_weight + 
    elastic.pred*Elastic_weight + rf.pred*RandomF_weight
  MSE$Ensemble[loop] <- sqrt(sum((test$SalePrice-pred)^2)/nrow(test))
}

write.csv(MSE, file="MSE.csv")

### When price range less than 100,000, we use ensemble method, otherwose we will use random forest



