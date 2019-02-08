# read into data
setwd("/Users/wangyuexi/Desktop/all")
housing_con <- read.csv("housing_con.csv")[, -1]
# scale proce back, and group into 4 classes. 
housing_con$SalePrice <- exp(housing_con$SalePrice)

index1 <- which(housing_con$SalePrice < 100000)
index2 <- which(housing_con$SalePrice < 150000 & housing_con$SalePrice >=100000)
index3 <- which(housing_con$SalePrice < 200000 & housing_con$SalePrice >=150000)
index4 <- which(housing_con$SalePrice >= 200000)
housing_con$SalePrice[index1] <- "less than 100,000"
housing_con$SalePrice[index2] <- "100,000 to 150,000"
housing_con$SalePrice[index3] <- "150,000 to 200,000"
housing_con$SalePrice[index4] <- "more than 200,000"


# turn category into dummy
housing_con$MSZoning <- ifelse(housing_con$MSZoning=="RL", 1, 0)
housing_con$Street <- ifelse(housing_con$Street=="Pave", 1, 0)
housing_con$LotShape <- ifelse(housing_con$LotShape=="Reg", 1, 0)
housing_con$Condition1 <- ifelse(housing_con$Condition1=="Norm", 1, 0)
housing_con$Condition2 <- ifelse(housing_con$Condition2=="Norm", 1, 0)
housing_con$RoofMatl <- ifelse(housing_con$RoofMatl=="CompShg", 1, 0)
housing_con$MasVnrType <- ifelse(housing_con$MasVnrType=="None", 0, 1)
housing_con$ExterQual <- ifelse(housing_con$ExterQual=="TA", 1, 0)
housing_con$ExterCond <- ifelse(housing_con$ExterCond=="TA", 1, 0)
housing_con$Foundation <- ifelse(housing_con$Foundation=="PConc", 1, 0)
housing_con$HeatingQC <- ifelse(housing_con$HeatingQC=="Ex", 1, 0)
housing_con$GarageQual <- ifelse(housing_con$GarageQual=="Ex", 1, 0)
housing_con$GarageCond <- ifelse(housing_con$GarageCond=="Ex", 1, 0)
housing_con$Fence <- ifelse(housing_con$Fence=="None", 0, 1)
housing_con$SaleCondition <- ifelse(housing_con$SaleCondition=="Normal", 0, 1)

# save data
write.csv(housing_con, file="housing_cat.csv")
housing_cat <- fread("housing_cat.csv")[,1]




