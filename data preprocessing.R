library(data.table)
setwd('/Users/wangyuexi/Desktop/all')
train <- fread("train.csv")[, -1]
test <- fread("test.csv")[, -1]
sample_submission <- fread("sample_submission.csv")[, -1]
test <- cbind(test, sample_submission)
#### clean the data ####
train$MSSubClass <- as.character(train$MSSubClass)

# Lot frontage NA to 0
train$LotFrontage[is.na(train$LotFrontage)] <- 0
# Alley from NA to None
train$Alley[is.na(train$Alley)] <- "None"

# condition1: change all road with railroad to RR. PosA and PosN to Pos
train[train$Condition1 %like% "RR", 13] <- "RR"
train[train$Condition1 %like% "Pos", 13] <- "Pos"
train[train$Condition2 %like% "RR", 14] <- "RR"
train[train$Condition2 %like% "Pos", 14] <- "Pos"

# MasVnrType NA to None
train$MasVnrType[is.na(train$MasVnrType)] <- "None"
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$MasVnrArea <- as.numeric(train$MasVnrArea)

# Bsmt NA to None
train[, c(30:35)][is.na(train[, 31:36])] <- "None"

# Electrical NA to None
train$Electrical[is.na(train$Electrical)] <- "None"

# FireplaceQu NA to None
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"

# Garage NA to None
train[, c(58, 60, 63, 64)][is.na(train[, c(58, 60, 63, 64)])] <- "None"
train[, c(59, 61, 62)][is.na(train[, c(59, 61, 62)])] <- 0

# PoolQC contains same info as PoolArea so delete PoolQC
train <- train[, -72]
train$Fence[is.na(train$Fence)] <- "None"
# MiscFeature contains same information as miscVal, so delete MiscFeature
train <- train[, -73]

# change year to group 
train$YearBuilt[train$YearBuilt < 1901] <- "before 1900"
train$YearBuilt[train$YearBuilt < 1926 & train$YearBuilt >= 1901] <- "1901-1925"
train$YearBuilt[train$YearBuilt < 1951 & train$YearBuilt >= 1926] <- "1926-1950"
train$YearBuilt[train$YearBuilt < 1976 & train$YearBuilt >= 1951] <- "1951-1975"
train$YearBuilt[train$YearBuilt < 2001 & train$YearBuilt >= 1976] <- "1976-2000"
train$YearBuilt[train$YearBuilt >= 2001] <- "after 2000"

train$YearRemodAdd[train$YearRemodAdd < 1901] <- "before 1900"
train$YearRemodAdd[train$YearRemodAdd < 1926 & train$YearRemodAdd >= 1901] <- "1901-1925"
train$YearRemodAdd[train$YearRemodAdd < 1951 & train$YearRemodAdd >= 1926] <- "1926-1950"
train$YearRemodAdd[train$YearRemodAdd < 1976 & train$YearRemodAdd >= 1951] <- "1951-1975"
train$YearRemodAdd[train$YearRemodAdd < 2001 & train$YearRemodAdd >= 1976] <- "1976-2000"
train$YearRemodAdd[train$YearRemodAdd >= 2001] <- "after 2000"

train$GarageYrBlt[train$GarageYrBlt < 1901] <- "before 1900"
train$GarageYrBlt[train$GarageYrBlt < 1926 & train$GarageYrBlt >= 1901] <- "1901-1925"
train$GarageYrBlt[train$GarageYrBlt < 1951 & train$GarageYrBlt >= 1926] <- "1926-1950"
train$GarageYrBlt[train$GarageYrBlt < 1976 & train$GarageYrBlt >= 1951] <- "1951-1975"
train$GarageYrBlt[train$GarageYrBlt < 2001 & train$GarageYrBlt >= 1976] <- "1976-2000"
train$GarageYrBlt[train$GarageYrBlt >= 2001] <- "after 2000"

# delete utility since there is only one group, meaningless in the prediction

# change month ot character
train$MoSold <- as.character(train$MoSold)


# same process to clean test data
test$BsmtFinSF1 <- as.numeric(test$BsmtFinSF1)
test <- na.omit(test)
#write.csv(train, "train_clean.csv")
#write.csv(test, "test_clean.csv")

library(data.table)
setwd('/Users/wangyuexi/Desktop/all')
train <- fread("train_clean.csv")[, -1]
test <- fread("test_clean.csv")[, -1]
train <- na.omit(train)
#### feature selection ####
lm1 <- lm(SalePrice ~ ., data=train)
# important varible picked
lm2 <- lm(SalePrice ~ MSZoning + LotArea + Street + 
            LandContour + LandSlope + 
            Neighborhood + Condition1 + Condition2 +  
            OverallQual + OverallCond + RoofStyle + 
            RoofMatl + MasVnrType + MasVnrArea + 
            ExterQual + ExterCond + Foundation + BsmtQual + 
            BsmtExposure + BsmtFinSF1 + BsmtFinType2 + 
            BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
            `1stFlrSF` + `2ndFlrSF` + FullBath + GarageCars + 
            GarageQual + GarageCond + WoodDeckSF + `3SsnPorch` + 
            ScreenPorch + PoolArea + Fence + SaleCondition, data=train)
summary(step(lm2, direction="backward"))
final_lm <- lm(formula = SalePrice ~ MSZoning + LotArea + Street + LandSlope + 
                 Neighborhood + Condition1 + Condition2 + OverallQual + OverallCond + 
                 RoofMatl + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                 Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + 
                 BsmtUnfSF + HeatingQC + `1stFlrSF` + `2ndFlrSF` + GarageCars + 
                 GarageQual + GarageCond + WoodDeckSF + ScreenPorch + PoolArea + 
                 Fence + SaleCondition, data = train)

predict(final_lm, test)

