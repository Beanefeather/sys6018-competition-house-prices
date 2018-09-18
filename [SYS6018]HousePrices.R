# Ashwanth Samuel (as7cs)
# SYS 6018: Kaggle Competition Number 2

#library reference for reading and writing files
library(readr)
#library reference for data handling
library(dplyr)
library(stringr)

library(dummies)
setwd('/Users/ashwanthsamuel96/Desktop/Fall 2018/SYS 6018/House Price Competition')

#options(stringsAsFactors=FALSE)
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

mode.custom <- function(v){
  a <- table(v) %>% as.data.frame()
  b <- a[order(a$Freq),]
  return(as.numeric(as.character(b[nrow(b),]$v)))
}

#cleaning data
train$LotFrontage[train$LotFrontage %>% is.na() == TRUE] <- mode.custom(train$LotFrontage[train$LotFrontage %>% is.na() == FALSE])
train$Alley[train$Alley %>% is.na() ==TRUE] <- 'TA'
train$GarageCars[train$GarageCars %>% is.na() ==TRUE] <- 0
train$KitchenQual[train$KitchenQual %>% is.na() ==TRUE] <- 'TA'
train$MasVnrType[train$MasVnrType %>% is.na() == TRUE] <- 'TA'
train$MasVnrArea[train$MasVnrArea %>% is.na() == TRUE] <- mode.custom(train$LotFrontage[train$LotFrontage %>% is.na() == FALSE])
train$BsmtQual[train$BsmtQual %>% is.na() == TRUE] <- 'TA'
train$BsmtCond[train$BsmtCond %>% is.na() == TRUE] <- 'none'
train$BsmtExposure[train$BsmtExposure %>% is.na() == TRUE] <- 'TA'
train$BsmtFinType1[train$BsmtFinType1 %>% is.na() == TRUE] <- 'TA'
train$BsmtFinType2[train$BsmtFinType2 %>% is.na() ==TRUE]<-'TA'
train$Electrical[train$Electrical %>% is.na() == TRUE] <- 'TA'
train$FireplaceQu[train$FireplaceQu %>% is.na() == TRUE] <- 'TA'
train$GarageType[train$GarageType %>% is.na() == TRUE] <- 'TA'
#using year built value
train$GarageYrBlt[train$GarageYrBlt %>% is.na() == TRUE] <- train[train$GarageYrBlt %>% is.na() == TRUE,]$YearBuilt
train$GarageFinish[train$GarageFinish %>% is.na() == TRUE] <- 'TA'
train$GarageQual[train$GarageQual %>% is.na() == TRUE] <- 'none'
train$GarageCond[train$GarageCond %>% is.na() == TRUE] <- 'none'
train$PoolQC[train$PoolQC %>% is.na() == TRUE] <- 'TA'
train$Fence[train$Fence %>% is.na() == TRUE] <- 'TA'
train$MiscFeature[train$MiscFeature %>% is.na() == TRUE] <- 'TA'



#cleaning test data
test$LotFrontage[test$LotFrontage %>% is.na() == TRUE] <- mode.custom(test$LotFrontage[test$LotFrontage %>% is.na() == FALSE])
test$Alley[test$Alley %>% is.na() ==TRUE] <- 'TA'
test$GarageCars[test$GarageCars %>% is.na() ==TRUE] <- 0
test$KitchenQual[test$KitchenQual %>% is.na() ==TRUE] <- 'TA'
test$MasVnrType[test$MasVnrType %>% is.na() == TRUE] <- 'TA'
test$MasVnrArea[test$MasVnrArea %>% is.na() == TRUE] <- mode.custom(test$LotFrontage[test$LotFrontage %>% is.na() == FALSE])
test$BsmtQual[test$BsmtQual %>% is.na() == TRUE] <- 'TA'
test$BsmtCond[test$BsmtCond %>% is.na() == TRUE] <- 'none'
test$BsmtExposure[test$BsmtExposure %>% is.na() == TRUE] <- 'TA'
test$BsmtFinType1[test$BsmtFinType1 %>% is.na() == TRUE] <- 'TA'
test$BsmtFinType2[test$BsmtFinType2 %>% is.na() ==TRUE]<-'TA'
test$Electrical[test$Electrical %>% is.na() == TRUE] <- 'TA'
test$FireplaceQu[test$FireplaceQu %>% is.na() == TRUE] <- 'TA'
test$GarageType[test$GarageType %>% is.na() == TRUE] <- 'TA'
#using year built value
test$GarageYrBlt[test$GarageYrBlt %>% is.na() == TRUE] <- test[test$GarageYrBlt %>% is.na() == TRUE,]$YearBuilt
test$GarageFinish[test$GarageFinish %>% is.na() == TRUE] <- 'TA'
test$GarageQual[test$GarageQual %>% is.na() == TRUE] <- 'none'
test$GarageCond[test$GarageCond %>% is.na() == TRUE] <- 'none'
test$PoolQC[test$PoolQC %>% is.na() == TRUE] <- 'TA'
test$Fence[test$Fence %>% is.na() == TRUE] <- 'TA'
test$MiscFeature[test$MiscFeature %>% is.na() == TRUE] <- 'TA'

#test$MSZoning[test$MSZoning %>% is.na() ==TRUE] <- mode.custom(test$MSZoning[test$MSZoning %>% is.na() == FALSE])

test.MSZoning.table <- test$MSZoning[test$MSZoning %>% is.na() == FALSE] %>% table() %>% as.data.frame()
test.MSZoning.table <- test.MSZoning.table[order(test.MSZoning.table$Freq),]
test$MSZoning[test$MSZoning %>% is.na() ==TRUE] <- test.MSZoning.table[nrow(test.MSZoning.table),]$. %>% as.character()

test$BsmtFullBath[test$BsmtFullBath %>% is.na() == TRUE] <- mode.custom(test$BsmtFullBath[test$BsmtFullBath %>% is.na() == FALSE])
test$GarageArea[test$GarageArea %>% is.na() == TRUE] <- mode.custom(test$GarageArea[test$GarageArea %>% is.na() == FALSE])
test$BsmtHalfBath[test$BsmtHalfBath %>% is.na() == TRUE] <- mode.custom(test$BsmtHalfBath[test$BsmtHalfBath %>% is.na() == FALSE])
test$BsmtUnfSF[test$BsmtUnfSF %>% is.na() == TRUE] <- mode.custom(test$BsmtUnfSF[test$BsmtUnfSF %>% is.na() == FALSE])
test$BsmtFinSF1[test$BsmtFinSF1 %>% is.na() == TRUE] <- mode.custom(test$BsmtFinSF1[test$BsmtFinSF1 %>% is.na() == FALSE])
test$BsmtFinSF2[test$BsmtFinSF2 %>% is.na() == TRUE] <- mode.custom(test$BsmtFinSF2[test$BsmtFinSF2 %>% is.na() == FALSE])
test$Utilities[test$Utilities %>% is.na() ==TRUE] <- 'TA'
test$Functional[test$Functional %>% is.na() ==TRUE] <- 'TA'


# A vector containing all of the categorical variables in the data
cols <- c('MSSubClass', 'MSZoning', 'Street',
          'Alley','LotShape','LandContour',
          'Utilities', 'LotConfig',
          'LandSlope',
          'Neighborhood',
          'Condition1',
          'Condition2',
          'BldgType',
          'HouseStyle',
          'OverallQual',
          'OverallCond',
          'RoofStyle',
          'RoofMatl',
          'Exterior1st',
          'Exterior2nd',
          'MasVnrType',
          'ExterQual',
          'ExterCond',
          'Foundation',
          'BsmtQual',
          'BsmtCond',
          'BsmtExposure',
          'BsmtFinType1',
          'BsmtFinType2',
          'Heating',
          'HeatingQC',
          'CentralAir',
          'Electrical',
          'KitchenQual',
          'Functional',
          'FireplaceQu',
          'GarageType',
          'GarageFinish',
          'GarageQual',
          'GarageCond',
          'PavedDrive',
          'PoolQC',
          'Fence',
          'MiscFeature',
          'SaleType',
          'SaleCondition')

# Utilizing lapply to change the categorical variables from numeric to factor
train[cols] <- lapply(train[cols], factor)
test[cols] <- lapply(test[cols], factor)
# Utilizing sapply to ensure that the correct variables are categorical 
sapply(train,class)
sapply(test,class)


# Ashwanth's Initial Thoughts
train1 <- lm(SalePrice ~ Neighborhood + OverallQual + OverallCond + 
               Foundation + RoofMatl + GarageCars 
             , data=train)

summary(train1)

# Hmm, this one didn't seem to work that well
train2 <- lm(SalePrice ~ Foundation + RoofMatl + GarageCars, data=train)
summary(train2)

# 
train3 <- lm(SalePrice ~ Neighborhood + OverallQual + 
               Foundation + RoofMatl + GarageCars  + KitchenQual + 
               BldgType + YearRemodAdd
             , data=train)

summary(train3)

predicted <- predict(train3, newdata=train)

error <- (train$SalePrice - predicted)
error.squared <- error ^ 2
mse <- sum(error.squared) / nrow(train)
mse

mse <- mean((log(predicted) - log(train$SalePrice)) ^ 2)
rmse <- sqrt(mse) 
rmse

# test.predicted <- predict(train3, newdata=test)
# test$SalePrice <- test.predicted
# test <- test[,names(test) %in% c('Id', 'SalePrice')]
# write.table(test, file = "test_house_prices_submission.csv", row.names=F, col.names=c('Id', 'SalePrice'), sep=",")


#basic linear model
basic.lm <- lm(SalePrice ~ ., data=train)
summary(basic.lm)

summary(basic.lm)$coef[summary(basic.lm)$coef[,4] <= .05, 4]

model <- lm(SalePrice ~ MSZoning + LotArea + Street + LotConfig + LandSlope + Neighborhood + Condition1 +
              Condition2 + OverallQual + YearBuilt + RoofStyle + RoofMatl + MasVnrArea + Foundation +
              BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Functional + Fireplaces + FireplaceQu + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + MoSold + SaleCondition
            ,
            data = train)


summary(model)$coef[summary(model)$coef[,4] <= .05, 4]

#removing RoofStyle, MasVnrArea, Foundation, FireplaceQu
model <- lm(SalePrice ~ MSZoning + LotArea + Street + LotConfig + LandSlope + Neighborhood + Condition1 +
              Condition2 + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + MoSold + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.925


#removing LotConfig, Condition1,  BsmtFinType1, Functional, MoSold
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope + Neighborhood +
              Condition2 + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.9161


#removing GarageQual, Condition2
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope + Neighborhood
            + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.9032

#removing Neighborhood (since many levels are insignificant)
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope
            + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.8885


predicted <- predict(model, newdata=train)

error <- (train$SalePrice - predicted)
error.squared <- error ^ 2
mse <- sum(error.squared) / nrow(train)
mse

mse <- mean((log(predicted) - log(train$SalePrice)) ^ 2)
rmse <- sqrt(mse) 
rmse


# from 0.17 to 0.13

# test.predicted <- predict(model, newdata=test)
# test$SalePrice <- test.predicted
# test[test$SalePrice %>% is.na() == TRUE,]
# test <- test[,names(test) %in% c('Id', 'SalePrice')]
# write.table(test, file = "test_house_prices_submission.csv", row.names=F, col.names=c('Id', 'SalePrice'), sep=",")
#this yields 0.18904 on test data as per kaggle which is improvement over previous 0.19118







train.dummy <- dummy.data.frame(train)
test.dummy <- dummy.data.frame(test)



# Create a "not-in" function
'%!in%' <- function(x,y)!('%in%'(x,y))
missing.train <- colnames(test.dummy)[colnames(test.dummy) %!in% colnames(train.dummy) == TRUE]
missing.test <- colnames(train.dummy)[colnames(train.dummy) %!in% colnames(test.dummy) == TRUE]


# Make a dataframe out of missing categorical variables
# Set column names to those missing variables
# and set every value equal to 0
train.missing.df <- data.frame(matrix(ncol = length(missing.train), nrow = dim(train[1])))
colnames(train.missing.df) <- missing.train
train.missing.df[is.na(train.missing.df)] <- 0



# Make a dataframe out of missing categorical variables
# Set column names to those missing variables
# and set every value equal to 0
test.missing.df <- data.frame(matrix(ncol = length(missing.test), nrow = dim(test[1])))
colnames(test.missing.df) <- missing.test
test.missing.df[is.na(test.missing.df)] <- 0



# Bind the missing data with the original dataframes
train.dummy <- cbind(train.dummy, train.missing.df)
test.dummy <- cbind(test.dummy, test.missing.df)
# Keeping SalePrice in test.dummy because we can just change it later






colsd.train <- apply(train.dummy, 2, sd)
colsd.test <- apply(test.dummy, 2, sd)


train.dummy.sdiv <- train.dummy/colsd.train
test.dummy.sdiv <- test.dummy/colsd.test
# Dunno what's happening with YearBuilt (getting some values close to zero in this)


# Create a function for normalizing values to somewhere between 0 and 1
# taken from https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}


# Normalize our dataframes
train.dummy.norm <- as.data.frame(lapply(train.dummy, normalize))
test.dummy.norm <- as.data.frame(lapply(test.dummy, normalize))

# Lucas is Scared about this (no idea where NAs are coming from)###########################################
train.dummy.norm[train.dummy.norm %>% is.na() == TRUE] <- 0
test.dummy.norm[test.dummy.norm %>% is.na() == TRUE] <- 0


# Get first entry in normalized test vector to check distances with all training data
first.test.vec <- as.numeric(as.vector(head(test.dummy.norm,1)))
first.train.vec <- as.numeric(as.vector(head(train.dummy.norm,1)))



dist(rbind(first.train.vec, first.test.vec))




# When initiating our KNN, we want to grab distances of k closest neighbors
# We then want to get the proportion of each of those distances to the sum of all of them
# We then apply that proportion to the SalePrice of the training datum
# We then sum all those products to get our prediction for each datum

knn_dists <- function(k, test.df.vec, train.df) {
  
  # Get distances of every 
  
  
}





#### Testing row dist on a matrix #######

test.mat <- matrix(data = c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 3)
test.vec <- as.vector(c(1,2,5,4))

dista(test.vec, test.mat[,1])

# Function for getting distance between two vectors
vec.dist <- function(vec1, vec2) {
  
  resids <- abs(vec1 - vec2)
  
  sq.res <- resids^2
  
  sum.sq.res <- sum(sq.res)
  
  dist <- sqrt(sum.sq.res)
  
  return(dist)
  
}

apply(test.vec, test.mat, vec.dist)

help(dplyr)


vec.dist(c(1,2,3,4),c(1,2,4,5))


help(mutate)


