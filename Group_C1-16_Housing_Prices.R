#library reference for reading and writing files
library(readr)
#library reference for data handling
library(dplyr)
library(stringr)
# library for creating dummy variables in a dataframe
library(dummies)

# Read in data
train <- read.csv("all/train.csv", stringsAsFactors = FALSE)
test <- read.csv("all/test.csv", stringsAsFactors = FALSE)

# Function for getting mode of data
mode.custom <- function(v){
  a <- table(v) %>% as.data.frame()
  b <- a[order(a$Freq),]
  return(as.numeric(as.character(b[nrow(b),]$v)))
}

# Clean train data
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



# clean test data
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

# Utilize lapply to change the categorical variables from numeric to factor
train[cols] <- lapply(train[cols], factor)
test[cols] <- lapply(test[cols], factor)

# Utilize sapply to ensure that the correct variables are categorical 
sapply(train,class)
sapply(test,class)


############### DATA EXPLORATION #############################################

# Team's Initial Thoughts
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

#################################################################################


# Basic linear model
basic.lm <- lm(SalePrice ~ ., data=train)
summary(basic.lm)

# Check on coefficients
summary(basic.lm)$coef[summary(basic.lm)$coef[,4] <= .05, 4]

# Create model with significant features
model <- lm(SalePrice ~ MSZoning + LotArea + Street + LotConfig + LandSlope + Neighborhood + Condition1 +
              Condition2 + OverallQual + YearBuilt + RoofStyle + RoofMatl + MasVnrArea + Foundation +
              BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Functional + Fireplaces + FireplaceQu + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + MoSold + SaleCondition
            ,
            data = train)

# Check coefficients
summary(model)$coef[summary(model)$coef[,4] <= .05, 4]

# Remove RoofStyle, MasVnrArea, Foundation, FireplaceQu
model <- lm(SalePrice ~ MSZoning + LotArea + Street + LotConfig + LandSlope + Neighborhood + Condition1 +
              Condition2 + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + MoSold + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.925


# Remove LotConfig, Condition1,  BsmtFinType1, Functional, MoSold
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope + Neighborhood +
              Condition2 + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + GarageQual + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.9161


# Remove GarageQual, Condition2
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope + Neighborhood
            + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.9032

# Remove Neighborhood (since many levels are insignificant)
model <- lm(SalePrice ~ MSZoning + LotArea + Street  + LandSlope
            + OverallQual + YearBuilt + RoofMatl +
              BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
              X2ndFlrSF + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
              GarageArea + X3SsnPorch + ScreenPorch + PoolArea + PoolQC  + SaleCondition
            ,
            data = train)

summary(model)$coef[summary(model)$coef[,4] <= .05, 4]
#R.Square = 0.8885

# Create predictions and check residuals and stats
predicted <- predict(model, newdata=train)

error <- (train$SalePrice - predicted)
error.squared <- error ^ 2
mse <- sum(error.squared) / nrow(train)
mse

mse <- mean((log(predicted) - log(train$SalePrice)) ^ 2)
rmse <- sqrt(mse) 
rmse


# from 0.17 to 0.13


# Create dummy variables in both training and testing data

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





##### Tried this method of normalizing our data but found it flawed (too much range in values)#####
colsd.train <- apply(train.dummy, 2, sd)
colsd.test <- apply(test.dummy, 2, sd)


train.dummy.sdiv <- train.dummy/colsd.train
test.dummy.sdiv <- test.dummy/colsd.test
# Dunno what's happening with YearBuilt (getting some values close to zero in this)
####################################################################################################



# Create a function for normalizing values to somewhere between 0 and 1
# taken from https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
  }


# Normalize our dataframes
train.dummy.norm <- as.data.frame(lapply(train.dummy, normalize))
test.dummy.norm <- as.data.frame(lapply(test.dummy, normalize))

# Set any NAs equal to 0
train.dummy.norm[train.dummy.norm %>% is.na() == TRUE] <- 0
test.dummy.norm[test.dummy.norm %>% is.na() == TRUE] <- 0


# Get first entry in normalized test vector to check distances with all training data
first.test.vec <- as.numeric(as.vector(head(test.dummy.norm,1)))
first.train.vec <- as.numeric(as.vector(head(train.dummy.norm,1)))





################ FEATURE SELECTION ###############################


features <- c('MSZoning', 'LotArea', 'Street', 'LandSlope',
              'OverallQual',  'YearBuilt' , 'RoofMatl' ,
              'BsmtQual' , 'BsmtExposure' , 'BsmtFinSF1' , 'BsmtFinSF2' , 'BsmtUnfSF' , 'X1stFlrSF' ,
              'X2ndFlrSF' , 'KitchenAbvGr' , 'KitchenQual' , 'Fireplaces' , 'GarageType' ,
              'GarageArea' , 'X3SsnPorch' , 'ScreenPorch' , 'PoolArea' , 'PoolQC'  , 'SaleCondition')

train.features <- train[features]
test.features <- test[features]



##################################################################

# Create data frame with dummy variables but only with selected features

train.features.dummy <- dummy.data.frame(train.features)
test.features.dummy <- dummy.data.frame(test.features)



# Get the missing features from each of the test/train dummy sets

missing.train.features <- colnames(test.features.dummy.norm)[colnames(test.features.dummy.norm) %!in% colnames(train.features.dummy.norm) == TRUE]
missing.test.features <- colnames(train.features.dummy.norm)[colnames(train.features.dummy.norm) %!in% colnames(test.features.dummy.norm) == TRUE]


# Make a dataframe out of missing categorical variables
# Set column names to those missing variables
# and set every value equal to 0
train.features.missing.df <- data.frame(matrix(ncol = length(missing.train.features), nrow = dim(train.features[1])))
colnames(train.features.missing.df) <- missing.train.features
train.features.missing.df[is.na(train.features.missing.df)] <- 0



# Make a dataframe out of missing categorical variables
# Set column names to those missing variables
# and set every value equal to 0
test.features.missing.df <- data.frame(matrix(ncol = length(missing.test.features), nrow = dim(test.features[1])))
colnames(test.features.missing.df) <- missing.test.features
test.features.missing.df[is.na(test.features.missing.df)] <- 0



# Bind the missing data with the original dataframes
train.features.dummy <- cbind(train.features.dummy, train.features.missing.df)
test.features.dummy <- cbind(test.features.dummy, test.features.missing.df)
# Keeping SalePrice in test.dummy because we can just change it later



# Normalize our dataframes
train.features.dummy.norm <- as.data.frame(lapply(train.features.dummy, normalize))
test.features.dummy.norm <- as.data.frame(lapply(test.features.dummy, normalize))

# Set any NAs equal to 0
train.features.dummy.norm[train.features.dummy.norm %>% is.na() == TRUE] <- 0
test.features.dummy.norm[test.features.dummy.norm %>% is.na() == TRUE] <- 0


# Get first entry in normalized test vector to check distances with all training data
first.test.vec <- as.numeric(as.vector(head(test.dummy.norm,1)))
first.train.vec <- as.numeric(as.vector(head(train.dummy.norm,1)))





# Function for getting distance between two vectors
vec.dist <- function(vec1, vec2) {
  
  resids <- abs(vec1 - vec2)

  sq.res <- resids^2

  sum.sq.res <- sum(sq.res)

  dist <- sqrt(sum.sq.res)
  
  return(dist)
  
}


# Initialize empty distances dataframe for storing distances per point in  the upcoming for loop
distances <- data.frame(matrix(nrow = nrow(train.features.dummy.norm), ncol = ncol(train.features.dummy.norm)))


# Iterate through all the rows of our test set, finding the distances from each point in the training set to it
# Then store those distances in a vector, which we store in a data.frame
for(i in 1:nrow(test.features.dummy.norm)) {
  distances[i] <- apply(t(train.features.dummy.norm), 2, vec.dist, t(test.features.dummy.norm[i,]))
  
}

# Add a SalePrice column to distances (to use as a stable identifier)
distances$SalePrice <- train$SalePrice


# Function to get the SalePrice for a test point based on the mean of k nearest neighbors
knn.saleprice <- function(distances.saleprices, k)
{
  # Store column name then sort dataframe by it
  id.column <- colnames(distances.saleprices[1])
  distances.saleprices <- distances.saleprices[order(distances.saleprices[id.column]),]

  # Get three closest values
  k.closest <- head(distances.saleprices, k)

  # Return average SalePrice of those three
  return(mean(k.closest$SalePrice))
}

# Initialize vector to store SalePrices
test.SalePrices <- vector(length = nrow(test))

# Initialize counter variable for upcoming for loop
i <- 1

# Iterate through every column of our distances dataframe 
# and get the SalePrice based on KNN
for(colname in colnames(distances)) {
  if(colname != 'SalePrice') {
    test.SalePrices[i] <- knn.saleprice(distances[c(colname,'SalePrice')], )
    i <- i + 1
  }
}

# Create a dataframe to store ID number and predicted SalePrice
test.submission <- data.frame(test$Id, test.SalePrices)
colnames(test.submission) <- c("Id", "SalePrice")

# Write to csv for submission
write.csv(test.submission, "Second Non-Par.csv", row.names =FALSE)




  
    
  




