library(h2o)
library(data.table)
# library(caret)
# h2o.init()

train <- fread("train.csv")
sub_test <- fread("test.csv")
# colnames(train) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition","SalePrice")

# colnames(sub_test) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition")

inTrain <- sample(train[, .I], floor(train[, .N] * .75))
train <- train[inTrain]
test <- train[-inTrain]


train_h2o <- as.h2o(x = train)
test_h2o <- as.h2o(x = test)

# Identify predictors and response
y <- "SalePrice"
x <- setdiff(names(train), y)

aml <- h2o.automl(x = x, y = y,
                  training_frame = train_h2o,
                  validation_frame = test_h2o, 
                  max_runtime_secs = 30,
                  max_models = 50,
                  project_name = "house_price")
# get variable importance of leader
var_imp <- h2o.varimp(aml@leader)

# get top 15 variables 
train2 <- subset(train, select = c(var_imp$variable[1:15], "SalePrice"))

vars <- dummyVars(formula = SalePrice ~ . , data = train2, sep = "_")

head(predict(vars, newdata = train2))
head(train2)

train3 <- cbind(predict(vars, newdata = train2), train2$SalePrice)

