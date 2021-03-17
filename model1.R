library(h2o)
library(data.table)
h2o.init()


train <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/train.csv"))
test <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/test.csv"))

colnames(train) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition","SalePrice")
colnames(test) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition")

train_h2o <- as.h2o(x = train)
test_h2o <- as.h2o(x = test)

# Import a sample binary outcome train/test set into H2O
# train_h2o <- h2o.importFile("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/train.csv",destination_frame = train, parse = T, na.strings = "NA")
# test <- h2o.importFile("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/test.csv", parse = T, na.strings = "NA")
# column_types <- c("Numeric","Numeric","Enum","Numeric","Numeric","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Numeric","Numeric","Numeric","Numeric","Enum","Enum","Enum","Enum","Enum","Numeric","Enum","Enum","Enum","Enum","Enum","Enum","Enum","Numeric","Enum","Numeric","Numeric","Numeric","Enum","Enum","Enum","Enum","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Enum","Numeric","Enum","Numeric","Enum","Enum","Numeric","Enum","Numeric","Numeric","Enum","Enum","Enum","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Enum","Enum","Enum","Numeric","Numeric","Numeric","Enum","Enum","Numeric")
# train <- h2o.parseRaw(data = train, header = TRUE, sep = ",", parse_type = "CSV", col.types = column_types)
# test <- h2o.parseRaw(test)


# Identify predictors and response
y <- "SalePrice"
x <- setdiff(names(train), y)

aml <- h2o.automl(x = x, y = y,
                  training_frame = train_h2o,
                  # max_runtime_secs = 30,
                  max_models = 100,
                  project_name = "house_price")

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

# The leader model is stored here
aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly
# 
# pred_h2o <- h2o.predict(aml, test_h2o) 
# pred <- as.data.frame(pred_h2o)
# submission <- data.table(cbind(test$Id, data.table(pred)))
# setnames(submission, c("V1", "predict"), c("Id","SalePrice"))
# write.csv(submission, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission1.csv", row.names = FALSE, quote = FALSE)

# Lead Model
pred_h2o <- h2o.predict(aml@leader, test_h2o) 
pred <- as.data.frame(pred_h2o)
submission <- data.table(cbind(test$Id, data.table(pred)))
setnames(submission, c("V1", "predict"), c("Id","SalePrice"))
write.csv(submission, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission2.csv", row.names = FALSE, quote = FALSE)
