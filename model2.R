library(h2o)
library(data.table)
# library(caret)
h2o.init()

train <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/train.csv"))
sub_test <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/test.csv"))

colnames(train) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition","SalePrice")

colnames(sub_test) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","a1stFlrSF","a2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","a3SsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition")


# feature engineering
train[BsmtExposure == "No", BsmtExposure_num := 0]
train[is.na(BsmtExposure), BsmtExposure_num := 0]
train[BsmtExposure == "Mn", BsmtExposure_num := 1]
train[BsmtExposure == "Av", BsmtExposure_num := 2]
train[BsmtExposure == "Gd", BsmtExposure_num := 3]

train[, Neighborhood_mean_year := mean(YearBuilt), by = "Neighborhood"]
train[, Neighborhood := NULL]

train[BsmtQual == "Ex", BsmtQual_num := 1]
train[BsmtQual == "Gd", BsmtQual_num := 2] 
train[BsmtQual == "Fa", BsmtQual_num := 4] 
train[BsmtQual == "TA", BsmtQual_num := 3] 

train[ExterQual == "Ex", ExterQual_num := 1]
train[ExterQual == "Gd", ExterQual_num := 2]
train[ExterQual == "Fa", ExterQual_num := 4]
train[ExterQual == "TA", ExterQual_num := 3]

train[KitchenQual == "Ex", KitchenQual_num := 1]
train[KitchenQual == "Gd", KitchenQual_num := 2]
train[KitchenQual == "Fa", KitchenQual_num := 4]
train[KitchenQual == "TA", KitchenQual_num := 3]

train[, BsmtQual := NULL]
train[, ExterQual := NULL]
train[, FireplaceQu := NULL]
train[, KitchenQual := NULL]
train[, BsmtExposure := NULL]

sub_test[, Neighborhood_mean_year := mean(YearBuilt), by = "Neighborhood"]
sub_test[, Neighborhood := NULL]
sub_test[BsmtQual == "Ex", BsmtQual_num := 1]
sub_test[BsmtQual == "Gd", BsmtQual_num := 2] 
sub_test[BsmtQual == "Fa", BsmtQual_num := 4] 
sub_test[BsmtQual == "TA", BsmtQual_num := 3] 
sub_test[ExterQual == "Ex", ExterQual_num := 1]
sub_test[ExterQual == "Gd", ExterQual_num := 2]
sub_test[ExterQual == "Fa", ExterQual_num := 4]
sub_test[ExterQual == "TA", ExterQual_num := 3]
sub_test[KitchenQual == "Ex", KitchenQual_num := 1]
sub_test[KitchenQual == "Gd", KitchenQual_num := 2]
sub_test[KitchenQual == "Fa", KitchenQual_num := 4]
sub_test[KitchenQual == "TA", KitchenQual_num := 3]

sub_test[BsmtExposure == "No", BsmtExposure_num := 0]
sub_test[is.na(BsmtExposure), BsmtExposure_num := 0]
sub_test[BsmtExposure == "Mn", BsmtExposure_num := 1]
sub_test[BsmtExposure == "Av", BsmtExposure_num := 2]
sub_test[BsmtExposure == "Gd", BsmtExposure_num := 3]

sub_test[, KitchenQual := NULL]
sub_test[, BsmtQual := NULL]
sub_test[, ExterQual := NULL]
sub_test[, FireplaceQu := NULL]
sub_test[, BsmtExposure := NULL]

pairs(~OverallQual + ExterQual_num + ExterQual_num + GarageCars + Neighborhood_mean_year + YearBuilt, data=train, main="Scatterplot Matrix")


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
train2 <- subset(train, select = c(var_imp$variable[1:20], "SalePrice"))



# convert variables
vars <- dummyVars(formula = SalePrice ~ . , data = train2, sep = "_")
train3 <- data.table(cbind(predict(vars, newdata = train2), train2$SalePrice))
setnames(train3, "V63", "SalePrice")
train2_h2o <-as.h2o(x = train2)
train3_h2o <-as.h2o(x = train3)

test2 <- subset(test, select = c(var_imp$variable[1:15], "SalePrice"))
test2_h2o <-as.h2o(x = test2)

# convert test data
test_vars <- dummyVars(formula = SalePrice ~ . , data = test2, sep = "_")
test3 <- data.table(cbind(predict(test_vars, newdata = test2), test2$SalePrice))
setnames(test3, "V63", "SalePrice")
test3_h2o <-as.h2o(x = test3)

sub_h2o <- as.h2o(sub_test)
 
# rerun model
aml2 <- h2o.automl(y = "SalePrice",
                  training_frame = train2_h2o,
                  validation_frame = test2_h2o, 
                  max_runtime_secs = 600,
                  max_models = 100,
                  project_name = "house_price2")

h2o.saveModel(object = aml2@leader, path = "models/price_model")

# model <- h2o.loadModel(path = "models/price_model/GBM_grid_0_AutoML_20171227_165020_model_1")

# View the AutoML Leaderboard
# lb <- aml@leaderboard
# 
# # The leader model is stored here
# aml@leader
# aml2@leader
# aml3@leader

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
# pred_h2o <- h2o.predict(aml@leader, test_h2o) 
# pred <- as.data.frame(pred_h2o)
pred_h2o <- h2o.predict(aml2@leader, train_h2o) 
pred <- as.data.frame(pred_h2o)
check <- data.table(cbind(train, data.frame(pred)))
setnames(check, "predict", "SalePricePredict")

library(plotly)
p <- plot_ly(data = check, x = ~SalePrice, y = ~SalePricePredict)
p
h2o.performance(aml2@leader, train_h2o)


pred2_h2o <- h2o.predict(aml2@leader, sub_h2o) 
pred2 <- as.data.frame(pred2_h2o)


# submission <- data.table(cbind(sub_test$Id, data.table(pred)))
# setnames(submission, c("V1", "predict"), c("Id","SalePrice"))
# write.csv(submission, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission2.csv", row.names = FALSE, quote = FALSE)

submission2 <- data.table(cbind(data.table(sub_test$Id), data.table(pred2)))
setnames(submission2, c("V1", "predict"), c("Id","SalePrice"))
write.csv(submission2, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission2.1.csv", row.names = FALSE, quote = FALSE)

# submission3 <- data.table(cbind(sub_test$Id, data.table(pred2)))
# setnames(submission3, c("V1", "predict"), c("Id","SalePrice"))
# write.csv(submission3, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission2.2.csv", row.names = FALSE, quote = FALSE)
h2o.getConnection()
h2o.shutdown(prompt = TRUE)
