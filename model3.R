library(h2o)
library(data.table)
# library(caret)
h2o.init()

# read in data
train <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/train.csv"))
sub_test <- data.table(read.csv("/Users/MarkDerry/R/Projects/HousingPrice_kaggle/test.csv"))

# data conditioning function
data_conditioning <- function(input_data){
  
  setnames(input_data, c("X1stFlrSF", "X2ndFlrSF"), c("FirstFlrSF", "SecondFlrSF"))
  
  # feature engineering
  input_data[BsmtExposure == "No", BsmtExposure_num := 0]
  input_data[is.na(BsmtExposure), BsmtExposure_num := 0]
  input_data[BsmtExposure == "Mn", BsmtExposure_num := 1]
  input_data[BsmtExposure == "Av", BsmtExposure_num := 2]
  input_data[BsmtExposure == "Gd", BsmtExposure_num := 3]
  input_data[, Neighborhood_mean_year := mean(YearBuilt), by = "Neighborhood"]
  input_data[, Neighborhood := NULL]
  input_data[BsmtQual == "Ex", BsmtQual_num := 1]
  input_data[BsmtQual == "Gd", BsmtQual_num := 2] 
  input_data[BsmtQual == "Fa", BsmtQual_num := 4] 
  input_data[BsmtQual == "TA", BsmtQual_num := 3] 
  input_data[ExterQual == "Ex", ExterQual_num := 1]
  input_data[ExterQual == "Gd", ExterQual_num := 2]
  input_data[ExterQual == "Fa", ExterQual_num := 4]
  input_data[ExterQual == "TA", ExterQual_num := 3]
  input_data[KitchenQual == "Ex", KitchenQual_num := 1]
  input_data[KitchenQual == "Gd", KitchenQual_num := 2]
  input_data[KitchenQual == "Fa", KitchenQual_num := 4]
  input_data[KitchenQual == "TA", KitchenQual_num := 3]
  input_data[, BsmtQual := NULL]
  input_data[, ExterQual := NULL]
  input_data[, FireplaceQu := NULL]
  input_data[, KitchenQual := NULL]
  input_data[, BsmtExposure := NULL]
  
  setDT(input_data)[, c(levels(input_data$BsmtFinType1), "c") := c(lapply(levels(BsmtFinType1), function(x) as.integer(x == BsmtFinType1)), .(NULL))]
   
  setDT(input_data)[, c(levels(input_data$GarageType), "c") := c(lapply(levels(GarageType), function(x) as.integer(x == BsmtFinType1)), .(NULL))]

  return(input_data)
}

train <- data_conditioning(train)
sub_test <- data_conditioning(sub_test)

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
imp <- data.table(as.data.frame(var_imp))
imp <- imp[relative_importance > 0, ]
imp <- imp[scaled_importance > 0.01, ]
imp <- imp[scaled_importance > 0.01, ]

pbar <- plot_ly( x = as.character(imp$variable), y = imp$scaled_importance, name = "Var Importance", type = "bar")
pbar
# get top 30 variables 
# train2 <- subset(train, select = c(var_imp$variable[1:30], "SalePrice"))
train2 <- subset(train, select = c(imp$variable, "SalePrice"))

# check cross plot matrix
# library(GGally)
# ggpairs(train2)

# convert variables
# vars <- caret::dummyVars(formula = SalePrice ~ . , data = train2, sep = "_")
# train3 <- data.table(cbind(predict(vars, newdata = train2), train2$SalePrice))
# setnames(train3, "V63", "SalePrice")
train2_h2o <-as.h2o(x = train2)

test2 <- subset(test, select = c(imp$variable, "SalePrice"))
test2_h2o <-as.h2o(x = test2)

# convert test data
sub_h2o <- as.h2o(sub_test)

# rerun model
aml2 <- h2o.automl(y = "SalePrice",
                   training_frame = train2_h2o,
                   validation_frame = test2_h2o, 
                   max_runtime_secs = 600,
                   max_models = 100,
                   project_name = "house_price2")

h2o.saveModel(object = aml2@leader, path = "models/price_model", force = TRUE)

# Lead Model
# pred_h2o <- h2o.predict(aml@leader, test_h2o) 
# pred <- as.data.frame(pred_h2o)
pred_h2o <- h2o.predict(aml2@leader, train_h2o) 
pred <- as.data.frame(pred_h2o)
check <- data.table(cbind(train, data.frame(pred)))
setnames(check, "predict", "SalePricePredict")

h2o.make_metrics(predicted = pred_h2o, actuals = train_h2o$SalePrice)


library(plotly)
p <- plot_ly(data = check, x = ~SalePrice, y = ~SalePricePredict)
p
h2o.performance(aml2@leader, train_h2o)
h2o.cross_validation_predictions(aml2@leader)
h2o.confusionMatrix(aml2@leader)
h2o.hit_ratio_table(object = aml2@leader, train = TRUE, valid = TRUE, xval = FALSE)
h2o.make_metrics(predicted = pred2_h2o)

pred2_h2o <- h2o.predict(aml2@leader, sub_h2o) 

pred2 <- as.data.frame(pred2_h2o)



submission2 <- data.table(cbind(data.table(sub_test$Id), data.table(pred2)))
setnames(submission2, c("V1", "predict"), c("Id","SalePrice"))
write.csv(submission2, file = "/Users/MarkDerry/R/Projects/HousingPrice_kaggle/submission/submission3.csv", row.names = FALSE, quote = FALSE)

h2o.getConnection()
h2o.shutdown(prompt = TRUE)
