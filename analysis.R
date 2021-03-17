library(data.table)
library(h2o)

# loading data
train <- fread('train.csv')
test <- fread('test.csv')

# data engineering
train[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(train)]

train[, .(LotFrontage, LotArea, LotShape, lotDepth = LotArea/LotFrontage)]
# add calculations for lot size and shape
train[is.na(LotFrontage), LotFrontage_b := 0]
train[!is.na(LotFrontage), LotFrontage_b := 1]
train[, lotDepth := LotArea/LotFrontage]
train[, lotDepth := LotArea/LotFrontage]
train[, lotRatio := LotFrontage/lotDepth]
frontage <- train[!is.na(LotFrontage), .(frontage = mean(LotFrontage, na.rm = TRUE), depth = mean(lotDepth, na.rm = TRUE)), by = 'LotArea']

setkey(frontage, LotArea)
setkey(train, LotArea)
train <- train[frontage, roll = 'nearest']
train[is.na(LotFrontage),.(LotFrontage, LotArea, LotShape, lotDepth, depth, frontage, depth*frontage, depth*frontage - LotArea)]
train[, c('LotFrontage', 'lotDepth') := NULL]
train[is.na(lotRatio), lotRatio := frontage/depth]

# fix data
train[is.na(Alley), Alley := 'No alley access']

# remove cols that don't have enough data



# build model using h2o
h2o.init()

# Import a sample binary outcome train/test set into H2O
train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)
train_split <- h2o.splitFrame(train_h2o, ratios = .8)

training <- train_split[[1]]
validation <- train_split[[2]]

# Identify predictors and response
y <- "SalePrice"
x <- setdiff(names(train), y)

# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(x = x, y = y,
                  training_frame = training,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

# The leader model is stored here
aml@leader

pred <- h2o.predict(aml@leader, validation)
validation$predictions <- pred
validation <- as.data.table(validation)
# calculate rmsle
validation[, error := (log(predictions + 1) - log(SalePrice + 1))^2]
validation[, .(rmsle = (mean(error))^(1/2))]

# submit
submission <- h2o.predict(aml@leader, test_h2o)
test_h2o$predictions <- submission
test_h2o <- as.data.table(test_h2o)
submission <- test_h2o[, .(Id, SalePrice = predictions)]
fwrite(submission, 'submission.csv')

# system('kaggle competitions submit -c house-prices-advanced-regression-techniques -f submission.csv -m "run1"')
