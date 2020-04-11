library(sdmbench)
library(mlr)
library(lime)
library(dplyr)


set.seed(42)

# obtain occurence and environmental data
occ_data_raw <- get_benchmarking_data("Loxodonta africana", limit = 1000)

# rename environmental data features to be more understandable
names(occ_data_raw$raster_data$climate_variables) <- c(
  "mean_temp",
  "mean_diurnal",
  "isotherm",
  "seas_temp",
  "warmest_month",
  "coldest_month",
  "range_temp",
  "wettest",
  "driest",
  "warmest_quart",
  "coldest_quart",
  "precip",
  "precip_wettest_month",
  "precip_driest_month",
  "precip_season",
  "precip_wettest_quart",
  "precip_driest_quart",
  "precip_warmest_quart",
  "precip_coldest_quart"
)

names(occ_data_raw$df_data) <- c(
  "mean_temp",
  "mean_diurnal",
  "isotherm",
  "seas_temp",
  "warmest_month",
  "coldest_month",
  "range_temp",
  "wettest",
  "driest",
  "warmest_quart",
  "coldest_quart",
  "precip",
  "precip_wettest_month",
  "precip_driest_month",
  "precip_season",
  "precip_wettest_quart",
  "precip_driest_quart",
  "precip_warmest_quart",
  "precip_coldest_quart",
  "label"
)

# minor data processing
occ_data <- occ_data_raw$df_data
occ_data$label <- as.factor(occ_data$label)

coordinates.df <- rbind(occ_data_raw$raster_data$coords_presence,
                        occ_data_raw$raster_data$background)
occ_data <- cbind(occ_data, coordinates.df)
occ_data <- na.omit(occ_data)

# split data into training and testing
train_test_split <- rsample::initial_split(occ_data, prop = 0.7)
data.train <- rsample::training(train_test_split)
data.test  <- rsample::testing(train_test_split)

train.coords <- dplyr::select(data.train, c("x", "y"))
data.train$x <- NULL
data.train$y <- NULL

test.coords <- dplyr::select(data.test, c("x", "y"))
data.test$x <- NULL
data.test$y <- NULL

# fit Random Forest
task <- makeClassifTask(id = "model", data = data.train, target = "label")
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
mod <- train(lrn, task)

explainer <- lime(data.train, mod)
explanation <- lime::explain(sample_n(data.test, 3), explainer, n_labels = 1, n_features = 12)
plot_features(explanation)

# create a custom function to make model object work with raster predictions
customPredictFun <- function(model, data) {
  v <- predict(model, data, type = "prob")
  v <- as.data.frame(v)
  colnames(v) <- c("absence", "presence")
  return(v$presence)
}

# predict and create Habitat Suitability Map
pr <- dismo::predict(occ_data_raw$raster_data$climate_variables, mlr::getLearnerModel(mod, TRUE), fun = customPredictFun)

rf_map <- raster::spplot(pr, main = "Predicted Habitat Suitability Map (Random Forests)", ylab = "Suitability")

