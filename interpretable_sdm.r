library(sdmbench)
library(mlr)
library(lime)
library(dplyr)

set.seed(42)
occ_data_raw <- get_benchmarking_data("Loxodonta africana", limit = 1000)

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

occ_data <- occ_data_raw$df_data
occ_data$label <- as.factor(occ_data$label)

coordinates_df <- rbind(occ_data_raw$raster_data$coords_presence,
                        occ_data_raw$raster_data$background)
occ_data <- cbind(occ_data, coordinates_df)
occ_data <- na.omit(occ_data)

train_test_split <- rsample::initial_split(occ_data, prop = 0.7)
data_train <- rsample::training(train_test_split)
data.test  <- rsample::testing(train_test_split)

data_train$x <- NULL
data_train$y <- NULL

data_test_subset <- data.test %>% filter(label == 1)
sample_data <- sample_n(data_test_subset, 3)
sample_data_coords <- dplyr::select(sample_data, c("x", "y"))
sample_data$x <- NULL
sample_data$y <- NULL

task <- makeClassifTask(id = "model", data = data_train, target = "label")
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
mod <- train(lrn, task)

explainer <- lime(data_train, mod)
explanation <- lime::explain(sample_data, explainer, n_labels = 1, n_features = 5)
plot_features(explanation)

customPredictFun <- function(model, data) {
  v <- predict(model, data, type = "prob")
  v <- as.data.frame(v)
  colnames(v) <- c("absence", "presence")
  return(v$presence)
}

pr <- dismo::predict(occ_data_raw$raster_data$climate_variables, mlr::getLearnerModel(mod, TRUE), fun = customPredictFun)

rf_map <- raster::spplot(pr, main = "Predicted Habitat Suitability Map (Random Forest)", ylab = "Suitability")
rf_map
