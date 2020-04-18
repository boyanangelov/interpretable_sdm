library(sdmbench)
library(mlr)
library(lime)
library(dplyr)
library(rsample)
library(latticeExtra)
library(sp)
library(ggplot2)

# Get and prepare data ----------------------------------------------------

occ_data_raw <-
  get_benchmarking_data("Loxodonta africana", limit = 1000)

# rename bioclimatic variables to be more descriptive
names(occ_data_raw$raster_data$climate_variables) <- c(
  "Annual_Mean_Temperature",
  "Mean_Diurnal_Range",
  "Isothermality",
  "Temperature_Seasonality",
  "Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month",
  "Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter",
  "Mean_Temperature_of_Driest_Quarter",
  "Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter",
  "Annual_Precipitation",
  "Precipitation_of_Wettest_Month",
  "Precipitation_of_Driest_Month",
  "Precipitation_Seasonality",
  "Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter",
  "Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter"
)

names(occ_data_raw$df_data) <- c(
  "Annual_Mean_Temperature",
  "Mean_Diurnal_Range",
  "Isothermality",
  "Temperature_Seasonality",
  "Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month",
  "Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter",
  "Mean_Temperature_of_Driest_Quarter",
  "Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter",
  "Annual_Precipitation",
  "Precipitation_of_Wettest_Month",
  "Precipitation_of_Driest_Month",
  "Precipitation_Seasonality",
  "Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter",
  "Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter",
  "label"
)

occ_data <- occ_data_raw$df_data

occ_data$label <- as.factor(occ_data$label)

coordinates_df <- rbind(occ_data_raw$raster_data$coords_presence,
                        occ_data_raw$raster_data$background)

occ_data <-
  normalizeFeatures(occ_data, method = "standardize")

occ_data <- cbind(occ_data, coordinates_df)
occ_data <- na.omit(occ_data)

# Split data for machine learning -----------------------------------------

set.seed(42)
train_test_split <- initial_split(occ_data, prop = 0.7)
data_train <- training(train_test_split)
data_test  <- testing(train_test_split)
data_train$x <- NULL
data_train$y <- NULL
data_test_subset <- data_test %>% filter(label == 1)

# Train model -------------------------------------------------------------

task <-
  makeClassifTask(id = "model", data = data_train, target = "label")
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
mod <- train(lrn, task)
pred <- predict(mod, newdata=data_test)
VIMP <- as.data.frame(getFeatureImportance(mod)$res)

# Top n important variables
top_n(VIMP, n=5, importance) %>%
  ggplot(., aes(x=reorder(variable,importance), y=importance))+
  geom_bar(stat='identity')+ coord_flip() + xlab("")

# Performance
performance(pred, measures=auc)

# Generate explanations ---------------------------------------------------
# resampling
sample_data <- withr::with_seed(1, sample_n(data_test_subset, 3))
sample_data_coords <- dplyr::select(sample_data, c("x", "y"))
sample_data$x <- NULL
sample_data$y <- NULL


set.seed(42)
explainer <- lime(data_train, mod)
set.seed(42)
explanation <-
  lime::explain(sample_data,
                explainer,
                n_labels = 1,
                n_features = 5)
plot_features(explanation)

customPredictFun <- function(model, data) {
  v <- predict(model, data, type = "prob")
  v <- as.data.frame(v)
  colnames(v) <- c("absence", "presence")
  return(v$presence)
}

normalized_raster <- RStoolbox::normImage(occ_data_raw$raster_data$climate_variables)
names(normalized_raster) <- names(occ_data_raw$raster_data$climate_variables)
pr <-
  dismo::predict(normalized_raster,
                 mlr::getLearnerModel(mod, TRUE),
                 fun = customPredictFun)


coordinates(sample_data_coords) <- ~ x + y
rf_map <-
  spplot(pr, main = "Habitat Suitability Map", 
         scales = list(draw = TRUE),
         sp.layout = list("sp.points", sample_data_coords, pch=1, cex=1.2, lwd=2, col="white"))
rf_map

