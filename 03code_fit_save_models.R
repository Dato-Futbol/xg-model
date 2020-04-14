## Loading R packages 
library(tidyverse)
library(h2o)

## Loading data to model
data_to_mod <- readRDS("data_to_mod.rds")

## Fitting the model N°2: All features
h2o.init(nthreads = -1, max_mem_size = "4G")
data_h2o <- as.h2o(data_to_mod)
data_split = h2o.splitFrame(data = data_h2o, ratios = 0.8, seed=2212)
data_train = data_split[[1]]
data_test = data_split[[2]]

Y = "is_goal"
X = setdiff(h2o.names(data_train), Y)

# Auto Machine Learning
(mod_h2o <- h2o.automl(x=X, y=Y, training_frame = data_train, max_models = 10, seed = 2212))

# Getting the best model
lb <- mod_h2o@leaderboard
print(lb, n = nrow(lb))
lbdf <- as.data.frame(lb)

modelname <- "GBM_1_AutoML_20200407_200123" # write the chosen model name from the "lbdf" dataframe
cmod <- grep(modelname, lbdf$model_id, value=T)
cmod <- h2o.getModel(cmod)
summary(cmod)

# Save the model
h2o.saveModel(object = cmod, path = paste0(getwd(), "/saved-models"), force = TRUE)
h2o.shutdown(prompt=FALSE)


## Fitting model N°1: only 2 features (distance + angle)
data_to_mod <- data_to_mod %>%
        dplyr::select(is_goal, distance_to_goal_line, angle_to_goal)

h2o.init(nthreads = -1, max_mem_size = "4G")
data_h2o <- as.h2o(data_to_mod)
data_split = h2o.splitFrame(data = data_h2o, ratios = 0.8, seed = 2212)
data_train = data_split[[1]]
data_test = data_split[[2]]

Y = "is_goal"
X = setdiff(h2o.names(data_train), Y)

# Auto Machine Learning
(mod_h2o <- h2o.automl(x=X, y=Y, training_frame = data_train, max_models = 10, seed = 2212))

# Getting the best model
lb <- mod_h2o@leaderboard
print(lb, n = nrow(lb))
lbdf <- as.data.frame(lb)

modelname <- "GBM_1_AutoML_20200407_201557" # write the chosen model name from the "lbdf" dataframe
cmod <- grep(modelname, lbdf$model_id, value=T)
cmod <- h2o.getModel(cmod)
summary(cmod)

# Save the model
h2o.saveModel(object = cmod, path = paste0(getwd(), "/saved-models"), force = TRUE)
h2o.shutdown(prompt=FALSE)
