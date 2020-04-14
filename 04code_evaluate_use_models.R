## Loading R packages
library(tidyverse)
library(h2o)
library(pROC)
library(caret)
library(ROCR)
library(DALEX)
library(pdp)
library(ggsoccer)
library(RColorBrewer)
library(gridExtra)


## Loading data used to model
data_to_mod <- readRDS("data_to_mod.rds")

## Setting testing dataset and features
h2o.init(nthreads = -1, max_mem_size = "4G")
data_h2o <- as.h2o(data_to_mod)
data_split = h2o.splitFrame(data = data_h2o, ratios = 0.8, seed=2212)
data_train = data_split[[1]]
data_test = data_split[[2]]

Y = "is_goal"
X2 = setdiff(h2o.names(data_train), Y)
X1 = c("distance_to_goal_line", "angle_to_goal")



## Loading model Nº2
modelname2 <- "GBM_1_AutoML_20200407_model2" # change this name if you want to use another model saved by yourself
cmod2 <- h2o.loadModel(paste0("saved-models/", modelname2))

# Variable importance
vi2 <- h2o.varimp(cmod2)
vi2
h2o.varimp_plot(cmod2, num_of_features = length(X2))

# Model Performance
pred <- h2o.predict(cmod2, newdata = data_test)
(perf <- h2o.performance(cmod2, newdata = data_test, valid=T))

# Model Performance with another packages
data_test_r <- as.data.frame(data_test)
pred_r <- as.data.frame(pred)

roc <- roc(data_test_r$is_goal, pred_r$p1)
plot.roc(roc, print.auc = TRUE, print.thres = "best")

best_thres <- coords(roc, "best", ret = "threshold", transpose = T)
h2o.confusionMatrix(cmod2, data_test, thresholds = best_thres, metrics = "mean_per_class_accuracy")

pred_r$xG_bool <- ifelse(pred_r$p1 > best_thres, 1, 0)
(result <- confusionMatrix(data = factor(pred_r$xG_bool), 
                           reference = factor(data_test_r$is_goal), positive = "1", mode="prec_recall"))


## Loading model Nº1
modelname1 <- "GBM_1_AutoML_20200407_model1" # change this name if you want to use another model saved by yourself
cmod1 <- h2o.loadModel(paste0("saved-models/", modelname1))

# Variable importance
vi1 <- h2o.varimp(cmod1)
vi1
h2o.varimp_plot(cmod1, num_of_features = length(X1))

# Model Performance
pred <- h2o.predict(cmod1, newdata = data_test)
(perf <- h2o.performance(cmod1, newdata = data_test, valid=T))

# Model Performance with another packages
data_test_r <- as.data.frame(data_test)
pred_r <- as.data.frame(pred)

roc <- roc(data_test_r$is_goal, pred_r$p1)
plot.roc(roc, print.auc = TRUE, print.thres = "best")

best_thres <- coords(roc, "best", ret = "threshold", transpose = T)
h2o.confusionMatrix(cmod1, data_test, thresholds = best_thres, metrics = "mean_per_class_accuracy")

pred_r$xG_bool <- ifelse(pred_r$p1 > best_thres, 1, 0)
(result <- confusionMatrix(data = factor(pred_r$xG_bool), 
                           reference = factor(data_test_r$is_goal), positive = "1", mode="prec_recall"))




## Results analysis
pred_1 <- h2o.predict(cmod1, newdata = data_test)
pred_r_1 <- as.data.frame(as.numeric(as.character(pred_1)))

pred_2 <- h2o.predict(cmod2, newdata = data_test)
pred_r_2 <- as.data.frame(as.numeric(as.character(pred_2)))


# xG values distribution
ggplot(pred_r_2, aes(x=p1)) + 
        geom_density() +
        labs(x="xG") +
        theme_bw() +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 8), expand = c(0, 0))

ggsave("density_xg_plot.png", width = 10, height = 5)


data_test_r <- as.data.frame(data_test)
xx <- data_test_r %>% mutate(xG = pred_r_2$p1)

ggplot(data = xx, aes(x= x_1, y = y_1)) + 
        annotate_pitch(colour = "white",
                       fill   = "black",
                       limits = FALSE) +
        theme_pitch() +
        theme(plot.background = element_rect(fill = "black"),
              title = element_text(colour = "white")) +
        coord_flip(xlim = c(50, 100),
                   ylim = c(0, 100)) +
        geom_tile(aes(fill = xG)) +
        scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
        theme(legend.position = c(0.76, 1), legend.direction = "horizontal",
              legend.text = element_text(color = "white", size = 8, face = "plain"),
              legend.background = element_rect(fill = "black"),
              legend.key = element_rect(fill = "black", color = "black"),
              plot.title = element_text(hjust = 0.07, face = "plain"),
              plot.subtitle = element_text(hjust = 0.07, size = 10, face = "italic"),
              plot.caption = element_text(hjust = 0.95),
              plot.margin = margin(1, 0.2, 0.5, 0.2, "cm")) +
        labs(fill = "xG value", caption = "@DatoFutbol_cl")

ggsave("xG_plot2.png", width = 10*1.3, height = 5*1.6)


# ROC curve
preds_list <- list(pred_r_1$p1, pred_r_2$p1)

nm <- length(preds_list)
goal <- as.data.frame(as.numeric(as.character(data_test$is_goal)))
actuals_list <- rep(goal, nm)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")

png(filename = "AUC_plot.png", width = 8, height = 5, units = "in", res = 300)
plot(rocs, col = as.list(1:nm), main = "Testing dataset ROC Curves")
legend(x = "bottomright", 
       legend = c("GBM1", "GBM2"),
       fill = 1:nm)
dev.off()


# Partial dependency plots
custom_predict <- function(model, newdata) {
        newdata_h2o <- as.h2o(newdata)
        res <- as.data.frame(h2o.predict(model, newdata_h2o))
        return(as.numeric(as.character(res$predict)))
}

explainer_gbm_1 <- DALEX::explain(model = cmod1, 
                                    data = as.data.frame(data_test)[, X1],
                                    y = as.numeric(as.character(as.data.frame(data_test)[, Y])),
                                    predict_function = custom_predict,
                                    label = "GBM1")
mp_gbm_1 <- model_performance(explainer_gbm_1)

explainer_gbm_2 <- DALEX::explain(model = cmod2, 
                                    data = as.data.frame(data_test)[, X2],
                                    y = as.numeric(as.character(as.data.frame(data_test)[, Y])),
                                    predict_function = custom_predict,
                                    label = "GBM2")
mp_gbm_2 <- model_performance(explainer_gbm_2)


pdp_gbm_1 <- variable_response(explainer_gbm_1, variable = "distance_to_goal_line")
pdp_gbm_2 <- variable_response(explainer_gbm_2, variable = "distance_to_goal_line")
plot(pdp_gbm_1, pdp_gbm_2)
ggsave("distance_pdp_plot.png", width = 10, height = 5)


pdp_gbm_1_angle <- variable_response(explainer_gbm_1, variable = "angle_to_goal")
pdp_gbm_2_angle <- variable_response(explainer_gbm_2, variable = "angle_to_goal")
plot(pdp_gbm_1_angle, pdp_gbm_2_angle)
ggsave("angle_pdp_plot.png", width = 10, height = 5)


pdp_gbm_2_foot <- variable_response(explainer_gbm_2, variable = "skilled_foot", type = "factor")
plot(pdp_gbm_2_foot)
ggsave("skilled_foot_pdp_plot.png", width = 10, height = 5)



## Arturo Vidal shotmap
shots <- readRDS("all_unblocked_shots.rds")
AV_shots <- shots %>% filter(playerId == 20475)

shotsB_AV <- AV_shots %>%
        arrange(matchId, matchPeriod, teamId, eventSec) %>%
        mutate(eventSec2 = ifelse(matchPeriod == "2H", eventSec + 2700, eventSec),
               time_prev = ifelse(matchId == lag(matchId) & matchPeriod == lag(matchPeriod) & teamId == lag(teamId), eventSec - lag(eventSec), -1),
               time_prev = ifelse(is.na(time_prev), -1, time_prev),
               skilled_foot = ifelse(body_part == "head/body", body_part,
                                     ifelse(body_part == foot, "Yes", "No")),
               x_meter = x_1 * 105/100,
               y_meter = y_1 * 68/100,
               distance_to_goal_line = sqrt((105 - x_meter)^2 + (32.5 - y_meter)^2),
               angle_to_goal = atan( (7.32 * (105 - x_meter) ) / ( (105 - x_meter)^2 + (32.5 - y_meter)^2 - (7.32/2)^2 )) * 180/pi) %>%
        filter(!is.na(skilled_foot))

data_to_mod_AV <- shotsB_AV %>%
        dplyr::select(is_goal, eventSec, matchPeriod, x_1, y_1, is_CA, 
                      time_prev, skilled_foot, distance_to_goal_line, angle_to_goal) %>%
        mutate(is_goal = factor(is_goal),
               matchPeriod = factor(matchPeriod),
               is_CA = factor(is_CA),
               skilled_foot = factor(skilled_foot))

pred <- h2o.predict(cmod2, newdata = as.h2o(data_to_mod_AV))
pred_r <- as.data.frame(pred)

AV_data <- AV_shots %>%
        mutate(xG = pred_r$p1)

AV_table <- AV_data %>%
            summarise(Shots = n(),
                      Goals = sum(is_goal),
                      xG_Sum = round(sum(xG), 2),
                      xG_dif = paste(ifelse(Goals > xG_Sum, "+", ""), round(Goals - xG_Sum, 2)),
                      xG_per_shot = round(xG_Sum / Shots, 2),
                      Shots_per_goal = round(33/6, 2)) %>%
            t() %>%
            as.data.frame()

tt3 <- ttheme_minimal(
        core=list(bg_params = list(fill=NA, col=NA),
                  fg_params=list(fontface=3, col = "white")),
        rowhead=list(fg_params=list(col="white", fontface=3L)))

ggplot(data = AV_data, aes(y = y_1, x = x_1)) +
        annotate_pitch(colour = "white",
                       fill   = "black",
                       limits = FALSE) +
        theme_pitch() +
        theme(plot.background = element_rect(fill = "black"),
              title = element_text(colour = "white")) +
        coord_flip(xlim = c(50, 100),
                   ylim = c(0, 100)) +
        geom_jitter(aes(fill = factor(is_goal2, levels = c("TRUE", "FALSE")), size = xG,
                        color = factor(is_goal2, levels = c("TRUE", "FALSE"))),
                    alpha = 0.5, shape = 21) +
        annotation_custom(tableGrob(AV_table, cols = NULL, theme = tt3,
                                    rows = c("Shots", "Goals", "xG Sum", "xG dif.", "xG per shot", "Shots per goal")), 
                          xmin = 45, xmax = 70, ymin = 80, ymax = 95) +
        scale_fill_manual(values = c("red", "#00BFFF")) +
        scale_colour_manual(values = c("red", "#00BFFF")) +
        theme(legend.position = c(0.76, 1.03), legend.direction = "horizontal",
              legend.text = element_text(color = "white", size = 8, face = "plain"),
              legend.background = element_rect(fill = "black"),
              legend.key = element_rect(fill = "black", color = "black"),
              plot.title = element_text(hjust = 0.07, face = "plain"),
              plot.subtitle = element_text(hjust = 0.07, size = 10, face = "italic"),
              plot.caption = element_text(hjust = 0.95),
              plot.margin = margin(1, 0.2, 0.5, 0.2, "cm")) +
        labs(fill = "Goal?", size = "xG value", caption = "@DatoFutbol_cl", color = "") +
        guides(fill = guide_legend(override.aes = list(alpha = 0.8, size = 2), reverse=T),
               size = guide_legend(override.aes = list(color = "yellow")),
               color = F) +
        ggtitle("Arturo Vidal", "Open play unblocked shots Bundesliga 2017-2018")

ggsave("AV_shotmap_plot.png", width = 10*1.3, height = 5*1.6)
        