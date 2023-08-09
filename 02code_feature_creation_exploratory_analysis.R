## Loading R packages
library(tidyverse)
library(DataExplorer)

## Loading RDS file with unblocked shots (already processed data)
shots <- readRDS("all_unblocked_shots.rds")

## Feature edition/creation
shotsB <- shots %>%
  arrange(matchId, matchPeriod, teamId, eventSec) %>%
  mutate(eventSec2 = ifelse(matchPeriod == "2H", eventSec + 2700, eventSec),
         time_prev = ifelse(matchId == lag(matchId) & matchPeriod == lag(matchPeriod) & teamId == lag(teamId), eventSec - lag(eventSec), -1),
         time_prev = ifelse(is.na(time_prev), -1, time_prev),
         skilled_foot = ifelse(body_part == "head/body", body_part,
                               ifelse(body_part == foot, "Yes", "No")),
         x_meter = x1 * 105/100,
         y_meter = y1 * 68/100,
         distance_to_goal_line = sqrt((105 - x_meter)^2 + (32.5 - y_meter)^2),
         angle_to_goal = atan( (7.32 * (105 - x_meter) ) / ( (105 - x_meter)^2 + (32.5 - y_meter)^2 - (7.32/2)^2 )) * 180/pi) %>%
  filter(!is.na(skilled_foot))

data_to_mod <- shotsB %>%
        dplyr::select(is_goal, eventSec, matchPeriod, x1, y1, is_CA, 
                      time_prev, skilled_foot, distance_to_goal_line, angle_to_goal) %>%
        mutate(is_goal = factor(is_goal),
               matchPeriod = factor(matchPeriod),
               is_CA = factor(is_CA),
               skilled_foot = factor(skilled_foot))

write_rds(data_to_mod, "data_to_mod.rds")


## Exploratory analysis
plot_correlation(data_to_mod)
ggsave("corr_plot.png", width = 10, height = 5)

plot_bar(data_to_mod, ncol = 2)
ggsave("bars_plot.png", width = 10, height = 5)

plot_density(data_to_mod, ncol = 3)
ggsave("dens_plot.png", width = 10, height = 5)

