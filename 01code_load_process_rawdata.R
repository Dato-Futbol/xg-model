## Loading R packages and source the "getshots" customized own function
source("getshots.R")
library(jsonlite)
library(tidyverse)
library(ggsoccer)


## Getting processed shots data from JSON files of 5 leagues and join them (previous downloading datasets is required)
shotsEN <- get_shots("events/events_England.json", "EN")
shotsFR <- get_shots("events/events_France.json", "FR")
shotsGE <- get_shots("events/events_Germany.json", "GE")
shotsIT <- get_shots("events/events_Italy.json", "IT")
shotsSP <- get_shots("events/events_Spain.json", "SP")

shots <- shotsEN %>%
  bind_rows(shotsFR, shotsGE, shotsIT, shotsSP) %>%
  mutate(is_goal2 = ifelse(is_goal == 1, T, F))

write_rds(shots, "all_unblocked_shots.rds")


# Viz 1: all unblocked shots
ggplot(data = shots, aes(y = y_1, x = x_1)) +
        annotate_pitch(colour = "white",
                       fill   = "black",
                       limits = FALSE) +
        theme_pitch() +
        theme(plot.background = element_rect(fill = "black"),
              title = element_text(colour = "white")) +
        coord_flip(xlim = c(51, 101),
                   ylim = c(-1, 101)) +
        geom_jitter(aes(fill = factor(is_goal2, levels = c("TRUE", "FALSE"))),
                    alpha = 0.3, shape = 21, size = 0.8) +
        facet_wrap(~is_goal2, nrow = 1) +
        scale_fill_manual(values = c("red", "#00BFFF")) +
        scale_colour_manual(values = c("red", "#00BFFF")) +
        theme(legend.position = c(0.75, 1.12), legend.direction = "horizontal",
              legend.text = element_text(color = "white", size = 8, face = "plain"),
              legend.background = element_rect(fill = "black"),
              legend.key = element_rect(fill = "black"),
              strip.background=element_rect(fill = "black"),
              strip.text = element_text(colour = "black"),
              plot.title = element_text(hjust = 0.07, face = "plain"),
              plot.subtitle = element_text(hjust = 0.07, size = 10, face = "italic"),
              plot.caption = element_text(hjust = 0.95),
              plot.margin = margin(1, 0.2, 0.5, 0.2, "cm")) +
        labs(fill = "Goal?", caption = "@DatoFutbol_cl") +
        guides(fill = guide_legend(override.aes = list(alpha = 0.8, size = 2), reverse=T)) +
        ggtitle("Unblocked open play shots", "Top 5 European Leagues 2017-2018")

ggsave("shots_plot.png", width = 10, height = 5)
