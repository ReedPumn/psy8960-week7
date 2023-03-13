# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read.csv("../data/week3.csv") %>%
  mutate(timeStart = as.POSIXct(timeStart)) %>%
  mutate(timeEnd = as.POSIXct(timeEnd)) %>%
  mutate(timeSpent = as.numeric(difftime(timeEnd, timeStart, units = "mins"))) %>%
  mutate(condition = str_replace_all(condition, c("B" = "Block B", "A" = "Block A", "C" = "Control"))) %>%
  mutate(gender = str_replace_all(gender, c("F" = "Female", "M" = "Male"))) %>%
  filter(q6 == 1) %>%
  select(timeStart:q5,q7:timeSpent) %>%
  mutate(condition = as.factor(condition), gender = as.factor(gender)) %>%
  mutate(q1 = as.numeric(q1), q2 = as.numeric(q2), q3 = as.numeric(q3), q4 = as.numeric(q4), q5 = as.numeric(q5), q7 = as.numeric(q7), q8 = as.numeric(q8), q9 = as.numeric(q9), q10 = as.numeric(q10)) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female")))

# Visualization
ggpairs(data = week7_tbl, columns = 5:13, diag = list(continuous = "densityDiag"), upper = list(continuous = "cor"), lower = list("points"))


fig_1 <- (ggplot(data = week7_tbl, aes(x = timeStart, y = q1)) +
            geom_point() +
            labs(x = "Date of Experiment", y = "Q1 Score", )) %>%
  ggsave("../figs/fig1.png", ., width = 1920, height = 1080, units = "px")


fig_2 <- (ggplot(data = week7_tbl, aes(x = q1, y = q2, color = gender)) + 
            geom_jitter()  +
            guides(color = guide_legend(title = "Participant Gender"))) %>%
  ggsave("../figs/fig2.png", ., width = 1920, height = 1080, units = "px")

fig_3 <- (ggplot(data = week7_tbl, aes(x = q1, y = q2)) +
            geom_jitter() +
            facet_grid(cols = vars(gender)) + 
            labs(x = "Score on Q1", y = "Score on Q2")) %>%
  ggsave("../figs/fig3.png", ., width = 1920, height = 1080, units = "px")


fig_4 <- (ggplot(data = week7_tbl, aes(x = gender, y = timeSpent)) +
            geom_boxplot() + 
            labs(x = "Gender", y = "Time Elapsed (mins)")) %>%
  ggsave("../figs/fig4.png", ., width = 1920, height = 1080, units = "px")


fig_5 <- (ggplot(data = week7_tbl, aes(x = q5, y = q7, color = condition)) +
            geom_jitter(width = 0.1) +
            labs(x = "Score on Q5", y = "Score on Q7") +
            guides(color = guide_legend(title = "Experimental Condition")) +
            theme(legend.position = "bottom", legend.background = element_rect(fill = "#e0e0e0")) +
            geom_smooth(method = "lm", se = FALSE)) %>%
  ggsave("../figs/fig5.png", ., width = 1920, height = 1080, units = "px")