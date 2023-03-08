# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)



# Data Import and Cleaning
week7_tbl <- read.csv("../data/week3.csv") %>%
  mutate(timeStart = as.POSIXct(timeStart)) %>%
  mutate(timeEnd = as.POSIXct(timeEnd)) %>%
  mutate(timeSpent = timeEnd - timeStart) %>%
  mutate(condition = str_replace_all(condition, c("B" = "Block B", "A" = "Block A", "C" = "Control"))) %>%
  mutate(gender = str_replace_all(gender, c("F" = "Female", "M" = "Male")))
