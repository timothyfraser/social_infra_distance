#' @name 01_revision.R
#' @description Script for supplemental analyses for revise-and-resubmit of article. 
#' 



# Q1: DESCRIPTIVES #####################################
# What's the relationship between
#  - rates of each type of social infra and
#  - wealth

library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


data = read_rds("raw_data/rates.rds")

data %>%
  mutate(cat = ntile(pop_density_int, 2)) %>%

  group_by(cat, type) %>%
  summarize(
    density = median(pop_density_int, na.rm = TRUE),
    r_white = cor(rate, pop_white, use = "pairwise.complete.obs") %>% round(2),
    r_income = cor(rate, log(median_income), use = "pairwise.complete.obs") %>% round(2),
    count = sum(count, na.rm = TRUE),
    rate = mean(rate, na.rm = TRUE) %>% round(2),
  )

data %>%
  filter(type == "total") %>%
  summarize(income = median(median_income, na.rm = TRUE),
            white = median(pop_white, na.rm = TRUE),
            pop_density_int = median(pop_density_int, na.rm = TRUE))


read_rds("building_dist_dataset.rds") %>%
  select(building_id, community:social, pop_white, median_income) %>%
  pivot_longer(cols = community:social, names_to = "type", values_to = "dist") %>%
  group_by(type) %>%
  summarize(#dist = median(dist, na.rm = TRUE),
            r_white = cor(pop_white, dist, use = "pairwise.complete.obs"),
            r_income = cor(median_income, dist, use = "pairwise.complete.obs"))
