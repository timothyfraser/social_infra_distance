#' @name 03_figure_A3.R
#' @description
#' Code for visualizing Tile Map

# We can also map these percentages, to give us a relative comparison of 
# in which city blocks it is easiest to access each given type of social infrastructure.

rm(list = ls()); gc()

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(viridis)

mygridpercent <- read_rds("raw_data/mygridpercent.rds")

streets <- read_rds("raw_data/streets.rds")

gg <- ggplot() +
  geom_sf(data = mygridpercent, mapping = aes(fill = percent), color = NA) +
  geom_sf(data = streets, color = "white", linewidth = 0.1, alpha = 0.8) +
  facet_wrap(~type, ncol = 4) +
  scale_fill_viridis(option = "plasma") +
  scale_x_continuous(expand = expansion(add = c(0,0))) +
  scale_y_continuous(expand = expansion(add = c(0,0))) +
  theme_void(base_size = 14) +
  theme(panel.spacing = unit(0, "cm"),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1)) +
  labs(fill = "% of Buildings Nearest\nto Type of Social Infrastructure")


ggsave(gg, filename = "viz/figure_A3_tile_map.png", dpi = 500, width = 8, height = 3)

rm(list = ls()); gc()
