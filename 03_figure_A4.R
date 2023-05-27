#' @name 03_figure_A4.R
#' @description
#' Code for visualizing figure A4 boxplots!


### Averages
# Last, let's plot this as a bar chart.
library(viridis)
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(tidyr)
library(viridis)
library(shadowtext)


mygridpercent <- read_rds("raw_data/mygridpercent.rds")
grid <- read_rds("raw_data/grid.rds") %>%
  filter(milestone_id %in% c(1:4))

mygridstats <- mygridpercent %>%
  as_tibble() %>%
  group_by(type) %>%
  summarize(lower = quantile(percent, probs = 0.25, na.rm = TRUE),
            median = quantile(percent, probs = 0.50, na.rm = TRUE),
            upper = quantile(percent, probs = 0.75, na.rm = TRUE))

# Finally let's also calculate the number of Boston Buildings analyzed
myn = read_rds("building_dist_dataset.rds") %>% nrow()
mycells <- grid$cell_id %>% length()

gg <- mygridpercent %>%
  as_tibble() %>%
  ggplot(mapping = aes(x = type, y = percent, color = type, fill = type)) +
  geom_jitter(shape = 21,  color = "black", size = 4, alpha = 0.1) +
  geom_violin(fill = "white") +
  geom_crossbar(data = mygridstats, 
                mapping = aes(x = type, y = median, ymin = lower, ymax = upper, 
                              fill = type), color = "black", alpha = 0.5) +
  geom_shadowtext(data = mygridstats,
                  mapping = aes(x = type, y = median, label = paste(round(median, 0), "%", sep = "")),
                  # Create a nice white border around our dark grey (#373737) text, with a border radius of 0.2
                  bg.r = 0.2, bg.color = "white", color = "#373737",
                  # And make it big (size = 5) and vertically offset just slightly (-0.5)
                  size = 5, vjust = -0.5) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", guide = "none") +
  scale_color_viridis(discrete = TRUE, option = "plasma", guide = "none") +
  labs(subtitle = paste("Distribution of Nearest Types of Social Infrastructure,",
                        "\n", "among Boston Buildings in ", mycells, " City Blocks", sep = ""),
       y = "Percentage of Buildings per 1 sq.km.\nNearest to Type of Social Infrastructure",
       x = "Type of Social Infrastructure",
       caption = paste("Calculated for ", mycells, " 1-square-kilometer city blocks in Boston.", sep = "")) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0))

gg


ggsave(gg, filename = "viz/figure_A4_violins.png", dpi = 500, width = 7, height = 5)
