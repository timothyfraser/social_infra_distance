#' @name 03_figure_A4.R
#' @description
#' Code for visualizing figure A4 boxplots!

# Last, let's plot this as a bar chart.
library(viridis)
library(ggplot2)
library(dplyr)
library(readr)

rm(list = ls()); gc()
### Percentages


# For example, what share of buildings in Boston are nearest to what type of social infrastructure?

mytally <- read_rds("raw_data/nearesttype.rds") %>%
  as_tibble() %>%
  # For each type of social infrastructure
  group_by(type) %>%
  # Count for *how many buildings* that is the closest type of social infrastructure
  summarize(count = n()) %>%
  ungroup() %>%
  # Then calculate a percent, roundeth to the hundreths decimal place
  mutate(total = sum(count),
         percent = count / total,
         percent = paste(round(percent, 2)*100, "%", sep = ""))


mycolors <- viridis(n = 4, option = "plasma")

top = mytally %>% 
  summarize(
    type = type[count == max(count)],
    percent = percent[count == max(count)])

gg <- mytally %>%
  ggplot(mapping = aes(x = reorder(type, -count), y = count, label = percent, 
                       fill = reorder(type, -count))) +
  geom_col(color = "#373737") +
  geom_text(vjust = 0, nudge_y = 500) +
  scale_fill_manual(values = mycolors,
                    breaks = c("Community Spaces", "Places of Worship", "Social Businesses", "Parks"),
                    guide = "none") +
  labs(subtitle = "Share of Boston Buildings by Closest Type of Social Infrastructure",
       x = "Type of Social Infrastructure",
       y = "% of Boston Buildings",
       caption = paste0(
         "Note: Each bar shows the percentage of all buildings for whom",
         "\n",
         "their nearest type of social infrastructure is, for example, ", 
         top$type, " (", top$percent, ").")) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave(gg, filename = "viz/figure_A2_bars.png", dpi = 500, width = 7, height = 5)
