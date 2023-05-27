#' @name 03_figure_A1.R
#' @description Correlation Matrix ggplot visual code.
#' 
#' 

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

mydat <- read_rds("building_dist_dataset.rds")

square = mydat %>%
  mutate_at(vars(social, community, parks, worship), list(~.^2)) %>%
  select(community, worship, social,parks, 
         pop_density_int, pop_black, pop_hisplat, pop_asian,
         pop_some_college, median_income, income_inequality, median_monthly_housing_cost, #lu, 
         train_dist, bus_dist, overall_cond, own_occ, yr_built, gross_tax, cost_sqft)  %>%
  cor(use = "pairwise.complete.obs")


viz = square %>%
  as_tibble(rownames = "y") %>%
  mutate(y = factor(1:n(), levels = 1:n(), labels = y)) %>%
  pivot_longer(cols = -c(y), names_to = "x", values_to = "coefficient", values_drop_na = TRUE) %>%
  mutate(label = round(coefficient, 1)) %>%
  mutate_at(vars(x, y), list(~factor(.) %>% recode_factor(
    "community" = "Community Spaces",
    "worship" = "Places of Worship",
    "social" = "Social Businesses",
    "parks" = "Parks",
    "pop_density_int" = "Pop. Density",
    "pop_black" = "% Black",
    "pop_hisplat" = "% Hispanic/Latino",
    "pop_asian" = "% Asian",
    "pop_some_college" = "% Some College",
    "median_income" = "Median Income",
    "income_inequality" = "Income Inequality",
    "median_monthly_housing_cost" = "Median Housing Cost",
    "train_dist" = "Distance to Train",
    "bus_dist" = "Distance to Bus",
    "overall_cond" = "Building Condition",
    "own_occ" = "Owner Occupied",
    "yr_built" = "Year Built",
    "gross_tax" = "Gross Tax",
    "cost_sqft" = "Cost per sq. ft.") )) %>%
  mutate(xnum = as.numeric(x),
         ynum = as.numeric(y))


g1 <- viz %>%
  ggplot(mapping = aes(x = reorder(x, -xnum), y = reorder(y, -ynum), 
                       fill = coefficient, label = label)) +
  geom_tile(linewidth = 0.01, color = "#373737") +
  geom_text(hjust = 0.5) +
  geom_tile(data = . %>% filter(abs(coefficient) >= 0.75),
            fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradient2(low = "#DC267F", mid = "white",
                       high = "#648FFF", midpoint = 0,
                       limits = c(-1, 1),
                       breaks = c(-1, -0.75, -0.5, -0.25, 0, .25, 0.5, 0.75, 1)) +
  labs(caption = paste0("Note: Black squares denote correlation over +/-0.75.",
  "\n",
  "Parks, Social Businesses, Places of Worship, and Community Spaces",
  "\n",
  "refer to median distance to locations."),
       fill = "Pearson's r\nCorrelation Coefficient") +
  theme_classic(base_size = 14) +
  scale_y_discrete(position = "left", expand = expansion(0)) +
  scale_x_discrete(position = "bottom", expand = expansion(0)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        #axis.ticks = element_blank(),
        panel.border = element_rect(fill = NA, color ="#373737"),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.title = element_text(size = 12, hjust = 0),
        axis.text.y.left = element_text(hjust = 1),
        axis.text.x.bottom = element_text(hjust = 0, angle = -45)) +
  guides(fill = guide_colorsteps(barwidth = 20, barheight = 2)) +
  coord_fixed(ratio = 1)

ggsave(g1, filename = "viz/fig_A1_corrmatrix.png", dpi = 500, width = 9.5, height = 9)


