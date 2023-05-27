#' @name 03_figure_2.R
#' @description
#' Code for simulation visual
#' 
#' 

### Simulation

library(tidyverse)
library(Zelig)
library(viridis)
#install.packages(c("VGAM", "AER", "geepack", "MCMCpack", "maxLik"))
#install.packages("Zelig", repos = "http://r.iq.harvard.edu/", type  = "source")
# download.file(url = "https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.7.tar.gz", destfile = "zelig.tar.gz")
# install.packages("zelig.tar.gz", type = "source")

mydat <- read_rds("building_dist_dataset.rds") %>%
  filter(neighborhood != "Other") %>%
  mutate(pop_white = ntile(pop_white, 4)) %>%
  mutate_at(vars(neighborhood, lu), list(~factor(.)))

# Let's run each model in Zelig format

z1 <- mydat %>%
  zelig(formula = I((community/1000)^2) ~ pop_density_int + 
          pop_white + log(median_income) + neighborhood +
          log(train_dist) + log(bus_dist) + log(cost_sqft) + 
          lu + overall_cond + own_occ + yr_built, model = "ls")

z2 <- mydat %>%
  zelig(formula = I((worship/1000)^2) ~ pop_density_int + 
          pop_white + log(median_income) + neighborhood +
          log(train_dist) + log(bus_dist) + log(cost_sqft) + 
          lu + overall_cond + own_occ + yr_built, model = "ls")

z3 <- mydat %>%
  zelig(formula = I((social/1000)^2) ~ pop_density_int + 
          pop_white + log(median_income) + neighborhood +
          log(train_dist) + log(bus_dist) + log(cost_sqft) + 
          lu + overall_cond + own_occ + yr_built, model = "ls")

z4 <- mydat %>%
  zelig(formula = I((parks/1000)^2) ~ pop_density_int + 
          pop_white + log(median_income) + neighborhood +
          log(train_dist) + log(bus_dist) + log(cost_sqft) + 
          lu + overall_cond + own_occ + yr_built, model = "ls")



# Here's what the quartiles look like
mystat <- read_rds("building_dist_dataset.rds") %>%
  filter(neighborhood != "Other") %>%
  group_by(breaks = ntile(pop_white, 4)) %>%
  summarize(lower = min(pop_white, na.rm = TRUE),
            median = median(pop_white, na.rm = TRUE),
            upper = max(pop_white, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = paste(round(lower*100, 0), "-", round(upper*100, 0), "%", sep = ""))


# Get rid of unnecessary data
remove(mydat)


bind_rows(
  z1 %>% 
    setx(pop_white = seq(from = 0, to = 5, length.out = 21)) %>% sim() %>%
    zelig_qi_to_df() %>%
    select(x = pop_white, ev = expected_value) %>%
    mutate(type = "community"),
  z2 %>% 
    setx(pop_white = seq(from = 0, to = 5, length.out = 21)) %>% sim() %>%
    zelig_qi_to_df() %>%
    select(x = pop_white, ev = expected_value) %>%
    mutate(type = "worship"),
  z3 %>% 
    setx(pop_white = seq(from = 0, to = 5, length.out = 21)) %>% sim() %>%
    zelig_qi_to_df() %>%
    select(x = pop_white, ev = expected_value) %>%
    mutate(type = "social"),
  z4 %>% 
    setx(pop_white = seq(from = 0, to = 5, length.out = 21)) %>% sim() %>%
    zelig_qi_to_df() %>%
    select(x = pop_white, ev = expected_value) %>%
    mutate(type = "parks")) %>%
  mutate(type = type %>% recode_factor(
    "community" = "Community Spaces",
    "worship" = "Places of Worship",
    "social" = "Social Businesses",
    "parks" = "Parks")) %>%
  group_by(type, x) %>%
  summarize(lower_ci = quantile(ev, probs = 0.025),
            estimate = quantile(ev, probs = 0.50),
            upper_ci = quantile(ev, probs = 0.975)) %>%
  # Get better positions
  left_join(by = c("x" = "breaks"), y = mystat)  %>%
  # Transform the result back into meters, not kilometers squared 
  mutate_at(vars(estimate, lower_ci, upper_ci), list(~sqrt(.)*1000)) %>%
  ungroup() %>%
  saveRDS("raw_data/mysim.rds")


# Next, let's gather first differences,
# Calculating the difference in median distance squared
# for a building where pop_white = 4 vs. pop_white = 1
get_fd = function(mysimulation){
  data.frame(fd = mysimulation$sim.out$x1$fd %>% unlist(),
             x1 = mysimulation$sim.out$x1$ev %>% unlist(),
             x = mysimulation$sim.out$x$ev %>% unlist()) %>%
    return()
}
# Let's write a quick function to extract first differences
bind_rows(
  z1 %>% 
    sim(., x = setx(., pop_white = 1), x1 = setx1(., pop_white = 4))  %>%
    get_fd() %>%
    mutate(type = "community"),
  z2 %>% 
    sim(., x = setx(., pop_white = 1), x1 = setx1(., pop_white = 4))  %>%
    get_fd() %>%
    mutate(type = "worship"),
  z3 %>% 
    sim(., x = setx(., pop_white = 1), x1 = setx1(., pop_white = 4))  %>%
    get_fd() %>%
    mutate(type = "social"),
  z4 %>% 
    sim(., x = setx(., pop_white = 1), x1 = setx1(., pop_white = 4))  %>%
    get_fd() %>%
    mutate(type = "parks")) %>%
  mutate(type = type %>% recode_factor(
    "community" = "Community Spaces",
    "worship" = "Places of Worship",
    "social" = "Social Businesses",
    "parks" = "Parks")) %>% 
  # Back transform the simulated outcomes
  mutate_at(vars(x, x1), list(~sqrt(.)*1000)) %>%
  # Recalculate the first differences
  mutate(fd = x1 - x) %>%
  group_by(type) %>%
  summarize(
    estimate = quantile(fd, probs = 0.50),
    lower_ci = quantile(fd, probs = 0.025),
    upper_ci = quantile(fd, probs = 0.975),
    # Actual simulated SE
    sd = sd(fd, na.rm = TRUE),
    # Approximated from confidence interval,
    # assuming normal distribution
    se = (upper_ci - lower_ci) / (2*1.96),
    # se and sd are very close, so let's use sd, which is more accurate
    z = estimate / sd,
    p = exp(-0.717*z - 0.416*z^2),
    stars = gtools::stars.pval(p),
    estimate_label = round(estimate, 1),
    estimate_label = if_else(estimate > 0, 
                             true = paste("+", estimate_label, sep = ""),
                             false = paste(estimate_label)),
    diff_label = paste("Difference: ", estimate_label, " m", stars, sep = "")) %>%
  
  saveRDS("raw_data/myfd.rds")

rm(z1,z2,z3,z4)



library(viridis)
mysim <- read_rds("raw_data/mysim.rds") 
myfd <- read_rds("raw_data/myfd.rds")


gg = ggplot() +
  geom_crossbar(data = mysim,
                mapping = aes(x = median * 100, y = estimate, 
                              ymin = lower_ci, ymax = upper_ci, 
                              color = type, fill = type),
                color = "#373737", alpha = 0.5) +
  geom_text(data = mysim, mapping = aes(x = median * 100, y = estimate,
                                        label = paste0(round(estimate, 0), " m") ),
            nudge_y = 15) +
  geom_text(data = myfd, 
            mapping = aes(x = 50, y = 760, label = diff_label), color = "#373737") +
  facet_wrap(~type, ncol = 2)  +
  guides(fill = "none", color = "none") +
  theme_classic(base_size = 14) +
  labs(x = "% White Residents in surrounding 1 sq.km. cell",
       y = "Simulated Median Distance (m) to Nearby Social Infrastructure\n(within 1 km buffer)",
       #caption = paste0(
      #   "Numbers show expected median distance (m), with 95% simulated confidence intervals.",
      #   "\n",
      #   "X axis shows Share of White residents, using midpoints of sample quartiles.",
      #   "\n",
      #   "Q1 (First quartile) = 6~35% White. Q2 = 35-29%. Q3 = 59-72%. Q4 = 72-93%.",
      #   "\n",
      #   "'Difference' statistic shows expected difference between from lowest to highest quartile."
      # ),
       subtitle = "Access to Social Infrastructure vs. Race") +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100), 
                     expand = expansion(c(0,0))) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", begin = 0, end = 0.6) +
  scale_color_viridis(discrete = TRUE, option = "plasma", begin = 0, end = 0.6) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        panel.spacing.x = unit(0.75, "cm"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(hjust = 1),
        strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot")

ggsave(gg, filename = "viz/figure_2_crossbars.png", dpi = 500, width = 7, height = 7)
