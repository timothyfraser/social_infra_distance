#' @name 03_figure_1.R
#' 


grid <- read_rds("raw_data/grid.rds") %>%
  filter(milestone_id %in% c(1:4)) %>%
  # Cut the harbor islands
  filter(!cell_id %in% c(203, 192, 181, 171))


streets <- read_rds("raw_data/streets.rds")

mycolors <- viridis(n = 4, option = "plasma")
box = grid %>% st_bbox() %>% as.list()


g <- ggplot() +
  # Add grid as background
  geom_sf(data = grid, color = "#373737", linewidth = 0.5, fill = "black") +
  # Add streets
  geom_sf(data = streets, color = "grey", size = 0.15, alpha = 0.8) +
  # Visualize buildings, colored by type of social infrastructure nearest to it
  geom_sf(data = read_rds("raw_data/nearesttype.rds"), 
          mapping = aes(fill = type),
          shape = 21, color = "white", 
          # Add some attributes to the dot (stroke = how thick the outline is)
          stroke = 0.1, size = 0.5, alpha = 0.90) +
  scale_fill_manual(values = mycolors,
                    breaks = c("Community Spaces", "Places of Worship", "Social Businesses", "Parks")) +
  labs(fill = "Type of Nearest\nSocial Infrastructure",
       subtitle = "Boston Buildings by Nearest Social Infrastructure",
       caption = "Points depict buildings. Empty spots reveal expansive parks, industrial parks, or airport.") +
  theme_void(base_size = 14) +
  theme(legend.position = c(0.95, 0.2),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        plot.caption = element_text(hjust = 0, color = "white"),
        plot.subtitle = element_text(color = "white")) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  coord_sf(xlim = c(box$xmin, box$xmax))

ggsave(plot = g + labs(caption = NULL, subtitle = NULL) + ggpubr::bgcolor(color = "black"), 
       filename = "viz/figure_1_pointmap.png", dpi = 500, width = 6, height = 5)


magick::image_read("viz/figure_1_pointmap.png", density = 500) %>%
  magick::image_crop(geometry = "5000x3500+500") %>%
  magick::image_write(path = "viz/figure_1_pointmap.png", format = "png", density = 500, flatten = FALSE, quality = 100)
