###Loading ggridges to create ridgeplots###

rm(list = ls()); gc()

diagnostic = function(.cell){
  load("raw_data/case_studies.rdata")
  nearest %>% 
    filter(cell_id == .cell) %>%
    as_tibble() %>%
    group_by(type) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(percent = count / sum(count, na.rm = TRUE)*100,
           percent = round(percent, 1)) %>%
    print()
}

visualize = function(.cell, .path){
  
  library(ggplot2)
  library(ggridges)
  
  load("raw_data/case_studies.rdata")
  
  g <- ggplot() +
    # Add grid as background
    geom_sf(data = grid %>% filter(cell_id == .cell), 
            color = NA, fill = "black") +
    # Add streets
    geom_sf(data = streets, color = "grey", size = 0.3, alpha = 0.8) +
    labs(fill = "Type of Nearest\nSocial Infrastructure") +
    theme_void(base_size = 14) +
    theme(legend.position = c(0.8, 0.2),
          panel.background = element_rect(fill = "black"),
          legend.title = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          plot.subtitle = element_text(color = "black", hjust = 0.5)) +
    guides(fill = guide_legend(override.aes = list(size = 5)))
  
  
  mybox <- grid %>%
    filter(cell_id == .cell) %>% 
    st_transform(crs = aea) %>% st_bbox()
  
  # Map
  g1 <- g + 
    # Visualize buildings, colored by type of social infrastructure nearest to it
    geom_sf(data = nearest %>% filter(cell_id == .cell), 
            mapping = aes(fill = type),
            shape = 21, color = "white", 
            # Add some attributes to the dot (stroke = how thick the outline is)
            stroke = 0.5, size = 2, alpha = 0.90) +
    scale_fill_manual(values = mycolors,
                      breaks = c("Community Spaces", "Places of Worship", "Social Businesses", "Parks")) +
    coord_sf(crs = aea, 
             xlim = c(mybox["xmin"]+50, mybox["xmax"]-50),
             ylim = c(mybox["ymin"]+50, mybox["ymax"]-50)) +
    labs(subtitle = "Buildings, Colored by Nearest\nType of Social Infrastructure") 
  
  
  lines = mydist %>% 
    filter(cell_id == .cell) %>% 
    group_by(type) %>% 
    summarize(dist = median(dist, na.rm = TRUE),
              label = paste0(round(dist, 0), " m"))
  
  # Ridgeplot
  g2 <- mydist %>% 
    filter(cell_id == .cell) %>%
    ggplot(mapping = aes(x = dist, y = type, fill = type)) +
    geom_density_ridges( color= "white", alpha = 0.7) +
    shadowtext::geom_shadowtext(
      data = lines,
      mapping = aes(x = dist, y = type, label = label, color = type),
      vjust = 0, nudge_y = 0.2,
      bg.r = 0.3, bg.color = "#373737") +
    shadowtext::geom_shadowtext(
      data = lines,
      mapping = aes(x = dist, y = type, label = label, color = type),
      vjust = 0, nudge_y = 0.2,
      bg.r = 0.2, bg.color = "white", color = "black") +
    
    scale_x_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1000)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_color_manual(values = c(mycolors[c(1:3)], "#373737"), 
                       breaks = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks"))+
    scale_fill_manual(values = mycolors,
                      breaks = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks"))+
    labs(y= NULL, x= "Distance (0 - 1000 meters)",
         subtitle = "Distributions of Distance between\nBuildings and Social Infrastructure") +
    theme_minimal(base_size = 14) +
    guides(fill="none", color = "none") +
    theme(panel.grid.major.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, size = 12))
  
  g3 <- ggpubr::ggarrange(g1,g2, nrow =1, legend = "none", labels = c("A", "B"))
  
  ggsave(g3, filename = .path, dpi = 500, width = 10, height = 5)
  
  print(.path)
  diagnostic(.cell)

}

visualize(.cell = 162, .path = "viz/figure_3_cell_162.png")

visualize(.cell = 78, .path = "viz/figure_4_cell_78.png")


visualize(.cell = 97, .path = "viz/figure_4_cell_97.png")

output = tibble(cell_id = c(162, 78,97)) %>% 
  split(.$cell_id) %>%
  purrr::map_dfr(~diagnostic(.cell = .$cell_id), .id = "id")

output
