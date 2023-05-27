.cells = c(162, 78,97)

mydist <- read_rds("building_dist_dataset.rds") %>%
  # Filter to sites within boston neighborhoods
  filter(neighborhood != "Other") %>%
  select(building_id, cell_id, community:social) %>%
  pivot_longer(cols = -c(building_id, cell_id), names_to = "type", values_to = "dist") %>%
  mutate(type = type %>% recode_factor(
    "parks" = "Parks",
    "social" = "Social\nBusinesses",
    "worship" = "Places of\nWorship",
    "community" = "Community\nSpaces")) 

rates = read_rds("raw_data/rates.rds") %>%
  select(cell_id, type, count) %>%
  filter(type != "total") %>%
  group_by(cell_id) %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total) %>%
  ungroup() %>%
  mutate(type = type %>% recode_factor(
    "community_space" = "Community\nSpaces",
    "place_of_worship" = "Places of\nWorship",
    "social_business" = "Social\nBusinesses",
    "park" = "Parks")) 


sumdist <- expand_grid(
  cell_id = mydist$cell_id %>% unique(),
  type = mydist$type %>% levels())  %>%
  # Join in distnace stats
  left_join(by = c("cell_id", "type"),
            y = mydist %>%
              group_by(cell_id,type) %>%
              summarize(dist = median(dist, na.rm = TRUE))) %>%
  # join in percentage of total social infrastructure of each type
  left_join(by = c("type", "cell_id"),
            y = rates) %>%
  mutate_at(vars(dist,count:percent), list(~if_else(is.na(.), 0, as.numeric(.)))) %>%
  mutate(row = if_else(cell_id %in% .cells, paste0("Cell ", cell_id), NA_character_)) %>%
  mutate(type = factor(type, levels = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks")))

bounds <-  sumdist %>% 
  with(seq(from = min(count, na.rm = TRUE), to = max(count, na.rm = TRUE), length.out = 5))


###Creating vector for color of ridgeplots###
cols <- viridis(n = 4, option = "plasma")
cols[4] <- viridis(n = 4, option = "plasma", end = 0.9)[4]

# Create a background
a <- data.frame(dist = seq(from = 0, to = max(sumdist$dist)+2, length.out = 200),
                y = seq(from = 0, to = 100, length.out = 200))
b <- data.frame(count = seq(from = 0, to = max(sumdist$count)+2, length.out = 200),
                x = seq(from = 0, to = 100, length.out = 200))

# Get subset
fewdist = sumdist %>%  filter(cell_id %in% .cells)

# Make full grid
bg <- expand_grid(dist = a$dist, count = b$count) %>%
  left_join(by = "dist", y = a) %>%
  left_join(by = "count", y = b) %>%
  mutate(shade = x * (100 - y) ) %>%
  expand_grid(type = sumdist$type %>% levels()) %>%
  mutate(type = factor(type, levels = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks")))
remove(a,b)


### Creating Visualization
gg = ggplot() +
  geom_tile(data = bg, mapping = aes(x = count, y = dist, fill = shade)) +
  scale_fill_gradient2(low = "white", high = "black") +
  ggnewscale::new_scale("fill") +
  facet_grid(rows = vars(row), cols = vars(type), scales = "free_x", drop = TRUE,
             labeller = labeller(.rows = c(
               "Cell 162" = "<b>Back Bay</b><br>(Cell 162)",
               "Cell 97" = "<b>Nubian<br>Square</b><br>(Cell 97)",
               "Cell 78" = "<b>Mount<br>Bowdoin</b><br>(Cell 78)"))) +
  geom_point(data = sumdist %>% select(-row),
             mapping = aes(x = count, y = dist, fill = type), 
             shape = 21, size = 3, color = "#373737", alpha = 0.4) +
  geom_hline(data = fewdist, 
             mapping = aes(yintercept = dist, color = type),
             linewidth = 2.5, color = "white") +
  geom_hline(data = fewdist, 
             mapping = aes(yintercept = dist, color = type),
             linewidth = 2) +
  geom_vline(data = fewdist, 
             mapping = aes(xintercept = count, color = type),
             linewidth = 2.5, color = "white") +
  geom_vline(data = fewdist, 
             mapping = aes(xintercept = count, color = type),
             linewidth = 2) +
  geom_point(data = fewdist,
             mapping = aes(x = count, y = dist, fill = type), 
             shape = 21, color = "white", stroke = 1, size = 7) +
  geom_point(data = fewdist,
             mapping = aes(x = count, y = dist, fill = type), 
             shape = 21, color = "black", stroke = 0.75, size = 5) +
  
  shadowtext::geom_shadowtext(
    data = fewdist,
    mapping = aes(x = count + 1, y = dist - 75,
                  label = paste(round(dist, 0), " m,\nn = ", count, sep = "")),
    bg.round = 0.2, bg.color = "white", color = "black", size = 3, 
    hjust = 0, vjust = 1, fontface = "bold") +
  
  scale_color_manual(values = cols,
                     breaks = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks"))+
  scale_fill_manual(values = cols,
                    breaks = c("Community\nSpaces", "Places of\nWorship", "Social\nBusinesses", "Parks"))+
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1000), expand = expansion(c(0,0))) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20, 25, 30),  limits = c(bounds[1], 27),
    expand = expansion(add = c(1, 0))) +
  labs(
    x = "Frequency of Social Infrastructure per Cell (Total Count)",
    y = "Average Distance from Buildings to Social Infrastructure",
    color = "Social Infrastructure Type", 
    subtitle = "Social Infrastructure by Proximity & Frequency in Boston Case Studies") +
  theme_classic(base_size = 16) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.line = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_rect(fill = NA, color = "#373737"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = ggtext::element_markdown(size = 10, hjust = 0),
        strip.background = element_blank(),
        strip.text.y = ggtext::element_markdown(size = 12, hjust = 0, angle = 0)) +
  theme(legend.position = "none")

ggsave(gg, filename = "viz/figure_6_scatterplot.png", dpi = 500, width = 8, height = 6)

rm(list = ls()); gc()
