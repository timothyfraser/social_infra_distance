#' @name 01_packages.R
#' @description Script to install packages. Run first.

p = c("googledrive", "tidyverse", "sf", 
      "tigris", "ggtext", "ggpubr", "shadowtext", 
      "viridis", "Zelig", "texreg", "lmtest", "ggspatial",
      "gert")
install.packages(p)
remove(p)