################################################################################
#  @Organisation - Defra Agri Foodchain Directorate (Evidence & Analysis)
#  @Project - England TIFF
#  @Title - Produce TIFF_England charts
#  @Decription - Produces all charts for TIFF_England publication
#  @Author - Tim Buttanshaw
#  @Date - 07.01.2022
################################################################################

# package load -----------------------------------------------------------------

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

#load other packages
p_load(tidyverse, 
       glue, 
       fs, 
       readxl, 
       cli,
       scales,
       gridExtra)

# source government font -------------------------------------------------------

source("add gov font.R")

# source arial font ------------------------------------------------------------

source("add arial font.R")

# functions --------------------------------------------------------------------

# accounts theme for graphs
accounts_theme <- function(){
  theme_bw() +
    theme(
      text=element_text(family = "Arial"),
      axis.text.x = element_text(size = 12, colour = "grey30"),
      axis.text.y = element_text(size = 12, colour = "grey30"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      panel.grid.major.y = element_line(colour = "grey"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "grey"),
      axis.ticks = element_line(colour = "grey"),
      plot.caption = element_text(size = 12, hjust = 0, vjust = 4),
      plot.caption.position = "plot"
    )
}

# format data for figures 3-5
horizontal_bar_data <- function(nu){
  data  <- nu %>% 
    mutate(component = fct_reorder(component, value_millions)) %>%
    mutate(label_form = format(round(value_millions, 0), nsmall = 0)) %>% 
    mutate(label = glue("\u00A3{label_form}")) %>%
    mutate(graph_width = (ceiling((max(value_millions)) / 5) * 5) + 2) %>% 
    mutate(label_colour = if_else(value_millions >= 0.1 * graph_width, "#ffffff","#000000")) %>% 
    mutate(label_position = if_else(value_millions >= 0.1 * graph_width, 1.1, 0.0)) %>% 
    mutate(year = as.character(year))
  return(data)
}

# plot figures 3-5
horizontal_bar_plot <- function(psi){
  fig <- psi %>% 
    ggplot(aes(x = value_millions, y = component)) +
    geom_bar(aes(fill = year), stat = "identity", 
             position = position_dodge2(reverse = TRUE, padding = 0.1),
             width = 0.8) +
    scale_fill_manual(values = c("2019" = "#1d70b8", "2020" = "#28a197")) +
    geom_text(aes(label = label),
              position = position_dodge2(reverse = TRUE, padding = 0.1, width = 0.75),
              hjust = c(psi$label_position[1:dim(psi)[1]]),
              family = "Arial",
              colour = c(psi$label_colour[1:dim(psi)[1]])) +
    labs(x = "\u00A3", y = "",
         caption = ("Source: Defra \u00A9 Crown copyright"), 
         axis.title.x = element_text(hjust = 10)) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0 , psi$graph_width)) +
    accounts_theme() +
    theme(panel.grid.major.x = element_line(colour = "grey"),
          panel.grid.major.y = element_blank())
  return(fig)
}

# save figures 3-5
horizontal_bar_save <- function(eta, xi){
  xi
  ggsave(filename = glue("{xi} chart.svg"),
         path = "Outputs",
         eta,
         width = 15.93,
         height = 8.97,
         units = "cm",
         dpi = 320)
  return(TRUE)
}
