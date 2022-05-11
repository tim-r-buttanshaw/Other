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

# fig1 -------------------------------------------------------------------------

# create data set for fig1
data1 <- read_excel("data for r.xlsx",
                    sheet = "fig1", 
                    range = "A1:B7", 
                    col_names = TRUE) %>% 
  mutate(TIFF_billions = TIFF_millions/1000) %>% 
  mutate(TIFF_label = format(round(TIFF_billions, 2), nsmall = 2)) %>% 
  mutate(label1 = glue("\u00A3{TIFF_label}bn"))

# plot fig1
fig1 <- data1 %>% 
  ggplot(aes(x = year, y = TIFF_billions)) +
  geom_col(fill = "#1d70b8", width = 0.65) +
  geom_text(aes(label = label1), vjust = 1.25, colour = "white") +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0 , ceiling(max(data1$TIFF_billions)) + 0.5)) +
  scale_x_continuous(expand = c(0,0), 
                     limits = c(max(data1$year) - 5.5, max(data1$year) + 0.5), 
                     breaks = seq(max(data1$year) - 5, max(data1$year), by = 1)) +
  labs(x = "", 
       y = "Total Income From Farming (\u00A3bn)", 
       caption = ("Source: Defra \u00A9 Crown copyright")) +
  accounts_theme()
fig1

# save fig1
# ggsave(filename = glue("Current prices TIFF {max(data1$year) - 5} to {max(data1$year)}.svg"),
#        path = "Outputs",
#        fig1,
#        width = 15.93,
#        height = 8.97,
#        units = "cm",
#        dpi = 320)

# fig2 -------------------------------------------------------------------------

# create data set for fig2
data2 <- read_excel("data for r.xlsx",
                    sheet = "fig2",
                    range = "A1:B22",
                    col_names = TRUE) %>%
  mutate(TIFF_billions = TIFF_millions/1000)

# plot fig2 
fig2 <- data2 %>% 
  ggplot(aes(x = year, y = TIFF_billions)) +
  geom_point(colour = "#1d70b8", size = 3) +
  geom_line(colour = "#1d70b8", size = 1.5) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0 , ceiling(max(data2$TIFF_billions)) + 0.5)) +
  scale_x_continuous(expand = c(0,0), 
                     limits = c(max(data2$year)-21, max(data2$year) + 1), 
                     breaks = seq(max(data2$year)-20, max(data2$year), by = 2)) +
  labs(x = "", y = "Total Income From Farming (\u00A3bn)", 
       caption = ("Source: Defra \u00A9 Crown copyright")) +
  accounts_theme() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9, size = 12, colour = "grey30"))
fig2

# save fig2
# ggsave(filename = glue("Real terms TIFF {max(data2$year)-20} to {max(data2$year)}.svg"),
#        path = "Outputs",
#        fig2,
#        width = 15.93,
#        height = 8.97,
#        units = "cm",
#        dpi = 320)

# fig3-5------------------------------------------------------------------------

big_data <- read_excel("data for r.xlsx",
                       sheet = "big",
                       col_names = TRUE)%>% 
  group_by(type) %>% 
  nest() %>% 
  mutate(plotable_data = map(.x = data, .f = horizontal_bar_data)) %>% 
  mutate(ploted_data = map(plotable_data, horizontal_bar_plot)) %>%
  select(-data)

output <- vector("logical", 3)
for (i in seq(1,dim(big_data)[1],1)){
  print(purrr::pluck(big_data, "ploted_data", i))
  # horizontal_bar_save(purrr::pluck(big_data, "ploted_data", i), pluck(big_data, "type", i))
  output[i] <- TRUE
  print(output)
  # if (output[3] == TRUE){
    #grid.arrange(
      purrr::pluck(big_data, "ploted_data", i-2)
                 purrr::pluck(big_data, "ploted_data", i-1)
                 purrr::pluck(big_data, "ploted_data", i)
                 #, nrow = 2)
  # }
}