# Goal: Create a life in months waffle chart then one of concentric circles
# Author: Jenn Schilling
# Date: 8/8/2020

# Source: https://github.com/sharlagelfand/mylifeinmonths/blob/main/my_life_in_months.R

library(tidyverse)
library(waffle)
library(extrafont)
library(lubridate)
library(prismatic)

# Load  fonts
loadfonts(device = "win", quiet = TRUE)


#### Data ####

birth_year <- 1989
birth_month <- 7
current_year <- year(today())
current_month <- month(today())

# months and years of life so far
life_data <- expand_grid(month = 1:12, 
                         year = birth_year:current_year) %>%
  arrange(year, month) %>%
 # filter(!(year == birth_year & month < birth_month)) %>%
  filter(!(year == current_year & month > current_month))

# eras to be labeled and colored in the chart
# year_month is the start of the era
# label is the label of the era
# fill_color is  the color for the chart
eras <- tribble(
  ~year_month, ~label, ~fill_color,
  "1989,1", "not born yet", "#F7F7F7",
  "1989,7", "childhood", "#549E99",
  "2003,9", "high school", "#5085E2",
  "2007,9", "undergrad", "#4EC29F",
  "2011,9", "masters", "#20AB9F",
  "2013,3", "operations research engineer", "#62B5CC",
  "2015,7", "statistician", "#648ADE",
  "2016,11", "americorps + teacher training", "#337CA3",
  "2017,8", "middle school teacher", "#128CE3",
  "2020,1", "data analyst", "#5889C4"
)

# join eras and life data together
life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month), collapse = ",")) %>%
  ungroup() %>%
  left_join(eras, by = "year_month") %>%
  fill(label, fill_color) %>%
  mutate(text_color = as.character(clr_darken(fill_color, shift = 0.2)),
         fill_color = fct_inorder(fill_color))

# Split life data into list based on era for using labels/colors later on
life_data_list <- split(life_data, life_data$label)


#### make waffle chart ####

# Base plot
life_in_months_base <- life_data %>%
  count(fill_color) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = fill_color, values = n)) +
  ## make each row a year/12 months
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = FALSE) + 
  coord_equal() +
  scale_x_continuous(limits = c(-0.5, 45)) + 
  # The max for the x-axis will differ based on how old you are
  scale_y_continuous(limits = c(-3.5, 15.5)) +
  scale_fill_identity() +
  labs(y = NULL, x = NULL) +
  #theme_ipsum(grid = "") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    # Remove x-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    # Fill Chart Background
    plot.background = element_rect(fill = "#F7F7F7", color ="#F7F7F7")
  )

#### Initial annotations ####

# Use ~10 for exporting at dpi 300, and ~3 for working interactively
annotation_base_size <- 7
annotation_lineheight <- 1
initial_annotations_font_family <- "Lucida Console"
initial_annotations_color <- "#666666"

initial_text <- function(x, y, label, size = annotation_base_size, 
                         color = initial_annotations_color, ...) {
  annotate("text", x = x, y = y, label = label, 
           size = size, color = color, family = "Lucida Console", 
           fontface = "italic", ...)
}

initial_segment <- function(x, xend, y, yend, 
                            color = initial_annotations_color) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), color = color)
}


life_in_months_initial_annotations <- life_in_months_base +
  initial_text(x = 0, y = 6.5, label = "1 year", angle = 90) +
  initial_segment(x = 0, xend = 0, y = 1, yend = 5) +
  initial_segment(x = -0.25, xend = 0.25, y = 1, yend = 1) +
  initial_segment(x = 0, xend = 0, y = 8, yend = 12) +
  initial_segment(x = -0.25, xend = 0.25, y = 12, yend = 12) +
  initial_text(x = 1, y = 14.5, 
               label = "1 square = 1 month", 
               size = annotation_base_size * 1, 
               lineheight = annotation_lineheight, 
               hjust = 0.4) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), 
             arrow = arrow(length = unit(0.0175, "npc")), 
             colour = initial_annotations_colour) +
  initial_text(x = 0.5, y = 0, label = "age", 
               size = annotation_base_size * 1, 
               hjust = 0) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), 
               arrow = arrow(length = unit(0.0175, "npc")), 
               colour = initial_annotations_colour) +
  annotate("text", x = 33, y = 6.5, label = "my life\nin months", 
           hjust = 0, family = "Verdana", 
           fontface = "bold", 
           lineheight = 1, 
           size = annotation_base_size * 2)

#### "Role" annotations ####

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5

role_text <- function(x, y = role_annotations_y, label, 
                      size = roles_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, 
           color = unique(unique(life_data_list[[label]][["text_color"]])), 
           family = "Bradley Hand ITC", ...)
}

role_text_under <- function(x, y, label, color_era, size, ...) {
  annotate("text", x = x, y = y, label = label, 
           color = unique(life_data_list[[color_era]][["text_color"]]), 
           size = size, family = "Bradley Hand ITC", ...)
}

# For annotations: x values are the usually ~midpoint of your age (+1) 
# during that era, give or take for some shifting around to fit labels

life_in_months_role_annotations <- life_in_months_initial_annotations +
  role_text(x = 8, 
            label = "childhood") +
  role_text(x = 17, 
            label = "high school") +
  role_text(x = 21,  
            y = role_annotations_y, #- 1.25, 
            label = "undergrad") +
  role_text_under(x = 21, 
                  y = role_annotations_y - 1, 
                  label = "(math + cs)", 
                  color_era = "undergrad", 
                  size = roles_size) +
  # geom_curve(aes(x = 21.5, xend = 22, y = -1.5, yend = 0.35), 
  #            curvature = 0.4, 
  #            arrow = arrow(length = unit(0.0175, "npc")), 
  #            color = unique(life_data_list[["undergrad"]][["text_color"]])) +
  role_text(x = 24.5, 
            y = role_annotations_y, 
            label = "masters") +
  role_text_under(x = 24.5, 
                  y = role_annotations_y - 1, 
                  label = "(o.r.)", 
                  color_era = "masters", 
                  size = roles_size) +
  role_text(x = 27, y = role_annotations_y - 2, 
            label = "operations research engineer", 
            lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 26, xend = 26.25, y = -1.75, yend = 0.35), 
             curvature = 0.2,
             arrow = arrow(length = unit(0.0175, "npc")), 
             color = unique(life_data_list[["operations research engineer"]][["text_color"]])) +
  role_text(x = 28.5, 
            y = role_annotations_y - 0.5, 
            label = "statistician", 
            lineheight = annotation_lineheight, 
            size = roles_size) +
  geom_curve(aes(x = 28, xend = 28, y = role_annotations_y, yend = 0.35), 
             curvature = 0.1,
             arrow = arrow(length = unit(0.0175, "npc")), 
             color = unique(life_data_list[["statistician"]][["text_color"]])) +
  role_text(x = 33, y = role_annotations_y - 3, 
            label = "americorps + teacher training", 
            lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 33, xend = 29, y = -2.75, yend = 0.35), 
             curvature = 0.1,
             arrow = arrow(length = unit(0.0175, "npc")), 
             color = unique(life_data_list[["americorps + teacher training"]][["text_color"]])) +
  role_text(x = 37, y = role_annotations_y - 1, 
            label = "middle school teacher", 
            lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 33, xend = 30.5, y = -1.25, yend = 0.35), 
             curvature = -0.2,
             arrow = arrow(length = unit(0.0175, "npc")), 
             color = unique(life_data_list[["middle school teacher"]][["text_color"]])) +
  role_text(x = 35, 
            y = 1, 
            label = "data analyst", 
            lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 34.75, xend = 32.5, y = 0.5, yend = 0.35), 
             curvature = -0.3, 
             arrow = arrow(length = unit(0.0175, "npc")), 
             color = unique(life_data_list[["data analyst"]][["text_color"]]))

#### Location annotations ####

location_color <- "#8c8c8c"
location_annotations_y <- 13

location_text <- function(x, y = location_annotations_y, 
                          label, size = roles_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, 
           color = "#808080", family = "Bradley Hand ITC", ...)
}

life_in_months_final <- life_in_months_role_annotations +
  location_text(x = 9, y = location_annotations_y + 0.1, 
                label = "born + raised in maryland") +
  geom_segment(aes(x = 1, xend = 4, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 14, xend = 18, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), 
               color = location_color) +
  geom_segment(aes(x = 18, xend = 18, y = 12.75, yend = 13.25), 
               color = location_color) +
  location_text(x = 21.5, y = location_annotations_y + 0.1, 
                label = "virginia") +
  geom_segment(aes(x = 19, xend = 20, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 23, xend = 24, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 19, xend = 19, y = 12.75, yend = 13.25), 
               color = location_color) +
  geom_segment(aes(x = 24, xend = 24, y = 12.75, yend = 13.25), 
               color = location_color) +
  location_text(x = 28.5, y = location_annotations_y + 0.1, 
                label = "arizona") +
  geom_segment(aes(x = 25, xend = 27.25, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 29.75, xend = 32, y = 13, yend = 13), 
               color = location_color) +
  geom_segment(aes(x = 25, xend = 25, y = 12.75, yend = 13.25), 
               color = location_color) +
  geom_segment(aes(x = 32, xend = 32, y = 12.75, yend = 13.25), 
               color = location_color) 

life_in_months_final


#### Save final plot ####

ggsave("life_in_months.png",
       plot = life_in_months_final, 
       device = "png", 
       type = "cairo", 
       width = 25, 
       height = 15, 
       dpi = 300)
