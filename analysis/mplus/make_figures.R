# Title:        Make trajectory plots for paper
# Author:       Ewan Carr
# Started:      2021-06-21

library(tidyverse)
library(here)
library(ggthemes)
library(extrafont)
library(lubridate)
load(here("analysis", "outputs", "class_summaries.Rdata"), verbose = TRUE)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "contextual", "uk_lockdown.Rdata"), verbose = TRUE)

font <- "Georgia"

# Prepare 'lockdown' data -----------------------------------------------------

uk_lockdown <- uk_lockdown %>%
    mutate(date = ymd(date),
           class_f = as_factor("Class 1")) %>%
    select(date, in_lockdown, class_f) 

# GMM trajectory plots --------------------------------------------------------

draw_traj <- function(class_data, 
                      context_data, 
                      outcome = "gad",
                      classes = 4,
                      font = "Georgia") {

    # Prepare plot data
    plot_data <- class_data %>%
        filter(y == outcome,
               nclasses == classes) %>%
        mutate(class_f = factor(paste("Class", class)),
               date = as_date(time)) 

    extra <- context_data %>%
        filter(date >= min(plot_data$date),
               date <= max(plot_data$date)) %>%
        mutate(class_f = factor("Class 1"),
               poly_size = max(plot_data$est) + 0.5,
               y = poly_size / 2,
               height = poly_size)

    plot_labels <- plot_data %>%
        left_join(class_size) %>%
        group_by(class_f) %>%
        summarise(last_date = max(date),
                  last_y = last(est),
                  class_label = first(str_glue("{class_f} (n={count}; {round(100 * proportion)}%)")))

    date_breaks <- ymd(c("2020-04-15",
                         "2020-08-15",
                         "2020-12-15",
                         "2021-04-15"))

    # Draw the plot
    p <- plot_data %>%
        ggplot(aes(x = date,
                   colour = class_f,
                   group = class_f)) +
        geom_tile(data = extra,
                  aes(y      = y,
                      height = height,
                      x      = date,
                      fill   = in_lockdown),
                  alpha = 0.1,
                  inherit.aes = FALSE) +
        geom_line(aes(y = est),
                  size = 1.3,
                  alpha = 0.9) +
        geom_point(aes(y = obs),
                   alpha = 0.3) +
        geom_hline(yintercept = 8,
                   color = "red",
                   alpha = 0.2) +
        geom_text(data = plot_labels,
                  family = font,
                  size = 4,
                  aes(x = last_date,
                      y = last_y,
                      label = class_label),
                  nudge_x = 70)

    # Adjust theming
    p <- p + theme_few(base_family = font) +
        theme(axis.title.x         = element_blank(),
              legend.justification = "right",
              legend.position      = c(0.99, 1.06),
              legend.title         = element_blank(),
              legend.margin        = margin(0, 0, 0, 0, "pt"),
              plot.title           = element_text(margin = margin(0, 0, 15, 0, unit = "pt")),
              plot.caption         = element_text(color = "gray50",
                                                  size   = 10,
                                                  hjust  = 0,
                                                  margin = margin(15, 0, 0, 0, unit = "pt"))) +
        scale_color_few(palette = "Dark",
                        guide = "none") +
        scale_fill_manual(values = c("gray95", "gray15"),
                          labels = c("", "National lockdown"),
                          guide = guide_legend(nrow = 1,
                                               direction = "horizontal",
                                               title_position = "left")) +
        expand_limits(x = ymd("2021-08-15")) +
        scale_x_date(breaks = date_breaks, 
                     date_labels = "%b\n%Y")

        return(p)
}

# GAD-7, 4 classes ------------------------------------------------------------
p_gad <- draw_traj(class_summaries, uk_lockdown, "gad", 4) +
    labs(title = "",
         y = "GAD-7 total score",
         caption = "Notes. Explain lockdown indicator/source. Explain dots vs. lines.")

# PHQ-9, 4 classes ------------------------------------------------------------
p_phq <- draw_traj(class_summaries, uk_lockdown, "phq", 4) +
    labs(title = "",
         y = "PHQ-9 total score",
         caption = "Notes. Explain lockdown indicator/source. Explain dots vs. lines.")

# Save ------------------------------------------------------------------------
walk2(list(p_gad, p_phq), 
      c("traj_c4_gad.png", "traj_c4_phq.png"),
      ~ ggsave(.x,
               file = here("analysis", "figures", .y),
               dpi = 1200,
               device = "png",
               width = 7,
               height = 5,
               units = "in"))
