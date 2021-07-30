# Title:        Make figures/tables based on Mplus outputs for use in paper
# Author:       Ewan Carr
# Started:      2021-06-21

library(tidyverse)
library(here)
library(ggthemes)
library(extrafont)
library(lubridate)
library(patchwork)
library(tidytext)
load(here("analysis", "outputs", "class_summaries.Rdata"), verbose = TRUE)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "contextual", "uk_lockdown.Rdata"), verbose = TRUE)
font <- "Franklin Gothic Book"

###############################################################################
####                                                                      #####
####                           TRAJECTORY PLOTS                           #####
####                                                                      #####
###############################################################################

# Prepare 'lockdown' data -----------------------------------------------------

uk_lockdown <- uk_lockdown %>%
    mutate(date = ymd(date),
           class_f = as_factor("Class 1")) %>%
    select(date, in_lockdown, class_f)

# GMM trajectory plots --------------------------------------------------------

make_class_label <- function(class_f, count, proportion) {
    first(str_glue("{class_f} (n={count}; {round(100 * proportion)}%)"))
}

draw_traj <- function(class_data,
                      context_data,
                      outcome = "gad",
                      classes = 4,
                      font = "Georgia") {

    # Prepare plot data
    plot_data <- class_data %>%
        filter(y == outcome,
               nclasses == classes) %>%
        mutate(class_f = factor(paste("Class", new_class)),
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
                  class_label = make_class_label(class_f, count, proportion))

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
        theme(axis.title.x = element_blank(),
              legend.justification = "right",
              legend.position = c(0.99, 1.05),
              legend.key.size = unit(0.5, "cm"),
              legend.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.margin = margin(0, 0, 5, 0, "pt"),
              plot.title = element_text(margin = margin(0, 0, 8, 0,
                                                        unit = "pt")),
              plot.caption = element_text(color = "gray50",
                                          size   = 10,
                                          hjust  = 0,
                                          margin = margin(15, 0, 0, 0,
                                                          unit = "pt"))) +
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

p_gad <- draw_traj(class_summaries, uk_lockdown, "gad", 4, font) +
    labs(title = "A. GAD-7",
         y = "GAD-7 total score")

# PHQ-9, 4 classes ------------------------------------------------------------
p_phq <- draw_traj(class_summaries, uk_lockdown, "phq", 4, font) +
    labs(title = "B. PHQ-9",
         y = "PHQ-9 total score") +
    theme(legend.position = "none")


# Combine plots ---------------------------------------------------------------
samp <- sum(class_size[class_size$nclasses == 4 &
                       class_size$y == "gad", ]$count)
tit <- str_glue("Trajectory plots for 4-class GMM models (n={samp})")
cap <- "Notes. Explain lockdown indicator/source. Explain dots vs. lines."

p_comb <- p_gad / p_phq +
    plot_annotation(# title = tit,
                    caption = cap,
                    theme = theme(title = element_text(family = font,
                                                       size = 14)))

# Save ------------------------------------------------------------------------
ggsave(p_comb,
       file = here("analysis", "figures", "traj.png"),
       dpi = 1200,
       device = "png",
       width = 8,
       height = 10,
       units = "in")


###############################################################################
####                                                                      #####
####                          R3STEP odds ratios                          #####
####                                                                      #####
###############################################################################

lookup <- read_csv(here("analysis",
                        "mplus",
                        "r3step_lookup.csv")) %>%
    filter(group != "Ethnicity") %>%
    drop_na(rank) %>%
    mutate(group = factor(group, levels = na.omit(unique(group))),
           label = factor(label, levels = na.omit(unique(label))))

lookup <- cross_df(list(y = c("gad", "phq"),
                        new_class = 1:3,
                        new_ref = 4)) %>%
    crossing(lookup)

or <- odds_ratios %>%
    filter(# Select reference class = 4
           new_ref == 4,
           new_class != 4,
           # Remove coefficients we can't/won't plot
           str_detect(model_id, "_age$|_sex$|_ethnicity$", negate = TRUE),
           !(param %in% c("shield_i", "is_staff")),
           # Remove duplicated 'age' and 'sex' rows
           (!(str_detect(model_id, "_agesex$", negate = TRUE) &
              param %in% c("age", "female", "age10"))),
           (adj == "adj" | str_detect(model_id, "_agesex$"))) %>%
    select(y, new_ref, new_class, param, est, lo = low025, hi = up025) %>%
    full_join(lookup) %>%
    mutate(refcat = if_else(is.na(est), "ref", y),
           across(c(est, lo, hi), ~ replace_na(as.numeric(.x), 1)),
           class_lab = case_when(
                new_class == 1 ~ "Class 1\n'Persistent high severity'",
                new_class == 2 ~ "Class 2\n'Against cases'",
                new_class == 3 ~ "Class 3\n'With cases'"),
           nudge = case_when(y == "gad" ~ 0.25,
                             y == "phq" ~ -0.25,
                             TRUE ~ 0))

p_r3step <- ggplot(or,
       aes(x = est,
           xmin = lo,
           xmax = hi,
           y = reorder_within(label, -as.numeric(label), group),
           group = y,
           color = y)) +
    geom_vline(xintercept = 1, color = "red", alpha = 0.1) +
    geom_point(aes(color = refcat),
                    size = 1.6,
                    position = position_dodge(width = 0.6)) +
    geom_linerange(aes(color = refcat),
                    size = 0.7,
                    position = position_dodge(width = 0.6)) +
    facet_grid(rows = vars(group),
               cols = vars(class_lab),
               scale = "free",
               space = "free_y",
               switch = "y") +
    coord_cartesian(xlim = c(0, 7)) +
    scale_y_reordered() +
    scale_color_manual(values = c("#FAA43A", "#5DA5DA", "#cccccc"),
                       labels = c("GAD-7", "PHQ-9"),
                       breaks = c("gad", "phq")) +
    theme_few(base_family = font) +
    theme(strip.text.y.left = element_text(angle = 0,
                                           hjust = 1,
                                           color = "gray60"),
          plot.title.position = "plot",
          axis.title.y = element_blank(),
          axis.text.y = element_text(color = "black"),
          strip.placement = "outside",
          legend.justification = "right",
          legend.direction = "horizontal",
          legend.position      = c(0.99, 1.12),
          legend.key.size      = unit(0.5, "cm"),
          legend.text          = element_text(size = 10),
          legend.title         = element_blank(),
          legend.margin        = margin(0, 0, 5, 0, "pt")) +
    labs(title = paste0("Associations of baseline variables with trajectory ",
                        "class assignment"),
         subtitle = "Reference class is 'low severity'.",
         x = "Odds ratio (95% confidence interval)",
         caption = "Notes.",
         color = "Outcome")


# Save ------------------------------------------------------------------------
ggsave(p_r3step,
       file = here("analysis", "figures", "r3step.png"),
       dpi = 600,
       device = "png",
       width = 10,
       height = 8,
       units = "in")




###############################################################################
####                                                                      #####
####                                TVCOVs                                #####
####                                                                      #####
###############################################################################

# For now, we're constraining the TVCs to be constant across classes. We might
# want to relax this later.

date_lookup <- sel %>%
    ungroup() %>%
    distinct(dap, midpoint) %>%
    drop_na()

label_lookup <- tribble(
~tvc, ~tvc_lab,
"stmat",  "Material\nstressors",
"stmed",  "Medical",
"stper", "Personal",
"stpla", "Plans",
)

date_breaks <- ymd(c("2020-04-15",
                     "2020-08-15",
                     "2020-12-15",
                     "2021-04-15"))


plot_data <- tvcov_coef %>%
    filter(orig_class == 1,
           str_starts(tvc, "st")) %>%
    left_join(date_lookup, by = "dap") %>%
    left_join(label_lookup) %>%
    mutate(midpoint = as.Date(midpoint),
           outcome = if_else(y == "gad",
                             "Anxiety (GAD-7)",
                             "Depression (PHQ-9)"))

extra <- uk_lockdown %>%
    filter(date >= min(plot_data$midpoint),
           date <= max(plot_data$midpoint)) %>%
    mutate(height = 6,
           y = min(plot_data$est))

p_tvc <- plot_data %>%
    ggplot(aes(x = midpoint,
               y = est,
               color = outcome,
               ymin = low2_5,
               ymax = up2_5)) +
    geom_tile(data = extra,
              aes(y = y,
                  height = height,
                  x = date,
                  fill   = in_lockdown),
              alpha = 0.1,
              inherit.aes = FALSE) +
    geom_linerange(size = 0.4) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0,
               colour = "red",
               alpha = 0.2) +
    facet_grid(rows = vars(tvc_lab),
               cols = vars(outcome)) +
    theme_few(base_family = font) +
    scale_color_manual(values = c("#dd9133", "#5DA5DA"),
                       labels = c("GAD-7", "PHQ-9"),
                       guide = "none") +
    scale_fill_manual(values = c("gray95", "gray15"),
                      labels = c("", "National lockdown"),
                      guide = guide_legend(nrow = 1,
                                           direction = "horizontal")) +
    scale_x_date(breaks = date_breaks,
                 date_labels = "%b\n%Y") +
    labs(title = paste0("Change in symptoms of anxiety and depression ",
                        "associated\nwith time-varying COVID stressors"),
         y = "Score",
         fill = "") +
    theme(axis.title.x = element_blank(),
          strip.text.x = element_text(size = 14,
                                    margin = margin(10, 0, 10, 0, "pt")),
          strip.text.y = element_text(angle = 0,
                                      size = 13,
                                      margin = margin(0, 15, 0, 15, "pt")),
          plot.title = element_text(size = 18,
                                    margin = margin(10, 0, 5, 0, "pt")),
          axis.text = element_text(size = 12),
          legend.justification = "right",
          legend.position = c(1.13, 1.14),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 5, 0, "pt")) +
                        coord_cartesian(ylim = c(-1, 2))

# Save ------------------------------------------------------------------------
ggsave(p_tvc,
       file = here("analysis", "figures", "tvc.png"),
       dpi = 600,
       device = "png",
       width = 9,
       height = 8,
       units = "in")
