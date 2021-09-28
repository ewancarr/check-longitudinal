# Title:      Weighted means of PHQ-9 and GAD-7 by age and gender
# Date:       2021-07-06
# Updated:    2021-07-30

renv::load()
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)
library(colorspace)
library(extrafont)
library(patchwork)
library(srvyr)
chosen_font <- "Arial"
load(here("data", "clean", "pseudo_anon.Rdata"), verbose = TRUE)
load(here("data", "clean", "contextual", "uk_lockdown.Rdata"), verbose = TRUE)

# Select analytical sample ----------------------------------------------------

load(here("data", "clean", "samples.Rdata"), verbose = TRUE)
sel <- sel %>%
    filter(pid %in% samples$s3)

# Recode age/gender; select first non-missing value per person ----------------
sel$agecat <- factor(cut(sel$age, 
                         breaks = c(-Inf, 16, 34, 54, Inf)),
                     labels = c("16-34", "35-54", "55+"))

sel$female <- ifelse(sel$female, "Female", "Male")

sel <- sel %>%
    group_by(pid) %>%
    mutate(across(c(female, agecat), ~ first(na.omit(.x))))

# Add a copy of the dataset to represent 'Total' category ---------------------
sel <- select(sel,
              pid, w_comb, gad_total, phq_total, midpoint, agecat, female)

total <- sel %>%
  ungroup() %>%
  mutate(agecat = "Total")

sel <- bind_rows(sel, total)

# Function to generate plot data, for age group OR gender ---------------------

prepare_data <- function(sel, lockdown, var) {
    # Prepare 'Total' group
    total <- sel %>%
      ungroup() %>%
      mutate({{var}} := "Total")

    # Prepare weighted means, stratified by chosen variable
    dat <- sel %>%
        bind_rows(total) %>%
        select(w_comb, pid, midpoint, {{var}}, gad_total, phq_total) %>%
        bind_rows(total) %>%
        group_by(midpoint, {{var}}) %>%
        summarise(midpoint = first(na.omit(midpoint)),
                  across(c(phq_total, gad_total),
                         weighted.mean,
                         w = w_comb,
                         na.rm = TRUE)) %>%
        mutate(midpoint = as.Date(midpoint)) %>%
        gather(measure, value, -midpoint, -{{var}}) %>%
        arrange(midpoint, {{var}}) %>%
        drop_na() %>%
        mutate(outcome = case_when(measure == "phq_total" ~ "Symptoms of depression (PHQ-9)",
                                   measure == "gad_total" ~ "Symptoms of anxiety (GAD-7)"))

    # Prepare contextual data on UK lockdown dates
    lock <- lockdown %>%
      mutate(date = ymd(date)) %>%
      select(date, in_lockdown) %>%
      filter(date >= min(dat$midpoint, na.rm = TRUE),
             date <= max(dat$midpoint, na.rm = TRUE)) %>%
      mutate({{var}} := factor("Total"),
             poly_size = max(dat$value, na.rm = TRUE) + 0.5,
             y = poly_size / 2,
             height = poly_size)

      return(list(plot_data = dat,
                  lockdown = lock))
}

# Function to generate the plot -----------------------------------------------

make_the_plot <- function(sel, lock, var) {
    d <- prepare_data(sel, lock, {{var}})
    date_breaks <- ymd(c("2020-04-15",
                         "2020-08-15",
                         "2020-12-15",
                         "2021-04-15"))
    p <- ggplot(rename(d$plot_data, byvar := {{var}}),
                aes(x = midpoint,
                    y = value,
                    group = byvar,
                    color = byvar)) +
      geom_tile(data = d$lockdown,
                aes(y      = y,
                    height = height,
                    x      = date,
                    fill   = in_lockdown),
                alpha = 0.1,
                inherit.aes = FALSE) +
      geom_line(size = 0.8) +
      geom_point(size = 2) +
      theme_few(base_family = chosen_font) +
      geom_hline(yintercept = 4,
                 size = 0.5,
                 color = "gray50",
                 alpha = 0.5,
                 linetype = "dotted") +
      scale_color_few(palette = "Medium") +
      scale_x_date(breaks = date_breaks,
                   date_labels = "%b\n%Y") +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(face = "bold",
                                      margin = margin(1, 0, 0, 0, "cm")),
            strip.text = element_text(size = 13),
            strip.background = element_blank(),
            legend.title = element_blank(),
            legend.key.height = unit(0.4, "cm"),
            legend.position = c(0.88, 1.1),
            plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm")) +
      scale_fill_manual(values = c("gray95", "gray15"),
                        labels = c("", "National lockdown")) +
      facet_wrap(~ outcome, ncol = 1) +
      guides(fill = "none",
             shape = "none")
      return(p)
}

add_label <- function(ypos, lab) {
        annotate("text",
                 x = ymd("2020-04-15"),
                 y = ypos,
                 label = lab,
                 size = 3,
                 family = chosen_font,
                 fontface = "italic",
                 hjust = 0,
                 color = "gray50")
}

# Pick some colours for the lines
plot_colors <- few_pal(palette = "Dark")(8)

# Make plot for age
p_left <- make_the_plot(sel, uk_lockdown, agecat) +
        labs(y = "Weighted total score",
             title = "By age") +
        add_label(3.75, "None") +
        add_label(4.25, "Mild symptoms") +
        scale_color_manual(values = c(plot_colors[c(8, 3, 6)], "gray60"))
p_left

# Make plot for gender
p_right <- make_the_plot(sel, uk_lockdown, female) +
      labs(title = "By gender") +
      theme(axis.title.y = element_blank()) +
      scale_color_manual(values = c(plot_colors[c(1, 4)], "gray60"))

# Combine into a single figure
cap <- str_squish("Notes. Shaded grey regions indicate periods of national
                  lockdown, as measured by the Oxford Covid-19 Government 
                  Response Tracker (OxCGRT).")

p_final <- p_left + p_right + 
    plot_annotation(caption = cap,
                    theme = theme(
                                  plot.caption = element_text(family = chosen_font)
                    )
    )

ggsave(p_final,
       filename = here("analysis", "figures", "desc.png"),
       dev = "png",
       dpi = 300,
       width = 9,
       height = 7,
       units = "in")
