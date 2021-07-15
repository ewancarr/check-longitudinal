# Title:        Make table/figure for SMT report
# Author:       Ewan Carr
# Started:      2021-07-14

library(tidyverse)
library(here)
library(ggthemes)
library(extrafont)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "samples.Rdata"), verbose = TRUE)

# Response rate / number responding at each survey period ---------------------

sel <- filter(sel, pid %in% samples$s3)

p <- sel %>%
    select(pid, dap, midpoint, two_month, gad_total, phq_total) %>%
    drop_na() %>%
    group_by(midpoint, two_month, dap) %>%
    count(midpoint) %>%
    ungroup() %>%
    mutate(total = max(n),
           rr = n / total,
           survey_type = if_else(two_month, "Two-monthly", "Fortnightly")) %>%
    ggplot(aes(x = as.Date(midpoint),
               y = rr * 100,
               color = survey_type)) +
    geom_point() +
    geom_text(aes(label = n),
              size = 2.7,
              nudge_y = -4,
              show.legend = FALSE) +
    labs(y = "Response rate (%)") +
    theme_few(base_family = "Arial") +
    theme(axis.title.x = element_blank(),
          legend.position = c(0.9, 0.15),
          legend.key.height = unit(15, "pt")) +
    labs(color = "Survey type") +
    scale_y_continuous(limits = c(0, 100)) +
    scale_color_few() +
    scale_x_date(date_labels = "%b\n%Y")

ggsave(p,
       file = here("analysis", "eda", "smt_report", "response_rate.png"),
       dpi = 300,
        width = 8,
        height = 4)


# Individual trajectories -----------------------------------------------------

library(lme4)
library(rms)
library(ggeffects)
library(broom)
library(broom.mixed)

opt <- lmerControl(optimizer = "bobyqa")
m_phq <- lmer(phq_total ~ rcs(dap) + (rcs(dap) | pid),
              data = sel,
              control = opt)
m_gad <- lmer(gad_total ~ rcs(dap) + (rcs(dap) | pid),
              data = sel,
              control = opt)

date_lookup <- sel %>%
    ungroup() %>%
    distinct(dap, midpoint)

relabel_outcomes <- function(dat) {
    dat %>%
    mutate(y = case_when(y == "gad" ~ "Anxiety GAD-7",
                         y == "phq" ~ "Depression (PHQ-9)"))
}

# Get individual predictions
ind_pred <- distinct(sel, pid, dap, midpoint)
ind_pred$phq <- predict(m_phq, ind_pred)
ind_pred$gad <- predict(m_gad, ind_pred)
ind_pred <- ind_pred %>%
    ungroup() %>%
    gather(y, pred, -pid, -dap, -midpoint) %>%
    mutate(midpoint = as.Date(midpoint)) %>%
    relabel_outcomes()

# Get predictions for entire sample
total <- sel %>%
    ungroup() %>%
    distinct(dap, midpoint)
total$gad <- predict(m_gad, total, re.form = NA)
total$phq <- predict(m_phq, total, re.form = NA)
total <- total %>%
    gather(y, pred, -dap, -midpoint) %>%
    mutate(midpoint = as.Date(midpoint)) %>%
    relabel_outcomes()

p <- ind_pred %>%
    ggplot(aes(x = midpoint,
               y = pred,
               group = pid)) +
    geom_line(alpha = 0.04) +
    geom_line(data = total,
              aes(group = NA),
              color = "red",
              size = 1.5) +
    facet_wrap(~ y) +
    scale_x_date(date_labels = "%b\n%Y") +
    theme_few(base_family = "Arial") +
    labs(y = "Score") +
    theme(axis.title.x = element_blank())

ggsave(p,
       file = here("analysis", "eda", "smt_report", "ind_traj.png"),
       dpi = 300,
        width = 8,
        height = 4)
