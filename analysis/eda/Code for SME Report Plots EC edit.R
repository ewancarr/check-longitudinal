# Title:      Weight mean PHQ-9 and GAD-7 by age, for SMT report
# Date:       2021-07-06

renv::load()
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)
library(extrafont)
library(patchwork)
library(srvyr)
chosen_font <- "Calibri"
load(here("data", "clean", "pseudo_anon.Rdata"), verbose = TRUE)
load(here("data", "clean", "contextual", "uk_lockdown.Rdata"), verbose = TRUE)

uk_lockdown <- uk_lockdown %>%
  mutate(date = ymd(date),
         class_f = as_factor("Class 1")) %>%
  select(date, in_lockdown, class_f)


# Create categorical variables for 3 age groups 16-34, 35-54, 55+ =============

sel <- sel %>%
  ungroup() %>%
  mutate(agecat = factor(cut(age,
                             breaks = c(-Inf, 16, 34, 54, Inf)),
                         labels = c("16-34", "35-54", "55+"))) %>%
  group_by(pid) %>%
  mutate(agecat = first(na.omit(agecat)))

# Create 'total' dataset ------------------------------------------------------

total <- sel %>%
  ungroup() %>%
  select(pid, w_comb, gad_total, phq_total, midpoint) %>%
  mutate(agecat = "Total")

# Prepare plot data -----------------------------------------------------------

plot_data <- sel %>%
  select(w_comb, pid, midpoint, agecat, gad_total, phq_total) %>%
  bind_rows(total) %>%
  group_by(midpoint, agecat) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            across(c(phq_total, gad_total),
                   weighted.mean,
                   w = w_comb,
                   na.rm = TRUE)) %>%
  mutate(midpoint = as.Date(midpoint)) %>%
  gather(measure, value, -midpoint, -agecat) %>%
  arrange(midpoint, agecat) %>%
  drop_na() %>%
  mutate(outcome = case_when(measure == "phq_total" ~ "PHQ-9 total score",
                             measure == "gad_total" ~ "GAD-7 total score"))

# Prepare data on lockdown dates ----------------------------------------------

extra <- uk_lockdown %>%
  filter(date >= min(plot_data$midpoint, na.rm = TRUE),
         date <= max(plot_data$midpoint, na.rm = TRUE)) %>%
  mutate(agegroup = factor("Total"),
         poly_size = max(plot_data$value, na.rm = TRUE) + 0.5,
         y = poly_size / 2,
         height = poly_size)

# Using ggplot to plot the data frame defined above ---------------------------

date_breaks <- ymd(c("2020-04-15",
                     "2020-08-15",
                     "2020-12-15",
                     "2021-04-15"))

p <- ggplot(plot_data,
            aes(x = midpoint,
                y = value,
                group = agecat,
                color = agecat)) +
  geom_tile(data = extra,
            aes(y      = y,
                height = height,
                x      = date,
                fill   = in_lockdown),
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_few(base_family = chosen_font) +
  scale_color_few() +
  scale_x_date(breaks = date_breaks,
               date_labels = "%b\n%Y") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = c("gray95", "gray15"),
                    labels = c("", "National lockdown")) +
  labs(y = "Weighted score",
       color = "Age group",
       fill = "") +
  facet_wrap(~ outcome, ncol = 1)

ggsave(p,
       filename = "final_age_plot4.png",
       dev = "png",
       dpi = 300,
       width = 7,
       height = 6, units = "in")

################################################################################
################################################################################

#GENDER

gender <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_female = female)

data_wrangled_phqtotal_female2 <- sel %>%
  left_join(gender, by = "pid") %>%
  group_by(dap, bl_female) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            w_comb_phqtotalav = weighted.mean(phq_total, w_comb, na.rm = TRUE))

# Create 'total' dataset ------------------------------------------------------

total <- sel %>%
  ungroup() %>%
  select(pid, w_comb, gad_total, phq_total, midpoint) %>%
  mutate(female = "Total")

# Prepare plot data -----------------------------------------------------------

plot_data <- sel %>%
  mutate(female = if_else(female, "Female", "Male")) %>%
  select(w_comb, pid, midpoint, female, gad_total, phq_total) %>%
  bind_rows(total) %>%
  group_by(midpoint, female) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            across(c(phq_total, gad_total),
                   weighted.mean,
                   w = w_comb,
                   na.rm = TRUE)) %>%
  mutate(midpoint = as.Date(midpoint)) %>%
  gather(measure, value, -midpoint, -female) %>%
  arrange(midpoint, female) %>%
  drop_na() %>%
  mutate(outcome = case_when(measure == "phq_total" ~ "PHQ-9 total score",
                             measure == "gad_total" ~ "GAD-7 total score"))


# Prepare data on lockdown dates ----------------------------------------------

extra <- uk_lockdown %>%
  filter(date >= min(plot_data$midpoint, na.rm = TRUE),
         date <= max(plot_data$midpoint, na.rm = TRUE)) %>%
  mutate(female = factor("Total"),
         poly_size = max(plot_data$value, na.rm = TRUE) + 0.5,
         y = poly_size / 2,
         height = poly_size)

# Using ggplot to plot the data frame defined above ---------------------------

date_breaks <- ymd(c("2020-04-15",
                     "2020-08-15",
                     "2020-12-15",
                     "2021-04-15"))

p <- ggplot(plot_data,
            aes(x = midpoint,
                y = value,
                group = female,
                color = female)) +
  geom_tile(data = extra,
            aes(y      = y,
                height = height,
                x      = date,
                fill   = in_lockdown),
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_few(base_family = chosen_font) +
  scale_color_few() +
  scale_x_date(breaks = date_breaks,
               date_labels = "%b\n%Y") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = c("gray95", "gray15"),
                    labels = c("", "National lockdown")) +
  labs(y = "Weighted score",
       color = "Gender",
       fill = "") +
  facet_wrap(~ outcome, ncol = 1)

ggsave(p,
       filename = "final_gender_plot4.png",
       dev = "png",
       dpi = 300,
       width = 7,
       height = 6, units = "in")

###############################################################################
################################################################################
#Total weighted PHQ 
data_wrangled_phqtotal <- sel %>%
  group_by(dap) %>% 
  summarise(phqtotalav = mean(phq_total, na.rm = TRUE),
            w_bl_phqtotalav = weighted.mean(phq_total, w_bl, na.rm = TRUE),
            w_comb_phqtotalav = weighted.mean(phq_total, w_comb, na.rm = TRUE))

# Create 'total' dataset ------------------------------------------------------

total <- sel %>%
  ungroup() %>%
  select(pid, w_comb, gad_total, phq_total, midpoint)

# Prepare plot data -----------------------------------------------------------

plot_data <- sel %>%
  select(w_comb, pid, midpoint, gad_total, phq_total) %>%
  bind_rows(total) %>%
  group_by(midpoint) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            across(c(phq_total, gad_total),
                   weighted.mean,
                   w = w_comb,
                   na.rm = TRUE)) %>%
  mutate(midpoint = as.Date(midpoint)) %>%
  gather(measure, value, -midpoint) %>%
  arrange(midpoint) %>%
  drop_na() %>%
  mutate(outcome = case_when(measure == "phq_total" ~ "PHQ-9 total score",
                             measure == "gad_total" ~ "GAD-7 total score"))

# Prepare data on lockdown dates ----------------------------------------------

extra <- uk_lockdown %>%
  filter(date >= min(plot_data$midpoint, na.rm = TRUE),
         date <= max(plot_data$midpoint, na.rm = TRUE)) %>%

# Using ggplot to plot the data frame defined above ---------------------------

date_breaks <- ymd(c("2020-04-15",
                     "2020-08-15",
                     "2020-12-15",
                     "2021-04-15"))

p <- ggplot(plot_data,
            aes(x = midpoint,
                y = value)) +
  geom_tile(data = extra,
            aes(y      = y,
                height = height,
                x      = date,
                fill   = in_lockdown),
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_few(base_family = chosen_font) +
  scale_color_few() +
  scale_x_date(breaks = date_breaks,
               date_labels = "%b\n%Y") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = c("gray95", "gray15"),
                    labels = c("", "National lockdown")) +
  labs(y = "Weighted score",
       color = "Age group",
       fill = "") +
  facet_wrap(~ outcome, ncol = 1)

ggsave(p,
       filename = "final_totals_plot4.png",
       dev = "png",
       dpi = 300,
       width = 7,
       height = 6, units = "in")



################################################################################
################################################################################

#Student vs staff

role <- sel %>% 
  filter(dap == 0)%>%
  select(pid, bl_staff = is_staff)

data_wrangled_phqtotal_is_staff <- sel %>%
  left_join(role, by = "pid") %>%
  group_by(dap, bl_staff) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            w_comb_phqtotalav = weighted.mean(phq_total, w_comb, na.rm = TRUE))


# Create 'total' dataset ------------------------------------------------------

total <- sel %>%
  ungroup() %>%
  select(pid, w_comb, gad_total, phq_total, midpoint) %>%
  mutate(is_staff = "Total")

# Prepare plot data -----------------------------------------------------------

plot_data <- sel %>%
  mutate(is_staff = if_else(is_staff, "Staff", "PGRS")) %>%
  select(w_comb, pid, midpoint, is_staff, gad_total, phq_total) %>%
  bind_rows(total) %>%
  group_by(midpoint, is_staff) %>%
  summarise(midpoint = first(na.omit(midpoint)),
            across(c(phq_total, gad_total),
                   weighted.mean,
                   w = w_comb,
                   na.rm = TRUE)) %>%
  mutate(midpoint = as.Date(midpoint)) %>%
  gather(measure, value, -midpoint, -is_staff) %>%
  arrange(midpoint, is_staff) %>%
  drop_na() %>%
  mutate(outcome = case_when(measure == "phq_total" ~ "PHQ-9 total score",
                             measure == "gad_total" ~ "GAD-7 total score"))


# Prepare data on lockdown dates ----------------------------------------------

extra <- uk_lockdown %>%
  filter(date >= min(plot_data$midpoint, na.rm = TRUE),
         date <= max(plot_data$midpoint, na.rm = TRUE)) %>%
  mutate(is_staff = factor("Total"),
         poly_size = max(plot_data$value, na.rm = TRUE) + 0.5,
         y = poly_size / 2,
         height = poly_size)

# Using ggplot to plot the data frame defined above ---------------------------

date_breaks <- ymd(c("2020-04-15",
                     "2020-08-15",
                     "2020-12-15",
                     "2021-04-15"))

p <- ggplot(plot_data,
            aes(x = midpoint,
                y = value,
                group = is_staff,
                color = is_staff)) +
  geom_tile(data = extra,
            aes(y      = y,
                height = height,
                x      = date,
                fill   = in_lockdown),
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_few(base_family = chosen_font) +
  scale_color_few() +
  scale_x_date(breaks = date_breaks,
               date_labels = "%b\n%Y") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = c("gray95", "gray15"),
                    labels = c("", "National lockdown")) +
  labs(y = "Weighted score",
       color = "Role",
       fill = "") +
  facet_wrap(~ outcome, ncol = 1)

ggsave(p,
       filename = "final_role_plot4.png",
       dev = "png",
       dpi = 300,
       width = 7,
       height = 6, units = "in")

################################################################################
################################################################################

#### X% probable depression in April 2020 and X % probable depression in January 2021

#Weighted proportions for gender for gad caseness >= 10
des_rep <- sel %>%
  as_survey_design(id = pid, weights = rw)

des_rep %>%
  mutate(gad_case = gad_total >= 10) %>%
  group_by(female, gad_case) %>%
  summarise(prop = survey_prop("ci")) %>%
  mutate(cell = str_glue("{odp(prop)} [{odp(prop_low)}, {odp(prop_upp)}]"))  %>%
  select(female, gad_case, cell) %>%
  spread(gad_case, cell) %>% 
  select(`TRUE`)