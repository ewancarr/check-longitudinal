# Title:        Prepare numbers needed for longitudinal paper
# Author:       Ewan Carr
# Started:      2021-07-01

renv::load()
library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(janitor)
library(ggridges)
source(here("analysis", "functions", "generic.R"))
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "samples.Rdata"), verbose = TRUE)
load(here("data", "clean", "aw.Rdata"), verbose = TRUE)
load(here("data", "clean", "bl.Rdata"), verbose = TRUE)
latest_extract <- "2021-05-12"

# Schedule of longitudinal questionnaires -------------------------------------

f <- here("data", "raw", "survey", latest_extract, "master_tracker.xlsx")

left <- read_xlsx(f, sheet = "Total Counts", skip = 20) %>%
    select(t = Period, type = Type)

right <- read_xlsx(f, sheet = "Schedule", range = "A1:B41") %>%
    select(t = `Batch 1`, dt = Date)

schedule <- full_join(left, right) %>%
    filter(dt < ymd(latest_extract)) 

# Number of questionnaires
schedule %>%
    count(type)

range(schedule$dt)

# Samples ---------------------------------------------------------------------

sn <- map(samples, length)

# At least one follow-up assessment of PHQ-9 and GAD-7 
sn$s1 - sn$s2

# Missing information on baseline covariates
sn$s2 - sn$s3

# Compare included vs. excluded sample
incl <- filter(aw,
               t == 0,
               pid %in% samples$s3) %>%
    mutate(female = gender == "Female",
           age = as.numeric(age),
           samp = "incl",
           is_staff = str_detect(role, "staff")) %>%
    recode_ethnicity() %>%
    select(pid, age, female, is_staff, samp, ethnic_f)


excl <- bl %>%
    swap_names() %>%
    left_join(ids) %>%
    filter(pid %in% samples$s1,
           !(pid %in% samples$s3)) %>%
    mutate(age = as.numeric(age),
           female = gender == "Female",
           samp = "excl",
           is_staff = str_detect(role, "staff")) %>%
    recode_ethnicity() %>%
    select(pid, age, female, is_staff, ethnic_f, samp)

comp <- bind_rows(incl, excl) %>%
    group_by(samp) 

# Compare age and gender
summarise(comp, across(c(age, female), .fns = list(mean = mean, sd = sd)))
t.test(comp$age ~ comp$samp)
chisq.test(comp$female, comp$samp)

# Compare ethnicity
count(comp, ethnic_f) %>%
    group_by(samp) %>%
    mutate(total = sum(n),
           pct = n / total,
           cell = str_glue("{n}/{total} ({round(pct*100, 1)})")) %>%
    select(samp, ethnic_f, cell) %>%
    spread(samp, cell)

# Response rate ---------------------------------------------------------------

# Load HR data for staff
hr <- readxl::read_xlsx(here("data", "raw", "admin", "staff",
                         "2411_AllStaffD&I_nopassword.xlsx"),
                    sheet = 3) %>%
    clean_names()

# Load HESA data for PGR students
hesa <- read_xlsx(here("data", "raw", "admin", "pgr",
                       "2020-06-09 Updated numbers from HESA",
                       "Student PGR HESA Standard Registration 1819.xlsx")) %>%
    select(age = Age,
           female = Female,
           male = Male) %>%
    group_by(age) %>%
    summarise(across(female:male, sum)) %>%
    mutate(total = female + male)

# PGR response rate
sum(!incl$is_staff) / sum(hesa$total)

# Staff response rate
sum(incl$is_staff) / nrow(hr)

# Within-time variance of time ------------------------------------------------

# This is item 2 on the GRoLTS checklist. It tests the extent of variation of 
# of response time within each survey period.

plot_data <- aw %>%
  select(pid, t, end_date) %>%
  right_join(sel) %>%
  group_by(dap) %>%
  mutate(t_start = min(end_date)) 

plot_data %>%
  summarise(mid = median(end_date),
            low = min(end_date),
            high = max(end_date),
            pct = mean(end_date <= t_start + days(7))) %>%
  mutate(pct = sprintf("%.2f", pct),
         across(c(mid, low, high), as_date)) %>%
  write_csv("~/time_variance.csv")

  mutate(mid = median(end_date),
            low = min(end_date),
            high = max(end_date)) %>%
  summarise(pct = end_date < midpoint + days(4))


p <- plot_data %>%
  ggplot(aes(y = factor(dap), 
             x = end_date,
             group = dap)) +
  geom_density_ridges(scale = 2.5) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) +
  labs(y = "Weeks since baseline")
ggsave(p, 
       filename = here("analysis", "figures", "time_variance.png"),
       width = 8,
       height = 5,
       dpi = 300,
       dev = "png")


  summarise(mid = median(end_date),
            low = min(end_date),
            high = max(end_date))
