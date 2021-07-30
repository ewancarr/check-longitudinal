# Title:        Table showing schedule of longitudinal surveys and response
# Author:       Ewan Carr
# Started:      2021-07-29

renv::load()
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)
library(extrafont)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "samples.Rdata"), verbose = TRUE)

sel <- filter(sel, pid %in% samples$s3)

schedule <- sel %>%
    select(pid, dap, two_month, midpoint, gad_total, phq_total) %>%
    ungroup() %>%
    count(dap, midpoint, two_month) %>%
    mutate(total = max(n),
           rr = round(100 * (n / total)),
           type = if_else(two_month, "Two-monthly", "Fortnightly"),
           y = year(midpoint),
           m = month(midpoint, label = TRUE, abbr = FALSE),
           d = scales::ordinal(day(midpoint)),
           across(c(y, m), ~ if_else(.x == lag(.x) & !is.na(lag(.x)), 
                                     "", as.character(.x)))) %>%
    select(y, m, d, type, n, rr)  

write_csv(schedule, "~/schedule.csv")
