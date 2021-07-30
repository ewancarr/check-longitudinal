# Title:        Import contextual data
# Author:       Ewan Carr
# Started:      2021-04-22

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# Lockdown --------------------------------------------------------------------
ox <- read_csv(here("data", "raw", "contextual", "oxcgrt",
                    "OxCGRT_latest.csv")) %>%
    clean_names() %>%
    filter(country_name == "United Kingdom")

uk_lockdown <- ox %>%
    filter(jurisdiction == "NAT_TOTAL") %>%
    mutate(date = ymd(date),
           in_lockdown = (c6_stay_at_home_requirements %in% c(2, 3)))  # [A]

    # [A]
    # 1 = recommend not leaving house
    # 2 = require not leaving house with exceptions for daily exercise,
    #     grocery shopping, and 'essential' trips
    # 3 = require not leaving house with minimal exceptions (eg allowed to
    #     leave once a week, or only one person can leave at a time, etc)

save(uk_lockdown, file = here("data", "clean", "contextual",
                              "uk_lockdown.Rdata"))

