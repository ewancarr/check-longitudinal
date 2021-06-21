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
           in_lockdown = (c2_workplace_closing == 3) &              # [1]
                         (c6_stay_at_home_requirements == 2))       # [2]

    # [1]: require closing (or work from home) for all-but-essential workplaces
    #      (eg grocery stores, doctors)
    # [2]: require not leaving house with exceptions for daily exercise,
    #      grocery shopping, and 'essential' trips

save(uk_lockdown, file = here("data", "clean", "contextual",
                              "uk_lockdown.Rdata"))

# table(ox$country_name)

# gov_uk <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=hospitalCases&metric=newAdmissions&metric=newCasesByPublishDate&metric=cumDeaths28DaysByDeathDate&format=csv"
# d1 <- read_csv(gov_uk) %>%
#     clean_names() %>%
#     select(date,
#            deaths28 = cum_deaths28days_by_death_date,
#            hospital_cases, 
#            new_admissions,
#            new_cases = new_cases_by_publish_date)

# save(d1, file = here("data", "raw", "contextual", "gov_d1.Rdata"))

# oxcgrt <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

# url <- "https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/2020-04-01/2021-04-01"

# oxcgrt <- fromJSON(url)

# oxcgrt$data[[1]][[1]]


# oxcgrt %>%
#     filter(CountryName == "United Kingdom") %>%
#     clean_names() %>%
#     mutate(date = as_date(date))



