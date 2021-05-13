# Title:        Data cleaning for KCL-CHECK longitudinal paper
# Author:       Ewan Carr
# Started:      2021-02-23

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(janitor)
noisy <- FALSE
source(here("analysis", "functions", "generic.R"))
latest <- "2021-05-12"
load(here("data", "clean", "aw.Rdata"), verbose = TRUE)

# Identify non-response -------------------------------------------------------

aw <- aw %>%
        # Individuals not agreeing to follow-up
        mutate(excluded = (t == 0) & (is.na(login_id))) %>%
        # Individuals only responding at baseline
        group_by(pid) %>%
        mutate(max_wave = max(t, na.rm = TRUE)) %>%
        ungroup()
            
count(aw, max_wave)
count(aw, excluded)

# Resolve duplicate entries ---------------------------------------------------

n1 <- count(aw, t, name = "n1")

# Some participants have more than one entry per 'wave', and so, have more than
# 20 entres in total. To fix this, I'm (i) removing unfinished questionnaires
# and, (ii) for remaining duplicates, selecting a single observation per
# participant/time based on data completness.

# Remove "unfinished" questionnaires
table(aw$finished)
aw <- filter(aw, finished == "True")
n2 <- count(aw, t, name = "n2")

# Identify duplicates
aw %>%
    group_by(pid, t) %>%
    mutate(n = n()) %>%
    filter(n > 1) 

# Pick a single entry per person/survey period
aw <- distinct(aw, pid, t, .keep_all = TRUE)
    # NOTE: This is selecting the FIRST row in the case of duplicates.
    #       This might not be ideal -- e.g. we should select the most complete
    #       row instead. 
n3 <- count(aw, t, name = "n3")

# Check
aw %>%
    count(pid, t) %>%
    filter(n > 1)

# Produce summary table
dupes <- reduce(list(n1, n2, n3), full_join, by = "t")

# Remove individuals who only participant once --------------------------------

aw <- aw %>%
    count(pid, name = "n_entries") %>%
    full_join(aw, by = "pid") %>%
    filter(n_entries > 1)

# Convert 'survey period' into a measure of time ------------------------------

table(aw$t)

# The 't' variable is based on the filename of each dataset.
# t = 0 = '0-period.xlsx'
# t = 1 = '1-period.xlsx' and so on.
# The gaps between period 0, 1, 2 etc. is inconsistent. So, we want to create
# another time variable that maps "survey period" onto a "week number". I'll
# use the master tracker for this.

tracker <- read_xlsx(here("data", "raw", "survey", 
                          latest,
                          "master_tracker.xlsx"),
          sheet = "Schedule",
          range = "A1:D41") %>%
    mutate(gap = interval(`Date...1`, `Date...4`) / days(1),
           midpoint = `Date...1` + days(round(gap / 2))) %>%
    select(batch = `Batch 1`, midpoint)

# Get date for baseline (0-period); TODO: query, this isn't in tracker?
bl_date <- aw %>%
    filter(t == 0) %>%
    summarise(midpoint = min(start_date) + (interval(min(start_date),
                                                      max(start_date)) / days(1))) %>%
    mutate(batch = 0)

tracker <- bind_rows(bl_date, tracker) %>%
    mutate(delta = interval(min(midpoint), midpoint) / weeks(1),
           # Get approx. delta, because decimal timing is awkward
           dap = round(delta)) 

aw <- left_join(aw, tracker, by = c("t" = "batch"))

count(aw, dap) %>% print(n = 100)

###############################################################################
####                                                                      #####
####                  Prepare required BASELINE measures                  #####
####                                                                      #####
###############################################################################

bl <- filter(aw, dap == 0)

# Role, age -------------------------------------------------------------------

bl$is_staff <- str_detect(bl$role, "staff")
bl$age <- as.numeric(bl$age)

# Gender ----------------------------------------------------------------------

# As in baseline paper, we're randomly assigning "Other" to either "Female" or
# "Male" based on sample proportions.

p_female <- prop.table(table(bl$gender))[1]

bl$female <- bl$gender == "Female"
to_replace <- bl$gender %in% c("Other", "Prefer not to say")
bl$female[to_replace] <- sample(c(TRUE, FALSE),             # TRUE = female
                                size = sum(to_replace),     # FALSE = male
                                prob = c(p_female, 1 - p_female),
                                replace = TRUE)

# Ethnicity -------------------------------------------------------------------

bl <- bl %>%
    mutate(ethnic_group = case_when(str_detect(ethnicity, "Mixed") ~ "Mixed",
                                    str_detect(ethnicity, "White") ~ "White",
                                    str_detect(ethnicity, "Black") ~ "Black",
                                    str_detect(ethnicity, "Asian") ~ "Asian",
                                    str_detect(ethnicity, "Other") ~ "Other",
                                    TRUE ~ NA_character_),
           ethnic_f = factor(ethnic_group,
                              levels = c("White", "Black", "Asian",
                                         "Mixed", "Other")))


# Other covariates ------------------------------------------------------------

bl <- bl %>%
    mutate(child_liv = replace_na(children_cohabit, 0),
           numchild = factor(case_when(child_liv %in% 0:2 ~ child_liv,
                                       child_liv >= 3 ~ 3,
                                       is.na(child_liv) ~ 0),
                             levels = 0:3,
                             labels = c("0", "1", "2", "3+")),
           child_yng = replace_na(children_age, 0),
           child6 = (child_liv > 0) & (child_yng <= 6),
           othercare = replace_na(dependents, "Missing"),
           highrisk = isolation_reason == "I have an existing medical condition or I am categorised as high risk",
           shield_isol = case_when(isolation_status %in% c("Currently shielding",
                                                           "Currently isolating") ~ TRUE,
                                   is.na(isolation_status) ~ NA,
                                   TRUE ~ FALSE),
           chronic_any = chronic_any == "Yes",
           probdef = covid_suspect_self %in% c("Probably", "Definitely"),
           kwself = case_when(keyworker_self == "None of these" ~ "No",
                              is.na(keyworker_self) ~ "Missing",
                              TRUE ~ "Yes"),
           kwself_b = kwself == "Yes",
           # NOTE: kwself_b codes both "No" and "Missing" as FALSE. This isn't ideal.
           livalon = str_detect(living_current, "Alone"),
           renting = case_when(str_detect(accom, "[Rr]ented") ~ TRUE,
                               accom == "Missing" ~ NA,
                               TRUE ~ FALSE),
           accom = recode_accom(replace_na(accom, "Missing")),
           relat = recode_relat(replace_na(relat, "Missing")))

###############################################################################
####                                                                      #####
####                       Prepare repeated measures                      #####
####                                                                      #####
###############################################################################

# PHQ-9 and GAD-7 -------------------------------------------------------------

aw <- aw %>%
    mutate(across(matches("[gad|phq]_item_[1-9]"),
                  ~ case_when(.x == "Not at all" ~ 0,
                              .x == "Several days" ~ 1,
                              .x == "More than half the days" ~ 2,
                              .x == "Nearly every day" ~ 3))) %>%
    rowwise() %>%
    mutate(# Person-mean imputation for PHQ-9 ---------------------------------
           # 0 missing items ==> calculate sum score.
           # 1 or 2 missing items ==> person mean impute score
           # >2 missing items ==> set as missing
           phq_sum   = sum(c_across(phq_item_1:phq_item_9)),
           phq_nmiss = sum(is.na(c_across(phq_item_1:phq_item_9))),
           phq_pm    = sum(c_across(phq_item_1:phq_item_9), na.rm = TRUE),
           phq_pm    = (phq_pm * 9) / (9 - phq_nmiss),
           phq_total = case_when(phq_nmiss > 2 ~ NA_real_,
                                 phq_nmiss %in% 1:2 ~ phq_pm,
                                 phq_nmiss == 0 ~ phq_sum),
           # Person-mean imputation for GAD-7 ---------------------------------
           # 0 missing items ==> calculate sum score.
           # 1 missing item ==> person mean impute score
           # >1 missing items ==> set as missing.
           gad_sum   = sum(c_across(gad_item_1:gad_item_7)),
           gad_nmiss = sum(is.na(c_across(gad_item_1:gad_item_7))),
           gad_pm    = sum(c_across(gad_item_1:gad_item_7), na.rm = TRUE),
           gad_pm    = (gad_pm * 7) / (7 - gad_nmiss),
           gad_total = case_when(gad_nmiss > 1 ~ NA_real_,
                                 gad_nmiss == 1 ~ gad_pm,
                                 gad_nmiss == 0 ~ gad_sum)) 


# Identify '2 monthly' questionnaires -----------------------------------------

periods <- read_xlsx(here("data", "raw", "survey", 
                          latest, 
                          "master_tracker.xlsx"),
                     sheet = "Total Counts",
                     range = "A20:D60") %>%
    clean_names()

aw <- periods %>%
    bind_rows(data.frame(period = 0, type = "2-Month")) %>%
    mutate(two_month = type == "2-Month") %>%
    select(two_month, t = period) %>%
    right_join(aw, by = "t")


# Self-reported COVID-19 status -----------------------------------------------

aw$probdef <- aw$covid_suspect_self %in% c("Probably", "Definitely")

# Date of first/second vaccination --------------------------------------------

vaccine_dates <- aw %>%
    # Remove people who are missing 'vaccination year' for both doses
    filter(!(is.na(dose1_year) & is.na(dose2_year))) %>%
    # Keep only 'year' values with 4 characters; sets "121" to missing.
    mutate(across(c(dose1_year, dose2_year), ~ if_else(nchar(.x) == 4, .x, NA_character_))) %>%
    group_by(pid) %>%
    arrange(pid, dap) %>%
    # Select the first non-missing date per person (because these are duplicated)
    summarise(across(starts_with("dose"), ~ first(na.omit(.x)))) %>%
    # Remove people missing a first vaccination date,
    # or whose vaccination date is before 2020 (e.g. "1921").
    drop_na(dose1_year) %>%
    filter(dose1_year > 2019) %>%
    mutate(# If ONLY the day is missing, replace with 15th of the month
           dose1_day = if_else(!is.na(dose1_month) & !is.na(dose1_year) & is.na(dose1_day), "15", dose1_day),
           dose2_day = if_else(!is.na(dose2_month) & !is.na(dose2_year) & is.na(dose2_day), "15", dose2_day),
           # Generate dates
           dd1 = ymd(paste(dose1_year, dose1_month, dose1_day)),
           dd2 = ymd(paste(dose2_year, dose2_month, dose2_day), 
                     quiet = TRUE)) %>%
    select(pid, dd1, dd2)


# At each survey period, had the participant and their first/second vaccine? --

aw <- aw %>%
    left_join(vaccine_dates, by = "pid") %>%
    mutate(had_v1 = dd1 < midpoint,
           had_v2 = dd2 < midpoint,
           across(c(had_v1, had_v2), replace_na, FALSE)) 


# NOTE: this assumes that if people don't provide a vaccination date, they
# haven't been vaccinated. There are about 10 people who say "Yes" they have
# been vaccinated but don't provide a date:

aw %>%
    filter(str_detect(had_vaccine, "^Yes")) %>%
    select(pid, dap, had_vaccine, starts_with("dose")) %>%
    group_by(pid) %>%
    summarise(check = all(is.na(dose1_month))) %>%
    count(check)

# COVID stressors -------------------------------------------------------------

categories <- flatten_chr(str_split(aw$stress_question, ",")) %>% discard(~ is.na(.x)) %>% str_squish() %>% unique() 
print(categories)

q1 <- "Unable to pay bills|sufficient food|Lost your job|Evicted|food bank|lost their job"
q2 <- "lost somebody close to you|in hospital"
q3 <- "delay major life plans"
q4 <- "difficulties accessing required medication"

stressors <- aw %>%
    select(pid, t, stress_question) %>% 
    complete(crossing(pid, t)) %>%
    mutate(s_material = str_detect(stress_question, q1),
           s_person = str_detect(stress_question, q2),
           s_plans = str_detect(stress_question, q3),
           s_medi = str_detect(stress_question, q4)) %>%
    group_by(pid) %>%
    arrange(pid, t) %>%
    mutate(across(starts_with("s_"), replace_na, FALSE),
           across(starts_with("s_"), cumsum, .names = "{col}_sum"),
           across(ends_with("_sum"), lead, 3, .names = "{col}_lag", default = FALSE),
           across(ends_with("_lag"), cummax))

aw <- stressors %>%
    select(pid, t,
           s_plans = s_plans_sum_lag,
           s_person = s_person_sum_lag,
           s_material = s_material_sum_lag,
           s_medi = s_medi_sum_lag) %>%
    right_join(aw, by = c("pid", "t"))

###############################################################################
####                                                                      #####
####                 Merge baseline and repeated measures                 #####
####                                                                      #####
###############################################################################

repeated <- aw %>% select(pid,
                          t, dap,
                          two_month, midpoint,
                          phq_total, gad_total,
                          had_v1, had_v2,
                          starts_with("s_"))

baseline <- bl %>% select(pid,
                          midpoint_bl = midpoint,
                          excluded, max_wave,
                          age, female, is_staff, ethnic_f, child6, numchild,
                          highrisk, othercare, shield_isol, probdef, kwself01,
                          probdef, livalon, renting)

aw <- full_join(repeated, baseline, by = "pid")

# Select required measures ----------------------------------------------------
sel <- aw %>%
    select(pid,
           t,
           dap,
           two_month,
           is_staff,
           age,
           female,
           ethnic_f,
           midpoint,
           excluded, max_wave,
           starts_with("phq_item"),
           starts_with("gad_item"),
           phq = phq_total,
           gad = gad_total,
           child6,
           numchild,
           highrisk,
           othercare,
           shield_isol,
           probdef,
           kwself01,
           livalon,
           renting)

# Check analysis dataset is unique by "pid" and "iw"
sel %>%
    group_by(pid, dap) %>%
    mutate(n = n()) %>%
    filter(n > 1)

###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

save(sel, aw,
     file = here("data", "clean", "rep.Rdata"))

save(bl, file = here("data", "clean", "baseline.Rdata"))
