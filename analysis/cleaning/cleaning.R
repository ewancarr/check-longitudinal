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

tracker <- read_xlsx(here("data", "raw", "survey", "master_tracker.xlsx"),
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
           kwself01 = kwself == "Yes",
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

periods <- read_xlsx(here("data", "raw", "survey", "master_tracker.xlsx"),
                     sheet = "Total Counts",
                     range = "A20:D60") %>%
    clean_names()

aw <- periods %>%
    bind_rows(data.frame(period = 0, type = "2-Month")) %>%
    mutate(two_month = type == "2-Month") %>%
    select(two_month, t = period) %>%
    right_join(aw, by = "t")

###############################################################################
####                                                                      #####
####                 Merge baseline and repeated measures                 #####
####                                                                      #####
###############################################################################

repeated <- aw %>% select(pid,
                          t, dap, phq_total, gad_total, two_month,
                          midpoint)

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
####                          Add baseline weights                        #####
####                                                                      #####
###############################################################################

load(here("data", "clean", "weights.Rdata"), verbose = TRUE)

# Check: does everyone in the `sel` dataset have a baseline weight
table(unique(sel$pid) %in% weights$pid)
# 2 people are missing a baseline weight. 
# TODO: investigate/resolve.

sel <- sel %>%
    left_join(weights, by = "pid") %>%
    select(pid, t, dap, rw, everything())

bl <- left_join(bl, weights, by = "pid")

###############################################################################
####                                                                      #####
####                         Merge contextual data                        #####
####                                                                      #####
###############################################################################

# load(here("data", "raw", "contextual", "gov_d1.Rdata"), verbose = TRUE)

# d1 <- d1 %>%
#     mutate(interview_fortnight = round_date(date,
#                                             unit = weeks(2),
#                                             week_start = 1)) %>%
#     group_by(interview_fortnight) %>%
#     summarise(across(deaths28:new_cases, sum, na.rm = TRUE))

# sel <- sel %>%
#     left_join(d1, by = "interview_fortnight")


###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

save(sel, aw,
     file = here("data", "clean", "rep.Rdata"))

save(bl, file = here("data", "clean", "baseline.Rdata"))
