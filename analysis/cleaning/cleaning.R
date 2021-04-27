# Title:        Data cleaning for KCL-CHECK longitudinal paper
# Author:       Ewan Carr
# Started:      2021-02-23

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
noisy <- FALSE
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

# TODO: query this.

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
####                                 DATES                                #####
####                                                                      #####
###############################################################################

# if (noisy) {
#     ggplot(aw,
#            aes(x = start_date)) +
#         geom_histogram(bins = 50) +
#         facet_wrap(~ t, scales = "free_x")
# }

# # Check gaps between successive questionnaires --------------------------------

# gaps <- aw %>%
#     select(pid, pid_str, t, recorded_date) %>%
#     arrange(pid, t) %>%
#     group_by(pid) %>%
#     mutate(gap = if_else(t == lag(t) + 1,
#                          interval(lag(recorded_date), recorded_date) / days(1),
#                          NA_real_),
#            min_gap = min(gap, na.rm = TRUE))

# table(gaps$gap < 7)

# gaps %>%
#     filter(!is.nan(min_gap)) %>%
#     arrange(min_gap) %>%
#     print(n = 200)

# # Create 'week number' and 'fortnight number' --------------------------------

# aw <- aw %>%
#     mutate(interview_week = round_date(recorded_date,
#                                        unit = weeks(1),
#                                        week_start = 1),
#            interview_fortnight = round_date(recorded_date,
#                                             unit = weeks(2),
#                                             week_start = 1),
#            iw = as.numeric(as.factor(interview_week)),
#            ifn = as.numeric(as.factor(interview_fortnight)))

# # Remove duplicates by week

# # NOTE: We already did this, above, for 't'. But we need to do it again because
# # when translating from "survey period" to "interview week", some individuals 
# # could respond more than once per week, as shown below:

# aw %>%
#     group_by(pid, iw) %>%
#     mutate(n = n(),
#            t = t,
#            interview_week = interview_week,
#            recorded_date = recorded_date) %>%
#     select(pid, t, iw, interview_week, recorded_date, n) %>%
#     group_by(pid) %>%
#     filter(max(n) > 1) %>%
#     print(n = 1000)

# aw %>%
#     group_by(pid, ifn) %>%
#     mutate(n = n(),
#            t = t,
#            interview_week = interview_week,
#            recorded_date = recorded_date) %>%
#     select(pid, t, ifn, interview_fortnight, recorded_date, n) %>%
#     group_by(pid) %>%
#     filter(max(n) > 1) %>%
#     print(n = 100)

# aw <- aw %>%
#     distinct(pid, ifn, .keep_all = TRUE)


###############################################################################
####                                                                      #####
####                        CLEAN REQUIRED MEASURES                       #####
####                                                                      #####
###############################################################################

aw$is_staff <- str_detect(aw$role, "staff")
aw$age <- as.numeric(aw$age)

# Gender ----------------------------------------------------------------------

# As in baseline paper, we're randomly assigning "Other" to either "Female" or
# "Male" based on sample proportions.

p_female <- prop.table(table(aw$gender))[1]

aw$female <- aw$gender == "Female"
to_replace <- aw$gender %in% c("Other", "Prefer not to say")
aw$female[to_replace] <- sample(c(TRUE, FALSE),             # TRUE = female
                                size = sum(to_replace),     # FALSE = male
                                prob = c(p_female, 1 - p_female),
                                replace = TRUE)

# Ethnicity -------------------------------------------------------------------

aw <- aw %>%
    mutate(ethnic_group = case_when(str_detect(ethnicity, "Mixed") ~ "Mixed",
                                    str_detect(ethnicity, "White") ~ "White",
                                    str_detect(ethnicity, "Black") ~ "Black",
                                    str_detect(ethnicity, "Asian") ~ "Asian",
                                    str_detect(ethnicity, "Other") ~ "Other",
                                    TRUE ~ NA_character_),
           ethnic_f = factor(ethnic_group,
                              levels = c("White", "Black", "Asian",
                                         "Mixed", "Other")))

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


###############################################################################
####                                                                      #####
####                   Forward-fill some baseline values                  #####
####                                                                      #####
###############################################################################

aw <- aw %>%
    group_by(pid) %>%
    arrange(pid, dap) %>%
    fill(ethnic_group, ethnic_f, age, gender, female, role, is_staff,
         .direction = "down")

# Select required measures ----------------------------------------------------
sel <- aw %>%
    select(pid,
           t,
           dap,
           is_staff,
           age,
           gender, female,
           ethnic_f,
           start = start_date,
           end = end_date,
           excluded, max_wave,
           starts_with("phq_item"),
           starts_with("gad_item"),
           phq = phq_total,
           gad = gad_total)

# Check analysis dataset is unique by "pid" and "iw"
sel %>%
    group_by(pid, dap) %>%
    mutate(n = n()) %>%
    filter(n > 1)


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

# Save longitudinal dataset ---------------------------------------------------

save(sel, aw,
     file = here("data", "clean", "rep.Rdata"))

# Save baseline dataset -------------------------------------------------------

bl <- filter(aw, t == 0)
save(bl, file = here("data", "clean", "baseline.Rdata"))
