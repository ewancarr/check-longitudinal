# Title:        Data cleaning for KCL-CHECK longitudinal paper
# Author:       Ewan Carr
# Started:      2021-02-23

library(tidyverse)
library(lubridate)
library(here)
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

# Pick a single entry per person/time
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

###############################################################################
####                                                                      #####
####                                 DATES                                #####
####                                                                      #####
###############################################################################

if (noisy) {
    ggplot(aw,
           aes(x = start_date)) +
        geom_histogram(bins = 50) +
        facet_wrap(~ t, scales = "free_x")
}

# Check gaps between successive questionnaires --------------------------------

gaps <- aw %>%
    select(pid, pid_str, t, recorded_date) %>%
    arrange(pid, t) %>%
    group_by(pid) %>%
    mutate(gap = if_else(t == lag(t) + 1,
                         interval(lag(recorded_date), recorded_date) / days(1),
                         NA_real_),
           min_gap = min(gap, na.rm = TRUE))

table(gaps$gap < 7)

gaps %>%
    filter(!is.nan(min_gap)) %>%
    arrange(min_gap) %>%
    print(n = 200)

# Create 'week number' -------------------------------------------------------

aw <- aw %>%
    mutate(interview_week = round_date(recorded_date, unit = "week",
                                       week_start = 1),
           iw = as.numeric(as.factor(interview_week)))

table(aw$interview_week, aw$t)

###############################################################################
####                                                                      #####
####                        CLEAN REQUIRED MEASURES                       #####
####                                                                      #####
###############################################################################

aw$is_staff <- str_detect(aw$role, "staff")
aw$age <- as.numeric(aw$age)

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
    arrange(pid, iw) %>%
    fill(ethnic_group, ethnic_f, age, gender, role, is_staff,
         .direction = "down")

# Select required measures ----------------------------------------------------
sel <- aw %>%
    select(pid,
           t,
           iw,
           interview_week,
           is_staff,
           age,
           gender, 
           ethnic_f,
           start = start_date,
           end = end_date,
           excluded, max_wave,
           starts_with("phq_item"),
           starts_with("gad_item"),
           phq_total,
           gad_total)

save(sel, aw, dupes,
     file = here("data", "clean", "rep.Rdata"))

# Save baseline dataset -------------------------------------------------------

bl <- filter(aw, t == 0)
save(bl, file = here("data", "clean", "baseline.Rdata"))
