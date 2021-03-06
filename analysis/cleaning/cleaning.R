# Title:        Data cleaning for KCL-CHECK longitudinal paper
# Author:       Ewan Carr
# Started:      2021-02-23

renv::load()
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

# Remove individuals who only participant once --------------------------------

aw <- aw %>%
    count(pid, name = "n_entries") %>%
    full_join(aw, by = "pid") %>%
    filter(n_entries > 1)

# Convert 'survey period' into a measure of time ------------------------------

table(aw$t)

# The 't' variable is based on the filename of each dataset.
# t => 0 => '0-period.xlsx'
# t => 1 => '1-period.xlsx' and so on.
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
    summarise(midpoint = min(start_date) +
                         (interval(min(start_date),
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

bl <- filter(aw, dap == 0) %>%
    mutate(two_month = TRUE)

# Age, gender ------------------------------------------------------------------

bl$age <- as.numeric(bl$age)

# As in baseline paper, we're randomly assigning "Other" to either "Female" or
# "Male" based on sample proportions.

p_female <- prop.table(table(bl$gender))[1]

bl$female <- bl$gender == "Female"
to_replace <- bl$gender %in% c("Other", "Prefer not to say")
bl$female[to_replace] <- sample(c(TRUE, FALSE),             # TRUE: female
                                size = sum(to_replace),     # FALSE: male
                                prob = c(p_female, 1 - p_female),
                                replace = TRUE)

# Role ------------------------------------------------------------------------

bl$is_staff <- str_detect(bl$role, "staff")

bl <- bl %>%
    mutate(role_cat = factor(case_when(
      staff_role %in% c("Academic",
                        "Specialists and professionals",
                        "Management") ~ "Academic, specialist and management",
      staff_role %in% c("Research",
                        "Clerical",
                        "Technical") ~ "Research, clerical and technical",
      staff_role %in% c("Teaching",
                        "Facilities",
                        "Clinical") ~ "Teaching, facilities and clinical",
      !is_staff ~ "PGR student",
      TRUE ~ "Missing"),
      levels = c("Academic, specialist and management",
                 "Research, clerical and technical",
                 "Teaching, facilities and clinical",
                 "PGR student",
                 "Missing")))

count(bl, role_cat)

# Ethnicity -------------------------------------------------------------------

bl <- recode_ethnicity(bl)

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
           shield_only = isolation_status == "Currently shielding",
           chronic_any = chronic_any == "Yes",
           probdef = covid_suspect_self %in% c("Probably", "Definitely"),
           kwself = case_when(keyworker_self == "None of these" ~ "No",
                              is.na(keyworker_self) ~ "Missing",
                              TRUE ~ "Yes"),
           kwself_b = kwself == "Yes",
           # NOTE: kwself_b codes both "No" and "Missing" as FALSE.
           # This isn't ideal.
           livalon = str_detect(living_current, "Alone"),
           renting = case_when(str_detect(accom, "[Rr]ented") ~ TRUE,
                               accom == "Missing" ~ NA,
                               TRUE ~ FALSE),
           accom = recode_accom(accom),
           relat = recode_relat(relat),
           prev_depress = str_detect(diagnoses, "Depression"),
           prev_gad = str_detect(bl$diagnoses, "anxiety|Panic attack|PTSD"))

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
                     range = "A21:D61") %>%
    clean_names()

aw <- periods %>%
    bind_rows(data.frame(period = 0, type = "2-Month")) %>%
    mutate(two_month = type == "2-Month") %>%
    select(two_month, t = period) %>%
    right_join(aw, by = "t")


# Self-reported COVID-19 status -----------------------------------------------

aw$probdef <- aw$covid_suspect_self %in% c("Probably", "Definitely")

# Date of first/second vaccination --------------------------------------------

get_year <- function(month) {
    case_when(month %in% c("January",
                           "February",
                           "March",
                           "April",
                           "May",
                           "June",
                           "July",
                           "August") ~ 2021,
              month %in% c("September",
                           "October",
                           "November",
                           "December") ~ 2020)
}

max_na <- function(x) ifelse(!all(is.na(x)), max(x, na.rm = TRUE), NA)

vaccine_dates <- aw %>%
    # Remove people who have not been vaccinated
    group_by(pid) %>%
    arrange(pid, dap) %>%
    mutate(which_dose = case_when(dose_n == "One" ~ 1,
                                  dose_n == "Two" ~ 2,
                                  TRUE ~ NA_real_),
           max_dose = max_na(which_dose)) %>%
    drop_na(max_dose) %>%
    mutate(# Derive vaccination year based on month
           dose1_yearfix = if_else(max_dose >= 1,
                                   get_year(dose1_month),
                                   NA_real_),
           dose2_yearfix = if_else(max_dose >= 2,
                                   get_year(dose2_month),
                                   NA_real_),
           # If only the day is missing, replace with 15th of the month
           dose1_day = if_else(!is.na(dose1_month) &
                               !is.na(dose1_yearfix) &
                               is.na(dose1_day), "15", dose1_day),
           dose2_day = if_else(!is.na(dose2_month) &
                               !is.na(dose2_yearfix) &
                               is.na(dose2_day), "15", dose2_day),
           # Derive dates
           dd1 = ymd(paste(dose1_yearfix, dose1_month, dose1_day),
                     quiet = TRUE),
           dd2 = ymd(paste(dose2_yearfix, dose2_month, dose2_day),
                     quiet = TRUE)) %>%
    summarise(across(c(dd1, dd2), ~ first(na.omit(.x)))) %>%
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

categories <- flatten_chr(str_split(aw$stress_question, ",")) %>%
  discard(~ is.na(.x)) %>%
  str_squish() %>%
  unique()
print(categories)

q1 <- paste(c("Unable to pay bills",
              "sufficient food",
              "Lost your job",
              "Evicted",
              "food bank",
              "lost their job"), collapse = "|")
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
           across(ends_with("_sum"), lead, 3,
                  .names = "{col}_lag",
                  default = FALSE),
           across(ends_with("_lag"), cummax))

aw <- stressors %>%
    select(pid, t,
           s_plans = s_plans_sum_lag,
           s_person = s_person_sum_lag,
           s_material = s_material_sum_lag,
           s_medi = s_medi_sum_lag) %>%
    right_join(aw, by = c("pid", "t"))


# Furlough --------------------------------------------------------------------

fill_lead <- function(d, var) {
    d %>%
        mutate(lead3 = lead({{var}}, 3),
               lead2 = lead({{var}}, 2),
               lead1 = lead({{var}}, 1),
               {{var}} := coalesce({{var}}, lead1, lead2, lead3)) %>%
        select(-lead1, -lead2, -lead3)
}

furlough <- aw %>%
    ungroup() %>%
    select(pid, t, on_furlough) %>%
    arrange(pid, t) %>%
    complete(crossing(pid, t)) %>%
    mutate(current_furlough = case_when(
                on_furlough %in% c("Yes, I am currently on furlough",
                                   "Yes, I will soon be on furlough") ~ TRUE,
                is.na(on_furlough) ~ NA,
                TRUE ~ FALSE),
           prev_furlough = case_when(
                str_detect(on_furlough, "have since returned") ~ TRUE,
                is.na(on_furlough) ~ NA,
                TRUE ~ FALSE)) %>%
    group_by(pid) %>%
    fill_lead(current_furlough) %>%
    mutate(across(c(current_furlough, prev_furlough), replace_na, FALSE),
           ever_furlough = as.logical(cummax(current_furlough |
                                             prev_furlough))) %>%
    select(pid, t,
           fur_cu = current_furlough,
           fur_ev = ever_furlough)

aw <- left_join(aw, furlough, by = c("pid", "t"))

# NOTE: we're interpreting "NA" values in the furlough question as "not on
# furlough". This is problematic.

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
                          fur_cu, fur_ev,
                          probdef,
                          starts_with("s_"))

baseline <- bl %>% select(pid,
                          t,
                          two_month,
                          midpoint_bl = midpoint,
                          excluded, max_wave,
                          age, female, is_staff, ethnic_f, child6, numchild,
                          prev_depress, prev_gad,
                          highrisk, othercare, shield_isol, shield_only,
                          kwself_b,
                          livalon, renting,
                          role_cat, chronic_any, relat)

sel <- full_join(repeated, baseline, by = c("pid", "two_month", "t"))

# Check analysis dataset is unique by "pid" and "dap"
dupes <- sel %>%
    group_by(pid, dap) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    nrow()
stopifnot(dupes == 0)

###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

save(sel, aw, bl,
     file = here("data", "clean", "check.Rdata"))
