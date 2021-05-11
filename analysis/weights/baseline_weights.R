# Title:        Baseline weights for KCL CHECK
# Author:       Ewan Carr
# Started:      2021-03-03

library(tidyverse)
library(here)
library(janitor)
library(survey)
library(assertthat)
library(readxl)
library(VIM)

hr <- readxl::read_xlsx(here("data", "raw", "admin", "staff",
                             "2411_AllStaffD&I_nopassword.xlsx"),
                        sheet = 3) %>%
    clean_names()

load(here("data", "clean", "baseline.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                         DERIVE STAFF WEIGHTS                         #####
####                                                                      #####
###############################################################################

# Extract required measures from survey ---------------------------------------

survey <- bl %>%
    filter(is_staff) %>%
    select(pid,
           age,
           gender,
           ethnicity,
           staff_pay,
           staff_role,
           staff_ft,
           staff_contract)

# Impute missing values from survey/HR ----------------------------------------

# First, set a few categories as missing
survey$staff_pay[survey$staff_pay == "Don't know or prefer not to say"] <- NA
survey$ethnicity[survey$ethnicity == "Missing"] <- NA

# Then impute
survey <- as_tibble(kNN(survey, imp_var = FALSE))
hr <- as_tibble(kNN(hr, imp_var = FALSE))

# Import lookup tables --------------------------------------------------------

lookup_ethnicity <- read_csv(here("analysis", "weights", "ethnicity.csv"))
lookup_grade <- read_csv(here("analysis", "weights", "grade.csv"))

# Can Arab be derived as Other to match the grouping we've used in the survey
# (based on ONS?)

# =========================================================================== #
# ====================== Harmonise survey/HR datasets ======================= #
# =========================================================================== #

count_miss <- function(dataset, measure) {
    measure <- enquo(measure)
    dataset %>%
        summarise(n_miss = naniar::n_miss(!!measure)) %>%
        pluck(1) %>% return()
}

check_match <- function(measure) {
    measure = enquo(measure)
    # Check missing
    if (count_miss(hr, !!measure) != 0) {
        stop("Missing in HR data")
    }
    if (count_miss(survey, !!measure) != 0) {
        stop("Missing in survey data")
    }
    # Check all categories present
    cat_survey <- survey %>% select(!!measure) %>% distinct() %>% pluck(1)
    cat_hr <- hr %>% select(!!measure) %>% distinct() %>% pluck(1)
    assert_that(all(cat_survey %in% cat_hr),
                msg = "Categories don't match")
}

survey %>% summarise(n_miss = naniar::n_miss(gender))

# 1: Age ----------------------------------------------------------------------
survey <- survey %>%
    mutate(age = as.integer(age),
           age_group = case_when(age %in% 16:25 ~ "16 - 25",
                                 age %in% 26:35 ~ "26 - 35",
                                 age %in% 36:45 ~ "36 - 45",
                                 age %in% 46:55 ~ "46 - 55",
                                 age %in% 56:65 ~ "56 - 65",
                                 age >= 66 ~ ">=66",
                                 TRUE ~ NA_character_))

hr <- rename(hr,
             age_group = age_banding)
check_match(age_group)

# 2: Gender -------------------------------------------------------------------
# NOTE: I'm replacing 'other' gender with a random value, using survey
# proportions. This is obviously fairly problematic, and only done for
# weighting purposes. Investigate better approaches later.

prop_male <- mean(survey$gender == "Male")
prop_female <- mean(survey$gender == "Female")

nonbin <- survey$gender %in% c("Other", "Prefer not to say")
replacement <- sample(c("Male", "Female"),
                      size = sum(nonbin),
                      replace = TRUE,
                      prob = c(prop_male, prop_female))
survey$gender[nonbin] <- replacement
check_match(gender)

# 3: Staff role ---------------------------------------------------------------
survey <- survey %>%
    mutate(staff_role = if_else(staff_role %in% c("Clerical",
                                                  "Facilities",
                                                  "Management",
                                                  "Specialists and professionals",
                                                  "Technical"),
                                "Professional Services",
                                staff_role))
hr <- hr %>%
    mutate(staff_role = if_else(category_description %in% c("Clinical Academic",
                                                            "Clinical Research",
                                                            "Clinical Teaching"),
                                "Clinical",
                          category_description)) %>%
    select(-category_description)
check_match(staff_role)

# 4: Hours --------------------------------------------------------------------
survey <- survey %>%
    rename(hours = staff_ft) %>%
    mutate(hours = toupper(hours))
hr <- rename(hr, hours = employee_sub_status_desc)
check_match(hours)

# 5: Ethnicity ----------------------------------------------------------------

survey <- survey %>%
    mutate(ethnic_group = case_when(
        str_detect(ethnicity, "Mixed") ~ "Mixed",
        str_detect(ethnicity, "White") ~ "White",
        str_detect(ethnicity, "Black") ~ "Black",
        str_detect(ethnicity, "Asian") ~ "Asian",
        str_detect(ethnicity, "Other") ~ "Other")) %>%
    select(-ethnicity)

hr <- hr %>%
    rename(eth = ethnic_origin_description) %>%
    mutate(ethnic_group =
      case_when(str_detect(eth, "Mixed") ~ "Mixed",
                str_detect(eth, "Gypsy|White") ~ "White",
                str_detect(eth, "Black") ~ "Black",
                str_detect(eth, "Chinese|Asian") ~ "Asian",
                str_detect(eth, "Arab") ~ "Other",
                str_detect(eth, "Known|Prefer|Latinx|Other") ~ "Other")) %>%
    select(-eth)
check_match(ethnic_group)

# 6: Contract -----------------------------------------------------------------

survey <- survey %>%
    rename(contract = staff_contract) %>%
    mutate(contract = case_when(
      contract %in% c("Fixed term",
                      "Casual") ~ "Fixed term/casual",
     contract == "Open ended/permanent" ~ "Permanent"))

hr <- hr %>%
    rename(contract = employee_status_desc) %>%
    mutate(contract = case_when(
      contract == "EMPLOYEE - FIXED TERM" ~ "Fixed term/casual",
      contract == "EMPLOYEE - INDEFINITE" ~ "Permanent",
      TRUE ~ NA_character_))
check_match(contract)


# =========================================================================== #
# ============================= Generate weights ============================ #
# =========================================================================== #

freq <- function(v) {
    v <- enquo(v)
    count(hr, !!v) %>%
        rename(Freq = n)
}

design <- svydesign(id = ~ 1, probs = 1, data = survey)

# Age, gender, ethnicity
raking <- rake(design,
               sample.margins = list(~age_group, ~gender, ~ethnic_group),
               population.margins = list(age_group = freq(age_group),
                                         gender = freq(gender),
                                         ethnic_group = freq(ethnic_group)))

# Trim ------------------------------------------------------------------------

staff_weights <- survey %>%
    mutate(rw = weights(raking),
           limit = median(rw) + (5 * IQR(rw)),
           rw = if_else(rw > limit, limit, rw))

###############################################################################
####                                                                      #####
####                      DERIVE PGR STUDENT WEIGHTS                      #####
####                                                                      #####
###############################################################################

rm(survey, raking, design)

# Load survey data ------------------------------------------------------------

pgr <- filter(bl, !is_staff)

# Impute missing data
pgr <- as_tibble(kNN(pgr,
                     variable = "ethnicity",
                     imp_var = FALSE))

# Load data on PGR population profile -----------------------------------------

# Get age and sex profile from HESA data
hesa <- read_xlsx(here("data",
                       "raw",
                       "admin",
                       "pgr",
                       "2020-06-09 Updated numbers from HESA",
                       "Student PGR HESA Standard Registration 1819.xlsx")) %>%
    select(age = Age,
           female = Female,
           male = Male)

pop_age <- hesa %>%
    group_by(age) %>%
    summarise(across(female:male, sum)) %>%
    mutate(total = female + male)

pop_gender <- data.frame(gender = c("Female",
                                    "Male"),
                         Freq = c(sum(hesa$female),
                                  sum(hesa$male)))

# Get ethnicity from Nigel's data (as of June 2020, we're using information
# from Nigel Eady[1], see email dated 24th June). 

nigel <- read_csv(here("data",
                       "raw",
                       "admin",
                       "pgr",
                       "2020-07-16 Data from Nigel",
                       "nigel.csv"))
pop_ethnic <- nigel %>%
    mutate(Freq = round(Total * 2460)) %>%
    select(ethnic_group = Ethnic,
           Freq)

# Prepare/harmonise population summaries --------------------------------------

# Ethnicity
pgr <- pgr %>%
    mutate(ethnic_group = case_when(str_detect(ethnicity, "Mixed") ~ "Mixed",
                                    str_detect(ethnicity, "White") ~ "White",
                                    str_detect(ethnicity, "Black") ~ "Black",
                                    str_detect(ethnicity, "Asian") ~ "Asian",
                                    str_detect(ethnicity, "Other") ~ "Other",
                                    TRUE ~ "Unknown")) %>%
    select(-ethnicity)

pop_ethnic$ethnic_group[pop_ethnic$ethnic_group == "Undisclosed"] <- "Unknown"
pop_ethnic <- filter(pop_ethnic, ethnic_group != "Unknown")

# Age
pgr <- pgr %>%
    mutate(age_group = case_when(age <= 22 ~ 22,
                                 age >= 43 ~ 43,
                                 TRUE ~ age),
           age_group = as.integer(age_group))

pop_age <- pop_age %>%
    mutate(age_group = parse_number(age)) %>%
    select(age_group,
           Freq = total)


# Gender
prop_male <- mean(pgr$gender == "Male")
prop_female <- mean(pgr$gender == "Female")
pgr <- pgr %>%
    mutate(gender = if_else(gender %in% c("Other",
                                          "Gender:",
                                          "Prefer not to say"),
                            sample(c("Male", "Female"),
                                   size = 1,
                                   prob = c(prop_male, prop_female)),
                            gender))

pop_gender

# Create raking weights -------------------------------------------------------

design <- svydesign(id = ~ 1, prob = 1, data = pgr)
raking <- rake(design,
               sample.margins = list(~age_group, ~gender, ~ ethnic_group),
               population.margins = list(age_group = pop_age,
                                         gender = pop_gender,
                                         ethnic_group = pop_ethnic))

# Trim based on median + 5 x IQR
w <- weights(raking)
limit <- median(w) + 5 * IQR(w)
w <- ifelse(w > limit, limit, w)

pgr$rw <- w
student_weights <- pgr

###############################################################################
####                                                                      #####
####                    APPEND STAFF/PGR WEIGHTS, SAVE                    #####
####                                                                      #####
###############################################################################

weights <- bind_rows(select(staff_weights, pid, rw),
                     select(student_weights, pid, rw))
save(weights, file = here("data", "clean", "weights.Rdata"))
