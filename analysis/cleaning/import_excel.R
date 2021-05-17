# Title:        Import Excel files from KCL CHECK
# Author:       Ewan Carr
# Started:      2021-02-02

renv::load()
library(tidyverse)
library(here)
library(readxl)
library(here)
library(fs)
library(labelled)
library(janitor)
source(here("analysis", "functions", "generic.R"))
latest <- "2021-05-12"

excel_files <- dir_ls(here("data", "raw", "survey", latest, "raw-do-not-edit"))

# Check for duplicated variable names in each file ----------------------------

identify_duplicates <- function(path) {
    cols <- names(read_xlsx(path, n_max = 0, .name_repair = "minimal"))
    return(cols[duplicated(cols)])
}

dupes <- map(excel_files, identify_duplicates)
names(dupes) <- path_ext_remove(path_file(names(dupes)))

walk2(dupes, names(dupes), 
      ~ cat("\n", .y, rep("_", 25 - nchar(.y)), ": ", paste(.x, collapse = ", "), sep = ""))

# VERY IMPORTANT: If you're using any of the duplicated variables, these need
#                 to be de-duplicated before importing.

# Import Excel files ----------------------------------------------------------

latest <- "2021-05-12"

import_raw <- function(path, ctypes) {
    # Import once, to get variable names only
    cname <- read_xlsx(path,
                       n_max = 1,
                       col_types = "text",
                       guess_max = 0) %>%
        names()
    # Import again to get data
    raw <- read_xlsx(path,
                     col_names = cname,
                     skip = 2,
                     guess_max = 10000)
    return(raw)
}

ar <- dir_ls(here("data", "raw", "survey",
                  latest, "raw-do-not-edit")) %>%
    map(import_raw)
names(ar) <- names(ar) %>% path_file() %>% path_ext_remove()

# Prepare baseline data -------------------------------------------------------

bl <- bind_rows(ar[["0-baseline-reminder"]],
                ar[["0-baseline_final"]]) %>%
    mutate(tid = "0-period") 

# Remove "Code 3" participants
code3 <- read_xlsx(here("data", "raw", "survey", latest, 
                        "master_tracker.xlsx"),
          sheet = "Participants") %>%
    select(`Dropout Code`,
           `ResponseId` = `Baseline Response ID`) %>%
    filter(`Dropout Code` == 3)

bl <- filter(bl, !(ResponseId %in% code3$ResponseId))

# Prepare follow-up waves -----------------------------------------------------

fu <- list_modify(ar, 
                  "0-baseline-reminder" = NULL,
                  "0-baseline_final" = NULL) 


# The "Q___" variables are complicated because (a) they're not collected at all
# waves and (b) the same Q-number can refer to completed different variables at
# different waves.  Therefore, the list below defines which Q-variables to
# extract at specifc waves.

# First, get a list of which Q-variables are measured at each wave

map(fu, names) %>%
    map(str_subset, "^Q") %>%
    map_dfr(~ paste(sort(.x), collapse = " "), .id = "period") %>%
    gather(period, q_vars) %>%
    mutate(period = parse_number(period)) %>%
    arrange(period) %>% print(n = 30) 
    
#                   variable  new label      survey periods
q_vars <- list(list("Q265",   "had_vaccine", c(16, 20, 24)),
               list("Q267_1", "dose1_month", c(20, 24)),
               list("Q267_2", "dose1_day",   c(20, 24)),
               list("Q267_3", "dose1_year",  c(20, 24)),
               list("Q268_1", "dose2_month", c(20, 24)),
               list("Q268_2", "dose2_day",   c(20, 24)),
               list("Q268_3", "dose2_year",  c(20, 24)),
               list("Q266",   "dose_n",      c(16, 20, 24)),
               list("Q216",   "on_furlough", c(4, 8, 12, 16, 20, 24)))

fu <- map2(fu, names(fu),
     function(d, w) {
         # Extract/rename required Q-variables
         period <- parse_number(w)
         for (k in q_vars) {
             required_periods <- k[[3]]
             old_name <- k[[1]]
             new_name <- k[[2]]
             if (period %in% required_periods) {
                 d[new_name] <- d[old_name]
             }
         }
         # Remove all other Q____ variables
         d <- select(d, -matches("^Q[0-9_\\.]+$"))
         d$tid <- w
         return(d) 
     }
) 

fu <- reduce(fu, bind_rows)

# 3. Add baseline, clean variable names
aw <- bind_rows(bl, fu)

# Rename variables ------------------------------------------------------------

aw <- aw %>%
    swap_names() %>%                    # See function.R for details
    clean_names() %>%                   # From the 'janitor' package
    mutate(t = parse_number(tid))

# Derive consistent longitudinal ID variable and merge ------------------------

# NOTE:
# 1. There are two IDs. At baseline we have "login_id"; at follow-ups we have
#    "external_reference".
# 2. Only people who agreed to participate in follow-up have "login_id" at
#    baseline.
# 3. To construct a consistent longitudinal ID, therefore, we 
#     i. Use "login_id" and/or "external_reference" where these are available;
#    ii. For participants at baseline without (i), we use "response_id".

aw <- aw %>%
    mutate(pid_str = case_when(
         # If both "login_id" and "external_reference" are missing,
         # use "response_id"
            (is.na(extref) & is.na(login_id)) ~ response_id,
         # If "login_id" is missing, use "external_reference")
            is.na(login_id) ~  extref,
         # Otherwise, use "login_id":
           TRUE ~ login_id
         )
    )

table(is.na(aw$pid_str))

# Convert string ID to integer

aw <- aw %>%
    mutate(pid = as.integer(factor(pid_str,
                                   levels = unique(pid_str)))) %>%
    arrange(pid, t) %>%
    select(pid, t, pid_str, login_id, extref, everything())

# Create variable identifying 2-monthly questionnaires ------------------------

qtypes <- ar %>%
    map_dfr(ncol, .id = "period") %>%
    gather(period, ncol) %>%
    mutate(qlong = ncol > 100,
           tid = if_else(str_detect(period, "baseline"), 
                         "0-period", period)) %>%
    select(tid, qlong) %>%
    distinct()

aw <- aw %>%
    left_join(qtypes, by = "tid")

###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

save(aw, file = here("data", "clean", "aw.Rdata"))
