# Title:        Import Excel files from KCL CHECK
# Author:       Ewan Carr
# Started:      2021-02-02

library(tidyverse)
library(here)
library(readxl)
library(here)
library(fs)
library(labelled)
library(janitor)
# source(here("analysis", "cleaning", "labels.R"))
source(here("analysis", "functions.R"))

# Import Excel files ----------------------------------------------------------

latest <- "2021-03-30"

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
code3 <- read_xlsx(here("data", "raw", "survey", "master_tracker.xlsx"),
          sheet = "Participants") %>%
    select(`Dropout Code`,
           `ResponseId` = `Baseline Response ID`) %>%
    filter(`Dropout Code` == 3)

bl <- filter(bl, !(ResponseId %in% code3$ResponseId))

# Prepare follow-up waves -----------------------------------------------------

fu <- list_modify(ar, 
                  "0-baseline-reminder" = NULL,
                  "0-baseline_final" = NULL) 

fu <- map2(fu, names(fu),
     function(d, w) {
         # d <- mutate(d, across(one_of(c("Q214", "Q218", "Q216", "Q212", "Q246", 
         #                                "Q242", "Q254", "Q256")),
         #                      as.character))
         # NOTE: I'm dropping all Q____ variables because they differ across
         # waves and are therefore complicated to import cleanly. If we need 
         # them, we'll need to rename in each wave before merging.
         d <- select(d, -matches("^Q[0-9_\\.]+$"))
         d$tid <- w
         return(d) }) 

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

# TODO: query this

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
