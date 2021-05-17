# Title:        Create pseudo-anonymised version of the longitudinal dataset
# Author:       Ewan Carr
# Started:      2021-04-09

# Specifically, we want to remove the folling columns:
#
# - IPAddress
# - RecipientLastName
# - RecipientFirstName
# - RecipientEmail
# - LocationLatitude
# - LocationLongitude
# - Follow_2 (email)
# - Follow_3 (first name)
# - Follow_5 (mobile number)
# - Testing_2 (address)
# - Testing_3 (address)
# - Testing_4 (address)
# - Testing_5 (address)
# - Q18 (post code)
# - Final_1 (free text comments)

renv::load()
library(tidyverse)
library(here)
load(here("data", "clean", "aw.Rdata"), verbose = TRUE)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)

aw_pseudo <- aw %>%
    select(-pid_str,
           -login_id,
           -response_id,
           -extref,
           -start_date,
           -end_date, 
           -recorded_date,
           -status, 
           -ip,
           -progress,
           -duration, 
           -lastname,
           -firstname,
           -email,
           -lat,
           -long,
           -distrib,
           -lang,
           -follow2,
           -follow3,
           -follow5,
           -testing2,
           -testing3,
           -testing4,
           -testing5,
           -postcode,
           -comms_comments,
           -positive_change_detail,
           -chronic_other,
           -discrimination_2)

save(aw_pseudo, sel, file = here("data", "clean", "pseudo_anon.Rdata"))
