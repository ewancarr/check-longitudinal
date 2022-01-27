# Title:        Script to run models locally
# Author:       Ewan Carr
# Started:      2021-04-30

# NOTE: only a good idea for simpler models.

library(tidyverse)
library(here)
library(fs)
library(furrr)
library(MplusAutomation)
plan(multicore, workers = 24)
inp <- dir_ls(here("analysis", "mplus", "fits")) %>%
    keep(~ str_detect(.x, "/[0-9]+$"))
root = here("analysis", "mplus", "fits")
mplus <- '/opt/cxoffice/bin/wine --bottle "Mplus_8" --cx-app "Mplus.exe"'

# Re-run R3STEP models --------------------------------------------------------

inpdir <- here("analysis", "mplus", "input_files", "r3step")
r3step <- dir_ls(inpdir, glob = "*adj.inp")

# Add unadjusted age/sex models
r3step <- c(r3step,
            paste0(inpdir, "/gad_agesex.inp"),
            paste0(inpdir, "/phq_agesex.inp"))

root <- here("analysis", "mplus", "fits", "r3step")
for (i in r3step) {
    cat(str_glue("Fitting: {i}"))
    system(str_glue("cd '{root}'; {mplus} '{i}'"), ignore.stdout = TRUE)
}

system(str_glue("cd '{root}'; {mplus} '{i}'"), ignore.stdout = TRUE)
