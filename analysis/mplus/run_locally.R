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

# future_walk(inp, ~ system(str_glue("cd '{root}'; mplus '{.x}'"),
                          # ignore.stdout = TRUE))

mplus <- '/opt/cxoffice/bin/wine --bottle "Mplus_8" --cx-app "Mplus.exe"'

for (i in inp) {
    cat(str_glue("Fitting: {i}"))
    system(str_glue("cd '{root}'; {mplus} '{i}'"), ignore.stdout = TRUE)
}
