# Author:       Ewan Carr
# Started:      2021-05-17

renv::load()
library(tidyverse)
library(here)

# Import raw data from Excel
source(here("analysis", "cleaning", "import_excel.R"), echo = TRUE)

# Clean data
source(here("analysis", "cleaning", "cleaning.R"), echo = TRUE)

# Generate baseline weights
source(here("analysis", "weights", "baseline_weights.R"), echo = TRUE)

# Merge baseline weights with clean data
source(here("analysis", "cleaning", "add_weights.R"), echo = TRUE)

# Create pseudo-anonimised version
source(here("analysis", "cleaning", "pseudo_anon.R"), echo = TRUE)
