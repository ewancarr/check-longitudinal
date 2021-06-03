# Title:        Merge baseline weights with clean datasets
# Author:       Ewan Carr
# Started:      2021-05-12

renv::load()
library(tidyverse)
library(here)

load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "weights_comb.Rdata"), verbose = TRUE)

# Check: does everyone in the `sel` dataset have a baseline weight
stopifnot(all(table(unique(sel$pid) %in% weights_comb$pid)))

# Merge weights with longitudinal data
sel <- full_join(sel, weights_comb, by = c("pid", "dap"))
aw <- left_join(aw, weights_comb, by = c("pid", "dap"))

# Merge weights with baseline data
bl <- full_join(select(bl, -dap),
                filter(weights_comb, dap == 0), 
                by = "pid")

# Check: does everyone in the merged dataset have a baseline weight?
table(is.na(sel$w_comb))
table(is.na(aw$w_comb))
table(is.na(bl$w_comb))

# Save
save(sel, aw, bl, file = here("data", "clean", "check.Rdata"))
