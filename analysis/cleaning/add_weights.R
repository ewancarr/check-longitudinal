# Title:        Merge baseline weights with clean datasets
# Author:       Ewan Carr
# Started:      2021-05-12

library(tidyverse)
library(here)

for (d in c("rep", "baseline", "weights")) {
    load(here("data", "clean", str_glue("{d}.Rdata")), verbose = TRUE)
}

# Check: does everyone in the `sel` dataset have a baseline weight
all(table(unique(sel$pid) %in% weights$pid))

# Merge weights with longitudinal data
sel <- inner_join(sel, weights, by = "pid") 
aw <- inner_join(aw, weights, by = "pid") 

# Merge weights with baseline data
bl <- inner_join(bl, weights, by = "pid") 

# Check: does everyone in the merged dataset have a baseline weight?
table(is.na(sel$rw))
table(is.na(aw$rw))
table(is.na(bl$rw))

# Save
save(sel, aw, bl, file = here("data", "clean", "clean.Rdata"))


