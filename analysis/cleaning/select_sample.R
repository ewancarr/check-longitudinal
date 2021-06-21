# Title:        Select analytical sample for longitudinal paper
# Author:       Ewan Carr
# Started:      2021-06-21

library(tidyverse)
library(here)
library(naniar)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)
load(here("data", "clean", "bl.Rdata"), verbose = TRUE)

# Sample 1 --------------------------------------------------------------------
# - Staff or PGR student
# - Agreeing to longitudinal follow-up
analytical <- bl %>%
    drop_na(Role_2) %>%
    filter(Follow_1 == "Yes") %>%
    select(extref = `ExternalReference`,
           ip = `IPAddress`,
           response_id = ResponseId,
           login_id = `Login ID`) 

analytical <- analytical %>%
    left_join(ids, by = c("extref", "ip", "login_id", "response_id")) %>%
    left_join(sel, by = "pid") 
samples <- list()
samples$s1 <- unique(analytical$pid)
print(nrow(analytical))

# Sample 2: Has follow-up information on outcomes (PHQ-9, GAD-7) --------------
analytical <- analytical %>%
    group_by(pid) %>%
    mutate(n_phq = n_complete(phq_total),
              n_gad = n_complete(gad_total)) %>%
    # NOTE: we're requiring 2+ measures here, because baseline
    filter(n_phq >= 2,
           n_gad >= 2)
samples$s2 <- unique(analytical$pid)

# Sample 3: Has information on aseline covariates -----------------------------
analytical <- analytical %>%
    drop_na(is_staff,
            age,
            female,
            ethnic_f,
            child6,
            numchild,
            highrisk,
            othercare,
            kwself_b,
            prev_gad,
            prev_depress,
            livalon,
            shield_only,
            role_cat,
            chronic_any,
            relat,
            renting) 
samples$s3 <- unique(analytical$pid)

# Save
map(samples, length)
save(samples, here("data", "clean", "samples.Rdata")
