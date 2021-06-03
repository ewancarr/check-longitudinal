# Title:        Model longitudinal non-response
# Author:       Ewan Carr
# Started:      2021-02-23
# Updated:      2021-06-03

library(tidyverse)
library(glmnet)
library(VIM)
library(here)

# Load data
load(here("data", "clean", "check.Rdata"), verbose = TRUE)

# Load baseline weights
load(here("data", "clean", "weights_bl.Rdata"), verbose = TRUE)

# Generate indicator of response at each wave (TRUE = responded)
resp <- sel %>%
    ungroup() %>%
    group_split(dap) %>%
    map(function(d) {
            v <- paste0(first(d$dap))
            d <- mutate(d, {{v}} := !is.na(phq_total)) %>%
                select(pid, matches("^[0-9]+$")) }) %>%
    reduce(full_join, by = "pid") %>%
    mutate(across(`0`:`53`, ~ replace_na(.x, FALSE))) %>%
    gather(dap, resp, -pid) 

# Check: proportion of non-response by week
resp %>%
    mutate(dap = parse_number(dap)) %>%
    group_by(dap) %>%
    summarise(resp = mean(resp)) %>%
    print(n = 100)

# Prepare baseline variables
bl <- sel %>%
    filter(dap == 0) %>%
    mutate(anychild = numchild != "0") %>%
    select(pid, age, female, ethnic_f, relat, anychild,
           chronic_any, is_staff, renting, role_cat) %>%
    mutate(across(c(role_cat, relat), ~ na_if(.x, "Missing"))) %>%
    kNN(imp_var = FALSE)

# Check: proportion responding by each baseline variable
check <- function(var) {
    left_join(resp, bl, by = "pid") %>%
    mutate(dap = parse_number(dap)) %>%
    group_by(dap, {{var}}) %>%
    summarise(resp = mean(resp)) %>%
    spread(dap, resp)
}

check(ethnic_f)
check(female)
check(relat)
check(chronic_any)

# Generate weights
generate_weights <- function(X) {
    dat <- left_join(bl, X, by = "pid")
    form <- as.formula(paste0("resp ~ age + female + factor(ethnic_f) + ",
                              "factor(relat) + chronic_any + renting + ",
                              "factor(role_cat) + anychild"))
    mm <- model.matrix(form, data = dat)[, -1]
    cv_model <- cv.glmnet(mm, dat$resp, alpha = 1, family = "binomial")
    pred <- predict(cv_model, mm, s = "lambda.min", type = "response")[,1]
    dat$w_long <- (1 / pred) * mean(dat$resp)
    return(dat)
}

weights_long <- map_dfr(sort(unique(sel$dap)[-1]),
    ~ filter(resp, dap == .x) %>% 
        generate_weights() %>%
        select(pid, dap, w_long) %>%
        mutate(dap = parse_number(dap)))

# Check for extreme weights
weights_long %>%
    group_by(dap) %>%
    summarise(av = mean(w_long),
              min = min(w_long),
              max = max(w_long)) %>% print(n = 100)

# Add a longitudinal weight of '1' for baseline
weights_long <- sel %>%
    filter(dap == 0) %>%
    select(pid, dap) %>%
    mutate(w_long = 1) %>%
    bind_rows(weights_long)

# Combine baseline and longitudinal weights
weights_comb <- weights_bl %>%
    rename(w_bl = rws) %>%
    full_join(weights_long, by = "pid") %>%
    mutate(w_comb = w_bl * w_long) %>%
    select(pid, dap, w_bl, w_long, w_comb)

# Save ------------------------------------------------------------------------
save(weights_comb, file = here("data", "clean", "weights_comb.Rdata"))
