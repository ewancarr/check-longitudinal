# Title:        Process Mplus outputs from Rosalind
# Author:       Ewan Carr
# Started:      2021-04-26

renv::load()
library(tidyverse)
library(furrr)
plan(multicore, workers = 12)
library(here)
library(fs)
library(MplusAutomation)
library(janitor)
library(hdf5r)
library(lubridate)
source(here("analysis", "functions", "extract_multinomial.R"))

read_models <- function(path, r3step = FALSE) {
    res <- MplusAutomation::readModels(path)
    if (r3step) {
        try(res$r3step <- extract_r3step(path), silent = TRUE)
    }
    return(res)
}

read_dir <- function(p, r3step = TRUE) {
    fit <- future_map(dir_ls(p, glob = "*.out", recurse = TRUE),
                      read_models, r3step)
    return(fit)
}

extract_plot_data <- function(file, dap = times) {
    gh5 <- H5File$new(file, mode = "r+")
    return(data.frame(
      dap = dap,
      est = gh5[["means_and_variances_data/y_estimated_means/values"]][, ],
      obs = gh5[["means_and_variances_data/y_observed_means/values"]][, ]
    ))
}

get_filename <- function(path) {
    path_ext_remove(path_file(path))
}

###############################################################################
####                                                                      #####
####                           Load output files                          #####
####                                                                      #####
###############################################################################

current <- "2022-01-13 Update R3STEP to adjust for prior mental health"
# current <- "2021-06-01 Remove sparse TVCs"
p <- here("analysis", "mplus", "saved_fits", current)

# Check: are any output files missing? ----------------------------------------
dir_ls(p, recurse = TRUE) %>%
    as_tibble() %>%
    mutate(fn = get_filename(value),
           ext = path_ext(value),
           parent = path_dir(path_rel(value, p)),
           present = TRUE) %>%
    filter(ext %in% c("inp", "out")) %>%
    select(parent, fn, ext, present) %>%
    spread(ext, present) %>%
    print(n = 100)

# Load source data ------------------------------------------------------------
load(here("data", "clean", "check.Rdata"), verbose = TRUE)

# Load Mplus outputs ----------------------------------------------------------
fits <- read_dir(p)

# Load GH5 output -------------------------------------------------------------
inp <- readLines(paste0(p, "/gmm/gmm_gad_4_cubic.inp"))
start <- grep("^NAMES =.*", inp)
end <- grep("^USEVARIABLES =.*", inp) - 1
times <- parse_number(grep("^gad[0-9]+",
                           str_split(paste(inp[start:end],
                                           collapse = " "), " ")[[1]],
                           value = TRUE))
fits <- map2(fits, names(fits),
     function(res, path) {
         try(res$gh5 <- extract_plot_data(path_ext_set(path, "gh5"), times),
             silent = TRUE)
         return(res)
     })

names(fits) <- path_rel(path_ext_remove(names(fits)),
                        here("analysis", "mplus", "saved_fits", current))


# Function to ensure class numbering is consistent across outcomes ------------

lu <- fits %>%
    map_dfr("gh5", .id = "model_id") %>%
    filter(model_id %in% c("gmm/gmm_gad_4_cubic",
                           "gmm/gmm_phq_4_cubic",
                           "r3step/gad_agesex",
                           "r3step/phq_agesex"),
           dap == 53) %>%
    select(model_id, est.1, est.2, est.3, est.4) %>%
    separate(model_id, c("type", "model"), sep = "/") %>%
    mutate(y = str_match(model, "gad|phq")) %>%
    select(type, y, est.1:est.4) %>%
    gather(orig_class, new_class, -type, -y) %>%
    group_by(type, y) %>%
    mutate(orig_class = parse_number(str_replace(orig_class, "\\.", "")),
           new_class = rank(-new_class)) %>%
    arrange(type, y, new_class)

make_classes_consistent <- function(dat) {
    left_join(dat, lu, by = c("model" = "type",
                         "y" = "y",
                         "class" = "orig_class"))
}

###############################################################################
####                                                                      #####
####                              GMM models                              #####
####                                                                      #####
###############################################################################

pick <- function(fits, term) {
    res <- fits[str_detect(names(fits), term)]
    names(res) <- str_replace(names(res), paste0(term, "/"), "")
    return(res)
}

get_id <- function(d) {
    separate(d, model_id, c("model", "y", "nclasses", "form"))
}

gmm <- pick(fits, "^gmm")

# Extract fit statistics ------------------------------------------------------
fit_stat <- map_dfr(gmm, "summaries", .id = "model_id") %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    get_id()

# Extract class sizes ---------------------------------------------------------
class_size <- gmm %>%
    map_dfr(~ .x$class_counts$mostLikely, .id = "model_id") %>%
    arrange(model_id, class) %>%
    get_id() %>%
    make_classes_consistent()

# Extract class summaries -----------------------------------------------------
class_summaries <- gmm %>%
    map_dfr("gh5", .id = "model_id") %>%
    get_id() %>%
    gather(k, v, -dap, -nclasses, -form, -y, -model) %>%
    as_tibble() %>%
    drop_na() %>%
    mutate(class = as.numeric(str_match(k, "^[estobs]+\\.([0-9]+)$")[, 2]),
           type = case_when(str_detect(k, "^est") ~ "est",
                            str_detect(k, "^obs") ~ "obs")) %>%
    select(nclasses, class, y, model, dap, v, type) %>%
    spread(type, v) %>%
    arrange(nclasses, class, y, model, dap) %>%
    make_classes_consistent()

# Get date corresponding to 'dap' ---------------------------------------------
date_lookup <- sel %>%
    group_by(dap) %>%
    summarise(time = first(midpoint))

class_summaries <- class_summaries %>%
    left_join(date_lookup, by = "dap")

# Get individual IDs by class -------------------------------------------------
class_ids <- map_dfr(gmm, "savedata", .id = "model_id") %>%
    as_tibble() %>%
    clean_names() %>%
    select(model_id, pid, class = c) %>%
    separate(model_id, c("model", "y", "nclasses", "form")) %>%
    make_classes_consistent()

###############################################################################
####                                                                      #####
####                             R3STEP MODELS                            #####
####                                                                      #####
###############################################################################

get_id <- function(d) {
    separate(d, model_id, c("y", "x"))
}
r3step <- pick(fits, "^r3step")

# Check: did any models fail to run?
map_dfr(r3step, ~ length(.x$errors)) %>%
    gather(model, errors) %>%
    filter(errors > 0)

# Extract odds ratios
odds_ratios <- map(r3step, "r3step") %>%
    map_dfr(.id = "model_id", ~ .x) %>%
    mutate(cell = str_glue("{est} [{low025}, {up025}]"),
           adj = if_else(str_detect(model_id, "_adj$"), "adj", "unadj"),
           model_id = str_replace(model_id, "_adj$", ""),
           across(c(ref, class), parse_number),
           y = str_match(model_id, "^([a-z]+)[_a-z]+$")[, 2]) %>%
    select(y, model_id, adj, ref, class, param, cell, est, low025, up025) %>%
    mutate(type = "r3step") %>%
    rename(orig_class = class,
           orig_ref = ref) %>%
    # Switch classes to be consistent
    left_join(lu, by = c("type", "orig_class", "y")) %>%
    # Switch reference class to be consistent
    left_join(rename(lu,
                     orig_ref = orig_class,
                     new_ref = new_class)) %>%
    # Remove coefficients for pranx/prdep if used as adjustments
    filter(!(param %in% c("pranx", "prdep") & 
             (!str_detect(model_id, "pranx|prdep"))))

###############################################################################
####                                                                      #####
####                              TVC models                              #####
####                                                                      #####
###############################################################################

tvcov <- pick(fits, "^tvcov")

process_tvc <- function(tvc, lookup) {
    samp <- tvc$summaries$Observations
    tvc$parameters$ci.std.standardized %>%
                      filter(str_detect(paramHeader, "^[GADPHQ0-9]+\\.ON")) %>%
    clean_names() %>%
    mutate(dap = parse_number(param_header),
           y = tolower(str_extract(param_header, "^GAD|^PHQ")),
           tvc = tolower(str_replace_all(param, "[0-9]", "")),
           n_classes = max(latent_class),
           n_sample = samp) %>%
    select(y, tvc, dap, low2_5, est, up2_5,
           orig_class = latent_class, n_sample) %>%
    arrange(orig_class, dap)
}

tvcov_coef <- map_dfr(tvcov, process_tvc)

###############################################################################
####                                                                      #####
####                                 SAVE                                 #####
####                                                                      #####
###############################################################################

save(fit_stat,
     class_size,
     class_summaries,
     class_ids,
     r3step,
     odds_ratios,
     tvcov, tvcov_coef,
     file = here("analysis", "outputs", "class_summaries.Rdata"))
