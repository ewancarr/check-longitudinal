# Title:        Process Mplus outputs from Rosalind
# Author:       Ewan Carr
# Started:      2021-04-26

renv::load()
library(tidyverse)
library(furrr)
plan(multicore)
library(here)
library(fs)
library(MplusAutomation)
library(janitor)
library(hdf5r)
library(lubridate)
source(here("analysis", "functions", "extract_multinomial.R"))

read_models <- function(path, r3step = FALSE) {
    res <- readModels(path)
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
      est = gh5[["means_and_variances_data/y_estimated_means/values"]][,],
      obs = gh5[["means_and_variances_data/y_observed_means/values"]][,]
    ))
}

get_filename <- function(path) { path_ext_remove(path_file(path)) }


###############################################################################
####                                                                      #####
####                           Load output files                          #####
####                                                                      #####
###############################################################################

current <- "2021-05-24 Updated GMM and R3STEP models"
p <- here("analysis", "mplus", "saved_fits", current)

# Load source data ------------------------------------------------------------
load(here("data", "clean", "check.Rdata"), verbose = TRUE)

# Load Mplus outputs ----------------------------------------------------------
fits <- read_dir(p)

# Load GH5 output -------------------------------------------------------------
inp <- readLines(paste0(p, "/gmm/gmm_gad_2_cubic.inp"))
start <- grep("^NAMES =.*", inp)
end <- grep("^USEVARIABLES =.*", inp) - 1
times <- parse_number(grep("^gad[0-9]+", str_split(paste(inp[start:end], collapse = " "), " ")[[1]], value = TRUE))

fits <- map2(fits, names(fits),
     function(res, path) {
         try(res$gh5 <- extract_plot_data(path_ext_set(path, "gh5"), times),
             silent = TRUE)
         return(res)
     })

names(fits) <- path_rel(path_ext_remove(names(fits)),
                        here("analysis", "mplus", "saved_fits", current))

###############################################################################
####                                                                      #####
####                              GMM models                              #####
####                                                                      #####
###############################################################################

get_id <- function(d) { separate(d, model_id, c("model", "y", "nclasses", "form")) }
gmm <- fits[str_detect(names(fits), "^gmm")]
names(gmm) <- str_replace(names(gmm), "^gmm/", "")

# Extract fit statistics ------------------------------------------------------
fit_stat <- map_dfr(gmm, "summaries", .id = "model_id") %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    get_id()

# Extract class sizes ---------------------------------------------------------
class_size <- gmm %>% 
    map_dfr(~ .x$class_counts$mostLikely, .id = "model_id") %>%
    arrange(model_id, class) %>%
    get_id() 

# Extract class summaries -----------------------------------------------------
class_summaries <- gmm %>%
    map_dfr("gh5", .id = "model_id") %>%
    get_id() %>%
    gather(k, v, -dap, -nclasses, -form, -y, -model) %>%
    as_tibble() %>%
    drop_na() %>%
    mutate(class = as.numeric(str_match(k, "^[estobs]+\\.([0-9]+)$")[,2]),
           type = case_when(str_detect(k, "^est") ~ "est",
                            str_detect(k, "^obs") ~ "obs")) %>%
    select(nclasses, class, y, model, dap, v, type) %>%
    spread(type, v) %>%
    arrange(nclasses, class, y, model, dap)

# Get date corresponding to 'dap' ---------------------------------------------
date_lookup <- sel %>%
    group_by(dap) %>%
    summarise(time = first(midpoint))

class_summaries <- class_summaries %>%
    left_join(date_lookup, by = "dap")

###############################################################################
####                                                                      #####
####                             R3STEP MODELS                            #####
####                                                                      #####
###############################################################################

get_id <- function(d) { separate(d, model_id, c("y", "x")) }
r3step <- fits[str_detect(names(fits), "^r3step")]
names(r3step) <- str_replace(names(r3step), "^r3step/", "")

# Check: did any models fail to run?
map_dfr(r3step, ~ length(.x$errors)) %>%
    gather(model, errors) %>%
    filter(errors > 0)

# Extract odds ratios
odds_ratios <- map(r3step, "r3step") %>%
    discard(~ length(.x) == 1) %>%
    map_dfr(.id = "model_id", ~ .x) %>%
    mutate(on = str_squish(on),
           reference = as.numeric(reference),
           cell = str_glue("{sprintf('%.3f', or)} [p={sprintf('%.3f', pval)}]"),
           adj = if_else(str_detect(model_id, "_adj$"), "Adj.", "Unadj."))
    
###############################################################################
####                                                                      #####
####                                 SAVE                                 #####
####                                                                      #####
###############################################################################

save(fit_stat,
     class_size,
     class_summaries,
     r3step,
     odds_ratios,
     file = here("analysis", "outputs", "class_summaries.Rdata"))
