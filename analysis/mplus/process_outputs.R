# Title:        Process Mplus outputs from Rosalind
# Author:       Ewan Carr
# Started:      2021-04-26

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
        res$r3step <- extract_r3step(path)
    }
    return(res)
}

read_dir <- function(p, r3step = FALSE) {
    fit <- future_map(dir_ls(p, glob = "*.out"), read_models, r3step)
    names(fit) <- path_ext_remove(path_file(names(fit)))
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
####                              GMM models                              #####
####                                                                      #####
###############################################################################

current <- "2021-05-04 Local fits with decimal timings"
p <- here("analysis", "rosalind", "saved_fits", current)

# Load index ------------------------------------------------------------------
load(here("analysis", "rosalind", "index.Rdata"), verbose = TRUE)

# Load source data ------------------------------------------------------------
load(here("data", "clean", "rep.Rdata"), verbose = TRUE)

# Load Mplus outputs ----------------------------------------------------------
gmm <- read_dir(p)

# Extract fit statistics ------------------------------------------------------
fit_stat <- map_dfr(gmm, "summaries", .id = "model_id") %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(model_id = as.numeric(model_id)) %>%
    left_join(index, by = "model_id")

lookup <- fit_stat %>%
    select(y, model, form, classes, model_id)

# Extract class sizes ---------------------------------------------------------
class_size <- gmm %>% 
    map_dfr(~ .x$class_counts$mostLikely, .id = "filename") %>%
    mutate(model_id = parse_number(filename)) %>% 
    left_join(lookup) %>%
    arrange(model_id, class)

# Extract class summaries -----------------------------------------------------
inp <- readLines(paste0(p, "/1"))
start <- grep("^NAMES =.*", inp)
end <- grep("^USEVARIABLES =.*", inp) - 1
times <- suppressWarnings(unique(na.omit(parse_number(str_split(paste(inp[start:end], collapse = " "), " ")[[1]]))))

gh5 <- map_dfr(dir_ls(p, glob = "*.gh5"),
               extract_plot_data,
               .id = "model_id")  %>%
    mutate(model_id = parse_number(get_filename(model_id)))

class_summaries <- gh5 %>%
    left_join(lookup) %>%
    gather(k, v, -model_id, -dap, -classes, -form, -y, -model) %>%
    as_tibble() %>%
    drop_na() %>%
    mutate(class = as.numeric(str_match(k, "^[estobs]+\\.([0-9]+)$")[,2]),
           type = case_when(str_detect(k, "^est") ~ "est",
                            str_detect(k, "^obs") ~ "obs")) %>%
    select(classes, class, y, model, dap, v, type) %>%
    spread(type, v) %>%
    arrange(classes, class, y, model, dap)

# Extract ID variable ---------------------------------------------------------
class_ids <- map_dfr(gmm, "savedata", .id = "model_id") %>%
    clean_names() %>%
    select(model_id, pid, c, cprob1, cprob2) %>%
    mutate(model_id = as.numeric(model_id)) %>%
    as_tibble() %>%
    left_join(lookup)

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

current <- "2021-05-10 First R3STEP results"
p <- here("analysis", "rosalind", "saved_fits", current)

r3step <- read_dir(p, r3step = TRUE)
names(r3step) <- str_extract(names(r3step), "\\d+$")

load(here("analysis", "rosalind", "index2.Rdata"))

odds_ratios <- map(r3step, "r3step", .id = "model_id") %>%
    discard(~ length(.x) == 1) %>%
    map_dfr(.id = "model_id", ~ .x) %>%
    left_join(index2) %>%
    as_tibble() %>%
    mutate(on = str_squish(on),
           across(c(model_id, reference), as.numeric),
           cell = str_glue("{sprintf('%.3f', or)} [p={sprintf('%.3f', pval)}]"))

odds_ratios

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
     file = here("analysis", "outputs", "class_summaries.Rdata"))
