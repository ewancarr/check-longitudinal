# Title:        Process Mplus outputs from Rosalind
# Author:       Ewan Carr
# Started:      2021-04-26

library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)
library(hdf5r)

p <- here("analysis", "rosalind", "saved_fits", 
          "2021-04-26 First run of LGCA and GMM")
fit <- readModels(p)

# Extract fit statistics ------------------------------------------------------

fit_stat <- map_dfr(fit, "summaries") %>%
    janitor::clean_names() %>%
    mutate(model = case_when(str_detect(title, "^LCGA") ~ "LCGA",
                             str_detect(title, "^GMM") ~ "GMM"),
           y = case_when(str_detect(title, "GAD,") ~ "GAD",
                         str_detect(title, "PHQ,") ~ "PHQ")) %>%
    select(y, model, nclass = n_latent_classes, everything())

save(fit_stat, file = here("analysis", "outputs", "fit_stat.Rdata"))

lookup <- fit_stat %>%
    select(y, model, nclass, filename) %>%
    mutate(filename = parse_number(filename))


# Extract class sizes ---------------------------------------------------------

class_size <- fit %>% 
    map_dfr(~ .x$class_counts$mostLikely, .id = "filename") %>%
    mutate(filename = parse_number(filename)) %>%
    left_join(lookup)

save(class_size, file = here("analysis", "outputs", "class_size.Rdata"))

# Extract class summaries -----------------------------------------------------

extract_plot_data <- function(file, ifn = 2:33) {
    gh5 <- H5File$new(file, mode = "r+")
    return(data.frame(
      ifn = ifn,
      est = gh5[["means_and_variances_data/y_estimated_means/values"]][,],
      obs = gh5[["means_and_variances_data/y_observed_means/values"]][,]
    ))
}


get_filename <- function(path) { path_ext_remove(path_file(path)) }

gh5 <- map_dfr(dir_ls(p, glob = "*.gh5"),
               extract_plot_data,
               .id = "filename")  %>%
    mutate(filename = parse_number(get_filename(filename)))

class_summaries <- gh5 %>%
    left_join(lookup) %>%
    gather(k, v, -filename, -ifn, -nclass, -y, -model) %>%
    as_tibble() %>%
    mutate(class = as.numeric(str_match(k, "^[estobs]+\\.([0-9]+)$")[,2]),
           type = case_when(str_detect(k, "^est") ~ "est",
                            str_detect(k, "^obs") ~ "obs")) %>%
    select(nclass, class, y, model, ifn, v, type) %>%
    spread(type, v) %>%
    arrange(nclass, class, y, model, ifn)


save(class_summaries, file = here("analysis",
                                  "outputs",
                                  "class_summaries.Rdata"))

