# Title:        Prepare inputs for Mplus on Rosalind
# Author:       Ewan Carr
# Started:      2021-04-21

library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)

load(here("data", "clean", "rep.Rdata"), verbose = TRUE)
load(here("data", "clean", "sdf.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                        Prepare data for Mplus                        #####
####                                                                      #####
###############################################################################

sdf$female <- sdf$female == "TRUE"

# Reshape outcomes to WIDE format ---------------------------------------------

wide_data <- sdf %>%
    group_by(pid, ifn) %>%
    filter(n() == 1,
           ifn > 1) %>%
    select(pid, ifn, gadnorm100, phqnorm100) %>%
    ungroup() %>%
    gather(k, v, -pid, -ifn) %>%
    mutate(k = str_replace(k, "norm100", ""),
           k = paste0(k, sprintf("%02d", ifn))) %>%
    select(-ifn) %>%
    spread(k, v)

# Add baseline variables ------------------------------------------------------

bl <- sdf %>%
    select(pid, ifn, age, female) %>%
    arrange(pid, ifn) %>%
    group_by(pid) %>%
    summarise(across(c(age, female) , first))

wide_data <- inner_join(bl, wide_data, by = "pid")

# Check: missing data?
wide_data %>%
    summarise(across(everything(), naniar::pct_complete)) %>%
    gather(measure, pct_complete) %>%
    as.data.frame()

# Export, create input file ---------------------------------------------------
input_file <- prepareMplusData(wide_data,
                               filename = here("analysis",
                                               "rosalind",
                                               "data",
                                               "check.dat"))

writeLines(input_file, here("analysis",
                            "rosalind",
                            "data",
                            "check.inp"))



###############################################################################
####                                                                      #####
####                          Create input files                          #####
####                                                                      #####
###############################################################################

"
For now, we're running:

1. Latent class growth analysis (LCGA)
2. Growth mixture models

The former is a simplified version of GMM, where within-class variations
(variances and covariances) are constrained to zero. We'll fit models for
cubic growth using (1) and (2) with increasing numbers of classes and examine
model fit.
"

make_input <- function(dpath, y, classes, model) {
    # Get number of time points from input file
    ifn <- str_split(input_file[4], " ") %>%
        unlist() %>%
        map_chr(str_trim) %>%
        keep(~ str_detect(.x, "[0-9]{2}$")) %>%
        map_dbl(parse_number) %>%
        unique() %>%
        sort() 
    # Get first/last time point
    start <- nth(ifn, 1)
    end <- nth(ifn, -1)
    pad <- function(x) sprintf("%.2d", x)
    # Get "NAMES" statement from input file
    vn <- input_file[4]
    # Define right-hand side of growth curve
    right_side <- str_wrap(paste(y, pad(start:end), "@", (start:end) - 1, sep = "", collapse = " "), 40)
    # Define constraints
    constraints <- case_when(model == "lcga" ~ "int-cubic@0",
                             model == "gmm" ~ "cubic@0")
    title <- str_glue("{toupper(model)} model for {toupper(y)}, {classes} classes") 
    # Generate the model
    return(str_glue("
    TITLE: {title}
    DATA: FILE = {dpath};
    VARIABLE: 
    {vn} 
    USEVARIABLES = {uv};
    CLASSES = C({classes});
    IDVARIABLE = pid;
    MISSING=.;
    ANALYSIS:
    TYPE = MIXTURE;
    STARTS = 500 10;
    STITERATIONS  10;
    LRTBOOTSTRAP = 50;
    ALGORITHM = INTEGRATION;
    ESTIMATOR = MLR;
    MODEL:
    %OVERALL%
    int linear quad cubic | {right_side};
    {constraints};
    OUTPUT:
    SAMPSTAT STANDARDIZED TECH7 TECH8 TECH11 TECH13 TECH14;
    PLOT:
    TYPE IS PLOT1 PLOT2 PLOT3;
    SERIES IS {uv} (*);
    "))
}

inputs <- list(dpath = "analysis/rosalind/data/check.dat",
               y = c("gad", "phq"),
               classes = 2:10,
               model = c("lcga", "gmm")) %>%
    cross() %>%
    map(~ exec(make_input, !!!.x))

walk2(inputs, 1:length(inputs),
     ~ writeLines(.x, here("analysis", "rosalind", "fits", .y)))

