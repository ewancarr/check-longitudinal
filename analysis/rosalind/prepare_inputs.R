# Title:        Prepare inputs for Mplus on Rosalind
# Author:       Ewan Carr
# Started:      2021-04-21

library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)

load(here("data", "clean", "rep.Rdata"), verbose = TRUE)
# load(here("data", "clean", "sdf.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                        Prepare data for Mplus                        #####
####                                                                      #####
###############################################################################

# Reshape outcomes to WIDE format ---------------------------------------------

sel

wide_data <- sel %>%
    group_by(pid, ifn) %>%
    filter(# Check data is unique by individual and fortnight
           n() == 1, 
           # Remove the first fortnight, because very few completed this
           # and it causes problems in Mplus.
           ifn > 1) %>%
    select(# ID variables
           pid, ifn,
           # Individual-level variables
           gad, phq, 
           # Contextual variables
           dth = deaths28, 
           hcase = hospital_cases,
           nadmit = new_admissions) %>%
    ungroup() %>%
    gather(k, v, -pid, -ifn) %>%
    mutate(k = paste0(k, sprintf("%02d", ifn))) %>%
    select(-ifn) %>%
    spread(k, v)

# Alternative: reshape outcomes to LONG format --------------------------------

long_data <- sel %>%
    select(pid, ifn,
           age, female, is_staff,
           phq, gad,
           dth = deaths28,
           hcase = hospital_cases,
           nadmit = new_admissions)
    
# Add baseline variables ------------------------------------------------------

bl <- sel %>%
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

prep <- function(dat, stub, p) {
    input_file <- prepareMplusData(dat,
                                   filename = paste0(p, "/", stub, ".dat"))
    writeLines(input_file, paste0(p, "/", stub, ".inp"))
    return(input_file)
}

input_file <- map2(list(wide_data, long_data),
      c("check_wide", "check_long"),
      prep,
      p = here("analysis", "rosalind", "data"))
names(input_file) <- c("wide", "long")

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

make_input <- function(dpath,
                       y,
                       classes,
                       model,
                       starts = FALSE,
                       boot = FALSE) {
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
    vn <- str_wrap(str_squish(input_file[4]), 40)
    # Define right-hand side of growth curve
    right_side <- str_wrap(str_squish(paste(y, pad(start:end), "@", (start:end) - 1, sep = "", collapse = " ")), 40)
    # Define constraints
    constraints <- case_when(model == "lcga" ~ "int-cubic@0",
                             model == "gmm" ~ "cubic@0")
    # Define title
    title <- str_glue("{toupper(model)} model for {toupper(y)}, {classes} classes") 
    # Define 'use variables'
    uv <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    # Define plot SERIES
    series <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    # Define STARTS and LRT bootstrap
    st <- ifelse(starts, "STARTS = 500 10;\nSTITERATIONS = 10;", "") 
    lrt <- ifelse(boot, "LRTBOOTSTRAP = 50;", "")
    tech14 = ifelse(boot, "TECH14", "")
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
    {st}
    {lrt}
    ! ALGORITHM = INTEGRATION;
    ! ESTIMATOR = MLR;
    MODEL:
    %OVERALL%
    int linear quad cubic | {right_side};
    {constraints};
    OUTPUT:
    SAMPSTAT STANDARDIZED TECH7 TECH8 TECH11 TECH13 {tech14};
    PLOT:
    TYPE IS PLOT1 PLOT2 PLOT3;
    SERIES IS {series} (*);
    "))
}


inputs <- list(dpath = "../data/check.dat",
               y = c("gad", "phq"),
               classes = 2:10,
               model = c("lcga", "gmm")) %>%
    cross() %>%
    map(~ exec(make_input, !!!.x))

# Delete 

walk2(inputs, 1:length(inputs), function(m, f) {
     fp <- here("analysis", "rosalind", "fits", f)
     if (file_exists(fp)) {
         file_delete(fp)
     }
     writeLines(m, fp)
   }
)

