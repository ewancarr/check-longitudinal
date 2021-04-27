# Title:        Prepare inputs for Mplus on Rosalind
# Author:       Ewan Carr
# Started:      2021-04-21

library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)

load(here("data", "clean", "rep.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                        Prepare data for Mplus                        #####
####                                                                      #####
###############################################################################

# Reshape outcomes to WIDE format ---------------------------------------------

wide_data <- sel %>%
    group_by(pid, dap) %>%
    filter(# Check data is unique by individual and survey period
           n() == 1) %>%
    select(# ID variables
           pid, dap,
           # Individual-level variables
           gad, phq) %>%
    ungroup() %>%
    gather(k, v, -pid, -dap) %>%
    mutate(k = paste0(k, sprintf("%02d", dap))) %>%
    select(-dap) %>%
    spread(k, v)

# Alternative: reshape outcomes to LONG format --------------------------------

long_data <- sel %>%
    select(pid, dap, age, female, is_staff, phq, gad)
    
# Add baseline variables ------------------------------------------------------

bl <- sel %>%
    select(pid, dap, age, female) %>%
    arrange(pid, dap) %>%
    group_by(pid) %>%
    summarise(across(c(age, female), first))

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
                       form = "quadratic",
                       names_statement,
                       starts = FALSE,
                       boot = FALSE) {
    # Get number of time points from input file
    dap <- str_split(names_statement, " ") %>%
        unlist() %>%
        map_chr(str_trim) %>%
        keep(~ str_detect(.x, "[0-9]{2}$")) %>%
        map_dbl(parse_number) %>%
        unique() %>%
        sort() 
    # Get first/last time point
    start <- nth(dap, 1)
    end <- nth(dap, -1)
    pad <- function(x) sprintf("%.2d", x)
    # Get "NAMES" statement from input file
    vn <- str_wrap(str_squish(names_statement), 40)
    # Define right-hand side of growth curve
    right_side <- str_wrap(str_squish(paste(y, pad(start:end), "@", (start:end), sep = "", collapse = " ")), 40)
    # Define title
    title <- str_glue("{toupper(model)} model for {toupper(y)}, {classes} classes") 
    # Define 'use variables'
    uv <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    # Define plot SERIES
    series <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    # Define STARTS and LRT bootstrap
    st <- ifelse(starts, "\nSTARTS = 100 20;\n", "") 
    lrt <- ifelse(boot, "LRTBOOTSTRAP = 50;", "")
    tech14 = ifelse(boot, "TECH14", "")
    # Define functional form
    ff <- case_when(form == "quadratic" ~ "i s q",
                    form == "cubic" ~ "i s q c")
    # Define constraints
    constraints <- case_when(model == "lcga" & form == "quadratic" ~ "i-q@0",
                             model == "lcga" & form == "cubic" ~ "i-c@0",
                             model == "gmm" & form == "quadratic" ~ "",
                             model == "gmm" & form == "cubic" ~ "c@0")
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
    TYPE = MIXTURE; {st} {lrt}
    MODEL:
    %OVERALL%
    {ff} | {right_side};
    {constraints};
    OUTPUT:
    SAMPSTAT STANDARDIZED TECH7 TECH8 TECH11 TECH13 {tech14};
    PLOT:
    TYPE IS PLOT1 PLOT2 PLOT3;
    SERIES IS {series} (*);
    "))
}

inputs <- list(dpath           = "../data/check_wide.dat",
               y               = c("gad", "phq"),
               form            = c("quadratic", "cubic"),
               names_statement = input_file$wide[4],
               classes         = 2:12,
               starts          = TRUE,
               boot            = FALSE,
               model           = c("lcga", "gmm")) %>%
    cross() %>%
    map(~ exec(make_input, !!!.x))

# Add "SAVEDATA" statement
inputs <- map2(inputs, 1:length(inputs),
               ~ paste0(.x, str_glue("\n\nSAVEDATA: \nFILE = save{.y}.dat;\nSAVE = CPROBABILITIES;")))

# Save input files (and delete old version if exists)
walk2(inputs, 1:length(inputs), function(m, f) {
     fp <- here("analysis", "rosalind", "fits", f)
     if (file_exists(fp)) {
         file_delete(fp)
     }
     writeLines(m, fp)
   }
)

