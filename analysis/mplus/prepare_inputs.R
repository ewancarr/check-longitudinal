# Title:        Prepare inputs for Mplus on Rosalind
# Author:       Ewan Carr
# Started:      2021-04-21

renv::load()
library(vctrs)
library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)
library(fastDummies)
library(naniar)
load(here("data", "clean", "check.Rdata"), verbose = TRUE)

# This script prepares Mplus input files for the longitudinal paper. There are
# three types of models:
# 
# 1. Unconditional growth mixture model (GMM)
# 2. GMM with time-invariant predictors of class membership
# 3. GMM with time-varying predictors
# 
# We're considering both PHQ-9 and GAD-7. The functions below include arguments
# that allow other options (e.g. cubic or quadratic).

###############################################################################
####                                                                      #####
####                        Prepare data for Mplus                        #####
####                                                                      #####
###############################################################################

# Decide which time points to use ---------------------------------------------

# Either all times points or 2-monthly questionnaires only.

two_monthly <- FALSE
if (two_monthly) {
    sel <- filter(sel, two_month)
}

# Reshape outcomes and time-varying covariates to WIDE format -----------------

# NOTE: I'm renaming most variables below, sorry! This is mostly for
# compatability with Mplus (which requires very short names). The aim: 6
# characters or less, [a-z] only.

wide_data <- sel %>%
    group_by(pid, dap) %>%
    filter(# Check data is unique by individual and survey period
           n() == 1) %>%
    select(# ID variables
           pid, dap,
           # Weights
           rw,
           # Outcomes
           gad = gad_total,
           phq = phq_total,
           # Time-varying covariates
           fstv = had_v1, 
           secv = had_v2,       # vaccination (renamed, because Mplus)
           furcu = fur_cu,      # furlough
           furev = fur_ev,      
           prob = probdef,      # suspected COVID
           stmat = s_material,  # stressors
           stmed = s_medi,
           stper = s_person,
           stpla = s_plans
           ) %>%
    ungroup() %>%
    gather(k, v, -pid, -dap, -rw) %>%
    mutate(k = paste0(k, sprintf("%02d", dap))) %>%
    select(-dap) %>%
    spread(k, v)

# Add baseline variables ------------------------------------------------------

baseline <- bl %>%
    select(pid,
           is_staff,
           age,
           female,
           ethnic_f,
           child6,
           nc = numchild,
           highrisk,
           othercare,
           shield_isol,
           kw = kwself_b,
           pranx = prev_gad,
           prdep = prev_depress,
           livalon,
           renting) %>%
    mutate(eth = tolower(as.character(ethnic_f)),
           othercare = othercare == "Yes",
           anychild = parse_number(as.character(nc)) > 0) %>%
    drop_na() %>%
    dummy_cols(select_columns = c("eth", "nc")) %>%
    select(-eth, -ethnic_f, -nc) 

wide_data <- inner_join(wide_data, baseline, by = "pid")

# Remove characters Mplus doesn't like
names(wide_data) <- str_replace_all(names(wide_data), "\\+", "")

# Check: missing data?
wide_data %>%
    summarise(across(everything(), pct_complete)) %>%
    gather(measure, pct_complete) %>%
    as.data.frame()

# Replace missing TVCs with a different missingness indicator -----------------

# Individuals missing information on TVCs are dropped from the analysis in Mplus.
# However, many of these individuals are also missing the corresponding outcome
# data. On the Mplus forum[1], Bengt suggests setting missing TVCs to another 
# values (i.e. besides NA):
# 
# > Bengt O. Muthen posted on Saturday, October 19, 2013 - 3:33 pm
# > Don't use a missing data flag for the time-varying covariates; use some other
# > number. If the tvs is missing at time t and the outcome at the corresponding
# > time is missing, such timepoints still won't contribute to the likelihood
# > computations. By not using a missing data flag, you avoid deleting subjects who
# > have missing on any tvcs.
#  
# > ZHANG Liang posted on Monday, March 09, 2015 - 6:04 am 	
# > Then what should I do with the missing values on TVCs? Should I just leave them
# > as blanks in .dat file while missing values on other variables are represented
# > as missing data flag?
#  
# > Bengt O. Muthen posted on Monday, March 09, 2015 - 6:08 pm 	
# > Just use some other value like 888 instead of 999.
# 
# So, here I'm setting all TVCs to '777' if the corresponding outcomes are 
# both missing (i.e. PHQ and GAD at that timepoint).
# 
# [1]: http://www.statmodel.com/discussion/messages/14/3460.html?1555003621

times <- select(wide_data, starts_with("fstv")) %>% names() %>% str_replace("fstv", "")
stubs <- c("fstv", "secv", "furcu", "furev", "prob", "stmat", "stmed", "stper",
           "stpla")

for (tp in times) {
    for (s in stubs) {
        stp <- paste0(s, tp)
        wide_data[[stp]] <- ifelse(is.na(wide_data[[paste0("phq", tp)]]) & 
                                   is.na(wide_data[[paste0("gad", tp)]]),
                                   777,
                                   wide_data[[stp]])
    }
}

# Export, create input file ---------------------------------------------------

prep <- function(dat, stub, p) {
    input_file <- prepareMplusData(dat,
                                   filename = paste0(p, "/", stub, ".dat"),
                                   overwrite = TRUE)
    writeLines(input_file, paste0(p, "/", stub, ".inp"))
    return(input_file)
}

input_file <- prep(wide_data,       
                   "check_wide",
                   here("analysis", "mplus", "data"))

###############################################################################
####                                                                      #####
####            1. Generate input files for unconditional GMMs            #####
####                                                                      #####
###############################################################################

"
We're considering two types of model:

1. Latent class growth analysis (LCGA)
2. Growth mixture models

The former is a simplified version of GMM, where within-class variations
(variances and covariances) are constrained to zero.
"

make_input <- function(dpath,
                       y,
                       classes,
                       model,
                       form = "quadratic",
                       proc = 2,
                       names_statement,
                       starts = FALSE,
                       boot = FALSE,
                       r3step = FALSE,
                       tvc = list(use = FALSE,
                                  vars = "",
                                  constrain = FALSE)) {
    # Get number of time points from input file
    dap <- str_split(names_statement, " ") %>%
        unlist() %>%
        map_chr(str_trim) %>%
        keep(~ str_detect(.x, "[gadphq][0-9]{2}$")) %>%
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
    right_side <- str_wrap(str_squish(paste(y, pad(dap), "@.", pad(dap), sep = "", collapse = " ")), 40)
    # Define title
    title <- str_glue("{toupper(model)} model for {toupper(y)}, {classes} classes") 
    # Define 'use variables'
    uv <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    if (tvc[[1]]) {
        uv <- paste0(uv, "\n", str_wrap(paste(tvc[[2]], collapse = " "), 40))
    }
    # Define plot SERIES
    series <- str_glue("{y}{pad(start)}-{y}{pad(end)}")
    # Define STARTS, PROCESSORS, LRT bootstrap
    pr <- str_glue("\n\nPROCESSORS = {proc};\n")
    st <- ifelse(starts, "\nSTARTS = 500 100;\n", "") 
    lrt <- ifelse(boot, "LRTBOOTSTRAP = 50;", "")
    tech14 = ifelse(boot, "TECH14", "")
    # Define functional form
    ff <- case_when(form == "quadratic" ~ "i s q",
                    form == "cubic" ~ "i s q cu")
    # Define constraints
    constraints <- case_when(model == "lcga" & form == "quadratic" ~ "i-q@0",
                             model == "lcga" & form == "cubic" ~ "i-cu@0",
                             model == "gmm" & form == "quadratic" ~ "",
                             model == "gmm" & form == "cubic" ~ "cu@0")
    # Define predictors (R3STEP)
    if (r3step[1] > 1) {
        r3step <- str_wrap(str_glue("AUXILIARY = {paste(r3step, '(R3STEP)', collapse = ' ')};\n"), 40)
    } else {
        r3step <- ""
    }
    # Define TVCs -------------------------------------------------------------
    delim <- if_else(tvc$constrain, " (1);", ";")
    if (tvc$use) {
        tvc_statement <- paste0(y,
                                sprintf("%02d", parse_number(tvc[[2]])),
                                " ON ", tvc$var, delim, collapse = "\n")
    } else {
        tvc_statement <- ""
    }
    # Generate the model
    return(str_glue("
    TITLE: {title}
    DATA: FILE = {dpath};
    VARIABLE: 
    {vn} 
    USEVARIABLES = {uv};
    CLASSES = C({classes});
    IDVARIABLE = pid;
    WEIGHT = rw;
    {r3step}
    MISSING=.;
    ANALYSIS: {pr}
    TYPE = MIXTURE; {st} {lrt}
    MODEL:
    %OVERALL%
    {ff} | {right_side};
    {constraints};
    {tvc_statement}
    OUTPUT:
    SAMPSTAT STANDARDIZED TECH7 TECH8 TECH11 TECH13 {tech14};
    PLOT:
    TYPE IS PLOT1 PLOT2 PLOT3;
    SERIES IS {series} (*);
    "))
}

comb1 <- list(dpath           = "../../data/check_wide.dat",
              y               = c("gad", "phq"),
              form            = "cubic",
              names_statement = input_file[4],
              classes         = 2:7,
              starts          = TRUE,
              proc            = 4,
              boot            = FALSE,
              model           = "gmm") %>%
    cross()

inputs1 <- map(comb1, ~ exec(make_input, !!!.x))

# Add "SAVEDATA" statement
inputs1 <- map2(inputs1, 1:length(inputs1),
               ~ paste0(.x, str_glue("\n\nSAVEDATA: \nFILE = save{.y}.dat;\nSAVE = CPROBABILITIES;")))

# Name each input file
get_name <- function(i) {
    str_glue("{i$model}_{i$y}_{i$classes}_{i$form}")
}
names(inputs1) <- map_chr(comb1, get_name)

# Delete old input files, if they exist
target <- here("analysis", "mplus", "input_files", "gmm")
file_delete(dir_ls(target))

# Save input files (and delete old version if exists)
write_models  <- function(inputs, target) {
    walk2(inputs, names(inputs), function(inp, lab) {
         fp = paste0(target, "/", lab, ".inp")
         writeLines(inp, paste0(target, "/", lab, ".inp"))
         cat(paste0("Saved file: ", lab, ".inp", "\n"))
    })
}

write_models(inputs1, target)

###############################################################################
####                                                                      #####
####     2. Generate input files for predictors of class membership       #####
####                                                                      #####
###############################################################################


# Pick base models ------------------------------------------------------------
pick <- comb1 %>%
    # Pick base models
    keep(~ (.x$y == "gad" & .x$classes == 5) | 
           (.x$y == "phq" & .x$classes == 5)) 
names(pick) <- c("gad", "phq")

# Define sets of covariates ---------------------------------------------------
unadj <- list(role      = "is_staff",
              ethnicity = c("eth_mixed", "eth_asian", "eth_black", "eth_other"),
              yngchild  = "child6",
              anychild  = "anychild",
              care      = "othercare",
              shield    = "shield_isol",
              kw        = "kw",
              livalon   = "livalon",
              rent      = "renting",
              pranx     = "pranx",
              prdep     = "prdep")
adj <- map(unadj, ~ c("age", "female", .x)) 
names(adj) <- paste0(names(adj), "_adj")
opts <- vec_c(unadj, adj)
opts$age <- "age"
opts$sex <- "female"
opts$agesex <- c("age", "female")

# Create combinations of models/covariates ------------------------------------
comb2 <- cross(list(mod = pick, r3step = opts))
names(comb2) <- cross(list(names(pick), names(opts))) %>%
    map_chr(~ paste0(.x[[1]], "_", .x[[2]]))

inputs2 <- comb2 %>%
    map(~ flatten(list_merge(.x[1], .x[2]))) %>%
    map(~ exec(make_input, !!!.x))

# Delete old files, if they exist
target <- here("analysis", "mplus", "input_files", "r3step")
file_delete(dir_ls(target))

# Write input files
write_models(inputs2, target)

###############################################################################
####                                                                      #####
####          3. Generate input files for time-varying predictors         #####
####                                                                      #####
###############################################################################

# Check: how many of the TVCs have zero variance?
rep <- sel %>% 
    ungroup() %>%
    select(dap, pid,
           phq_total, gad_total,
           had_v1, had_v2,
           fur_cu, fur_ev,
           probdef,
           starts_with("s_")) %>%
    group_by(dap) 

rep %>%
    summarise(across(everything(), ~ var(.x, na.rm = TRUE) > 0)) %>%
    print(n = 50)

# Pick base models ------------------------------------------------------------
pick <- comb1 %>%
    # Pick base models
    keep(~ (.x$y == "gad" & .x$classes == 5) | 
           (.x$y == "phq" & .x$classes == 5)) 
names(pick) <- c("gad", "phq")

# Add TVCs --------------------------------------------------------------------
non_zero <- function(i) {
    wide_data %>%
        select(starts_with(i)) %>%
        mutate(across(everything(), na_if, 777)) %>%
        summarise(across(everything(), ~ var(.x, na.rm = TRUE) > 0)) %>%
        gather(k, v) %>%
        filter(v) %>%
        pluck("k")
}
tvcs <- map(stubs, non_zero)
tvcs <- cross(list(tvc = tvcs, constrain = c(TRUE, FALSE))) %>%
    map(~ list(use = TRUE, vars = .x$tvc, constrain = .x$constrain))
comb3 <- cross(list(mod = pick, tvc = tvcs))

inputs3 <- comb3 %>%
    map(~ flatten(list_merge(.x[1], .x[2]))) %>%
    map(~ exec(make_input, !!!.x))

names(inputs3) <- map_chr(comb3,
                          ~ paste(.x$mod$model,
                                  .x$mod$y,
                                  .x$mod$classes,
                                  str_extract(.x$tvc$vars[[1]], "^[a-z]+"),
                                  if_else(.x$tvc$constrain,
                                          "constrained",
                                          "free"),
                                  sep = "_"))

# Delete old files, if they exist
target <- here("analysis", "mplus", "input_files", "tvcov")
file_delete(dir_ls(target))

# Write input files
write_models(inputs3, target)

###############################################################################
####                                                                      #####
####                            Generate index                            #####
####                                                                      #####
###############################################################################

inp <- dir_ls(here("analysis", "mplus", "input_files"),
              recurse = TRUE,
              glob = "*.inp") %>%
    path_rel(here("analysis", "mplus", "input_files")) %>%
    path_split() %>%
    map_chr(~ paste(.x[1], .x[2]))

writeLines(inp, here("analysis", "mplus", "data", "index"))
