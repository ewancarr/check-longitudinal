# Title:        Prepare inputs for Mplus on Rosalind
# Author:       Ewan Carr
# Started:      2021-04-21

library(tidyverse)
library(here)
library(fs)
library(MplusAutomation)
library(fastDummies)
library(naniar)
load(here("data", "clean", "rep.Rdata"), verbose = TRUE)
load(here("data", "clean", "baseline.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                        Prepare data for Mplus                        #####
####                                                                      #####
###############################################################################

# # Decide which time points to use -------------------------------------------

# Either all times points or 2-monthly questionnaires only.

two_monthly <- FALSE
if (two_monthly) {
    sel <- filter(sel, two_month)
}

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

# Add baseline variables ------------------------------------------------------

wide_data <- bl %>%
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
           probdef,
           kwself01,
           livalon,
           renting) %>%
    mutate(eth = tolower(as.character(ethnic_f)),
           othercare = othercare == "Yes") %>%
    drop_na() %>%
    dummy_cols(select_columns = c("eth", "nc")) %>%
    select(-eth, -ethnic_f, -nc) %>%
    full_join(wide_data, by = "pid")

# Remove characters Mplus doesn't like
names(wide_data) <- str_replace_all(names(wide_data), "\\+", "")

# Check: missing data?
wide_data %>%
    summarise(across(everything(), pct_complete)) %>%
    gather(measure, pct_complete) %>%
    as.data.frame()

# Create LONG version ---------------------------------------------------------

long_data <- select(aw, phq = phq_total, gad = gad_total, pid, dap)

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
####              create input files: GROWTH MIXTURE MODELS               #####
####                                                                      #####
###############################################################################

"
We're considering two types of model:

1. Latent class growth analysis (LCGA)
2. Growth mixture models

The former is a simplified version of GMM, where within-class variations
(variances and covariances) are constrained to zero.

And two functional forms:

1. Quadratic
2. Cubic

However, for now, I'm just fitting GMM/cubic.
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
                       r3step = FALSE) {
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
    # Generate the model
    return(str_glue("
    TITLE: {title}
    DATA: FILE = {dpath};
    VARIABLE: 
    {vn} 
    USEVARIABLES = {uv};
    CLASSES = C({classes});
    IDVARIABLE = pid;
    {r3step}
    MISSING=.;
    ANALYSIS: {pr}
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

combinations <- list(dpath           = "../data/check_wide.dat",
                     y               = c("gad", "phq"),
                     form            = "cubic",
                     names_statement = input_file$wide[4],
                     classes         = 2:10,
                     starts          = FALSE,
                     proc            = 24,
                     boot            = FALSE,
                     model           = "gmm") %>%
    cross()

inputs <- map(combinations, ~ exec(make_input, !!!.x))

# Add "SAVEDATA" statement
inputs <- map2(inputs, 1:length(inputs),
               ~ paste0(.x, str_glue("\n\nSAVEDATA: \nFILE = save{.y}.dat;\nSAVE = CPROBABILITIES;")))


# Save input files (and delete old version if exists)
write_models  <- function(inputs, stub = "", target) {
    walk2(inputs, 1:length(inputs), function(m, f) {
         fp <- paste0(target, "/", stub, f)
         if (file_exists(fp)) {
             file_delete(fp)
         }
         writeLines(m, fp)
       })
}

write_models(inputs,
             target = here("analysis", "rosalind", "local"))

# Save index
index <- reduce(combinations, bind_rows)
index$model_id <- 1:length(combinations)
save(index, file = here("analysis", "rosalind", "index.Rdata"))

###############################################################################
####                                                                      #####
####          create input files: PREDICTORS OF CLASS MEMBERSHIP          #####
####                                                                      #####
###############################################################################

# Pick base models ------------------------------------------------------------
pick <- combinations %>%
    # Pick base models
    keep(~ (.x$y == "gad" & .x$classes == 6) | 
           (.x$y == "phq" & .x$classes == 5)) 

# Define sets of covariates ---------------------------------------------------
to_include <- list(a = "age", 
                   b = "female",
                   c = "child6",
                   d = "livalon",
                   e = "renting",
                   f = "kwself01")
to_include$g <- reduce(to_include, c) 

# Create combinations of models/covariates ------------------------------------

combinations <- cross(list(mod = pick, r3step = to_include))

inputs2 <- combinations %>%
    map(~ flatten(list_merge(.x[1], .x[2]))) %>%
    map(~ exec(make_input, !!!.x))

write_models(inputs2,
             stub = "r3step",
             target = here("analysis", "rosalind", "local"))

# Save index
index2 <- map_dfr(combinations, ~ .x$mod, .id = "model_id") %>%
    select(model_id, y, classes)
save(index2, file = here("analysis", "rosalind", "index2.Rdata"))
