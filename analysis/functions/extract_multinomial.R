library(tidyverse)
library(here)

extract_r3step <- function(p) {

    # Load the output file

    entire_file <- readLines(p)

    # Check if the file contains the required section (i.e. multinomial 
    # regression).  If it doesn't, return an error, because something has 
    # probably gone wrong.

    starting_string <- "TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING"
    ending_string <- "ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS"
    has_start <- any(grepl(starting_string, entire_file))
    has_end <- any(grepl(ending_string, entire_file))
    if (!(all(has_start, has_end))) {
        warning("Multinomial results not found; check output file.")
        return(NA)
    }

    # Extract the relevant section from the output file

    start <- grep(starting_string, entire_file)
    end <- grep(ending_string, entire_file)
    results <- entire_file[(start):(end - 1)]

    # Extract each "Parameterization using..." section

    pos <- c(1,
             grep("^Parameterization using Reference Class.*$", results),
             length(results))
    results <- split(results, rep(seq(pos), diff(c(pos, length(results) + 1))))
    results <- results[-c(1, length(results))]
    names(results) <- map_dbl(results, ~ parse_number(.x[1]))

    # Convert strings into a tidy data frame

    return(map_dfr(results, function(x) {
        as.data.frame(x[-1]) %>%
            set_names("row") %>%
            mutate(class = str_extract(row, "^ *C#\\d+"),
                   to_drop = row == "" | !is.na(class)) %>%
            fill(class, .direction = "down") %>%
            filter(!to_drop) %>%
            separate(row,
                     letters[1:6],
                     sep = "[ ]+",
                     extra = "drop",
                     fill = "right") %>%
            drop_na(c) %>%
            select(param = b, on = class, est = c, se = d, pval = f)
             }, .id = "reference") %>%
        mutate(across(est:pval, as.numeric),
               or = exp(est)))
}
