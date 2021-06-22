library(tidyverse)
library(here)

extract_r3step <- function(p) {

    # Load the output file ----------------------------------------------------

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

    # Select required sections ------------------------------------------------
    # 1: Odds ratios and p-values              NOTE: Not using this at present.
    start <- grep(starting_string, entire_file)
    end <- grep(ending_string, entire_file)
    A <- entire_file[(start + 5):(end - 1)]

    # 2: Confidence intervals
    start <- grep("CONFIDENCE INTERVALS OF ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL",
                  entire_file)
    end <- min(grep("TECHNICAL 1 OUTPUT|SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", entire_file))
    ci <- entire_file[(start + 2):(end - 1)]

    # Extract confidence intervals
    pos <- c(1,
             grep("^Parameterization using Reference Class.*$", ci),
             length(ci))
    ci <- split(ci,
                rep(seq(pos),
                    diff(c(pos, length(ci) + 1))))  %>%
         discard(~ length(.x) < 3) %>%
         map(~ .x[2:length(.x)])

    ci <- map_dfr(ci, function(i) {
                  i <- i[i != ""]
                  i <- data.frame(i) %>%
                     mutate(class = str_extract(i, "^ *C#\\d+")) %>%
                     fill(class, .direction = "down") %>%
                     separate(i, 
                              c("drop", "param", "low005", "low025", "low05",
                                "est", "up05", "up025", "up005"),
                              sep = " +",
                              fill = "right") %>%
                     select(-drop) %>%
                     drop_na(est) 
                 class <- unique(parse_number(i$class))
                 i$ref <- paste0("C#", setdiff(1:(length(class) + 1), class))
                 i$param <- tolower(i$param)
                 return(i)
                     }
    )

    ci <- ci %>%
        mutate(across(everything(),
                      ~ if_else(str_detect(.x, "^\\*\\*\\*\\*+"),
                                NA_character_, .x)))

    return(ci)
}

    # Convert strings into a tidy data frame

    # return(map_dfr(results, function(x) {
    #     as.data.frame(x[-1]) %>%
    #         set_names("row") %>%
    #         mutate(class = str_extract(row, "^ *C#\\d+"),
    #                to_drop = row == "" | !is.na(class)) %>%
    #         fill(class, .direction = "down") %>%
    #         filter(!to_drop) %>%
    #         separate(row,
    #                  letters[1:6],
    #                  sep = "[ ]+",
    #                  extra = "drop",
    #                  fill = "right") %>%
    #         drop_na(c) %>%
    #         select(param = b, on = class, est = c, se = d, pval = f)
    #          }, .id = "reference") %>%
    #     mutate(across(est:pval, as.numeric),
    #            or = exp(est)))

