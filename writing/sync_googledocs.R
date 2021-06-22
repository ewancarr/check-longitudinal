# Title:        Archive latest draft from Google Docs
# Author:       Ewan Carr
# Started:      2021-06-22

library(tidyverse)
library(here)
library(googledrive)

filename <- paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"), ".docx")
target <- here("writing", "drafts", filename)
docs_id <- "https://docs.google.com/document/d/1mtw4auX6eoUx9x2yIRnQD_PClwi9UP1-l87gXnpAxUs"
drive_download(file = as_id(docs_id),
               path = target)
