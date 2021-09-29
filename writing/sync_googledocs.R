# Title:        Archive latest draft from Google Docs
# Author:       Ewan Carr
# Started:      2021-06-22

library(tidyverse)
library(here)
library(googledrive)

required <- list(manuscript = "https://docs.google.com/document/d/1mtw4auX6eoUx9x2yIRnQD_PClwi9UP1-l87gXnpAxUs",
                 supplementary_materials = "https://docs.google.com/document/d/1FJ3wt83vQvQjNHqgvlp6k5-dlW6TZCbNOmFdCQr-WKw")

walk2(required, names(required),
      function(url, folder) {
        target <- here("writing", "drafts", folder, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"), ".docx"))
        drive_download(file = as_id(url), path = target)
      })
