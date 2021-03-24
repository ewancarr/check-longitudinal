.PHONEY: data

# Data analysis ---------------------------------------------------------------

data: analysis/cleaning/*.R
	Rscript "analysis/cleaning/import_excel.R"
	Rscript "analysis/cleaning/cleaning.R"

# Writing ---------------------------------------------------------------------
# TODO
