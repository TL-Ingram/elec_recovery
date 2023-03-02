#####
# Load packages
suppressWarnings(shelf(tidyverse, here, lubridate, forecast, 
                       fpp3, hrbrthemes, odbc))


# ------------------------------------------------------------------------------
#####
# Check date - if start of month pull latest data from Qlik
if (day(Sys.Date()) == 1 | 2 | 3) {
  writeLines("Ensure an updated hist_wl.csv copy has been loaded")
}


# ------------------------------------------------------------------------------
#####
# Load historic waiting list and clean
{
  data <- read_csv(here("data", "hist_wl.csv"))
  source(here("scripts", "version_ai", "sourced-wl_cleaning.R"))
}

# Load parameters and clean
{
  if (day(Sys.Date()) != 1) {
  source(here("scripts", "version_ai", "v2.0-parameter_est.R"))
} else {
  parameters <- read_rds(here("rds", "parameters", "all_spec.rds"))
}
}

#