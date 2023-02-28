#####
# Load packages
shelf(tidyverse, here, lubridate, forecast, fpp3, hrbrthemes, odbc)


# ------------------------------------------------------------------------------
#####
# Load data
data <- read_csv(here("data", "hist_wl.csv"))

# Pull speciality names into vector for looping through
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)

# Source scripts
# Run line 19 once per month only
source(here("scripts", "version_ai", "sourced-wl_cleaning.R"))
# source(here("scripts", "NIU-parameter_estimation.R"))
source(here("scripts", "version_ai", "parameters", "spec_params.R"))

# Pull type of waiting list into vector for looping through
wl_type <- wl_comp %>%
  distinct(wl) %>%
  filter(., wl != c("Deferred", "Unknown")) %>%
  pull(wl)

