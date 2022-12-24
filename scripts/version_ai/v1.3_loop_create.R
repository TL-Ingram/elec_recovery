#####
# Load packages
shelf(tidyverse, here, lubridate, forecast, fpp3, hrbrthemes)


# ------------------------------------------------------------------------------
#####
# Load data
data <- read_csv(here("data", "hist_wl.csv"))
getwd()

# Source scripts
source(here("scripts", "version_ai", "sourced-wl_cleaning.R"))

# ------------------------------------------------------------------------------
#####
# Time period models trained on
train_init = "2022-09-01"
train_halt = "2022-12-14" # eventually change this to sys.date - 1
h = 365
speciality_name = "Gastroenterology"

# Pull speciality names into vector for looping through
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)


# ------------------------------------------------------------------------------
##### loop to output graphs for each speciality
# for (speciality_name in speciality)
# need to do this by both the dfs and all specs. So function better than loop?
    
# ------------------------------------------------------------------------------
#####
# Filter to init date, filling date gaps and imputing missing wl size
wl_prepared <- o_wl %>%
  filter(., spec_desc == speciality,
         date > train_init) %>%
  as_tsibble(.) %>%
  fill_gaps(., date) %>%
  fill(., patients, .direction = "up") %>%
  ungroup(.)

# Filter to halt date
train_set <- wl_prepared %>%
  filter(., date < train_halt) %>%
  select(., date, patients)


# ------------------------------------------------------------------------------
##### 
# Build forecasting models to test
STLF <- decomposition_model(
  STL((patients) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)
model_frame <- train_set %>%
  fill_gaps(patients = mean(patients)) %>%
  fabletools::model(
    ets = ETS(patients, trace = T),
    stlf = STLF,
    arima = ARIMA(patients, stepwise = F, approximation = F, trace = T),
    nnar = NNETAR(patients, stepwise = F, trace = T)
  ) %>%
  mutate(combination = (ets + stlf + arima + nnar)/4)


# ------------------------------------------------------------------------------
#####
# Generate future sample paths
sim_paths <- model_frame %>%
  generate(h = h, times = 25)

# Compute forecast distributions from future sample paths and create fable object
sim_results <- sim_paths %>%
  as_tibble() %>%
  group_by(date, .model) %>%
  summarise(dist = distributional::dist_sample(list(.sim))) %>%
  ungroup() %>%
  as_fable(index=date, key=.model, distribution=dist, response="patients")

# Plot results over-laid on wl and filter for combination model
sim_results %>%
  filter(.model == "combination") %>%
  autoplot(wl_52, level = 80, size = 1, alpha = 0.5) +
  ylim(0,100)
ggsave(here("plots", "combi_plots", paste0(speciality_name, "_wl_h365.jpg")), width = 10, height = 6, dpi = 600)

        