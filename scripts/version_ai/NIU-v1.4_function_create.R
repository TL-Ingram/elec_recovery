#####
# Load packages
shelf(tidyverse, here, lubridate, forecast, fpp3, hrbrthemes)


# ------------------------------------------------------------------------------
#####
# Load data
data <- read_csv(here("data", "hist_wl.csv"))

# Pull speciality names into vector for looping through
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)

# Source scripts
source(here("scripts", "version_ai", "sourced-wl_cleaning.R"))

# Pull type of waiting list into vector for looping through
wl_type <- wl_comp %>%
  distinct(wl) %>%
  pull(wl)

# ------------------------------------------------------------------------------
#####
# Time period models trained on
train_init = "2022-09-01"
train_halt = "2022-12-14" # eventually change this to sys.date - 1
h = 365


# ------------------------------------------------------------------------------
#####
# Filter to init date, filling date gaps and imputing missing wl size
looping_test <- function(a, b){
  for(i in a) {
    wl_prepared <- wl_comp %>%
      filter(., wl == i)
    for (j in b) {
      wl_prep <- wl_prepared %>%
        filter(., spec_desc == j)
      wl_ready <- wl_prep %>%
        filter(date > train_init) %>%
        as_tsibble(.) %>%
        fill_gaps(., date) %>%
        fill(., patients, .direction = "up") %>%
        ungroup(.)
      
      # Filter to halt date
      train_set <- wl_ready %>%
        filter(., date < train_halt) %>%
        select(., date, patients)
      
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
      results <- sim_results %>%
        filter(.model == "combination") %>%
        autoplot(wl_prep, level = 80, size = 1, alpha = 0.5)
      
      file_name <- paste0(i, "_", j)
      ggsave(here("plots", "combi_plots", filename=paste0(file_name, ".png")), device = "png")
    }
      #write_csv(train_set, here("test", paste0(i,j,"test.csv")))
  }
}

looping_test(wl_type, speciality)        
