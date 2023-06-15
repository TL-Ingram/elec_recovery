#####
# Load packages
suppressWarnings(shelf(tidyverse, here, lubridate, forecast, 
                       fpp3, hrbrthemes, odbc, glue, padr))


# ------------------------------------------------------------------------------
#####
# Check date - if start of month pull latest data from Qlik
if (day(Sys.Date()) <= 3) {
  glue("Ensure an updated hist_wl.csv copy has been loaded")
}


# ------------------------------------------------------------------------------
#####
# Load historic waiting list and clean
{
  data <- suppressWarnings(read_csv(here("data", "raw.csv"), 
                                    show_col_types = F))
  source(here("scripts", "current", "sourced_scripts", 
              "sourced-v2.0-wl_cleaning.R"))
  glue("Historic inpatient waiting lists loaded")
}


# ------------------------------------------------------------------------------
#####
# Time period models trained on
{
  train_halt = (date(max(wl_comp$date)) - 10)
  train_init = date(train_halt - 180)
  train_period_label = "Training period"
  param_start = date(train_halt - 45)
  h = 365
  # speciality = c("Trauma & Orthopaedics")
  # wl_type = c("Inpatient_wl")
}


# ------------------------------------------------------------------------------
#####
# Load parameters and clean
{
  if (day(Sys.Date()) == 1) {
  source(here("scripts", "current", "sourced_scripts", 
              "sourced-v2.2-parameter_est.R"))
} else {
  parameters <- read_rds(here("rds", "parameters", "all_spec.rds"))
  glue("Speciality specific parameters loaded")
}
}


# ------------------------------------------------------------------------------
#####
# Train, create combination forecast, bias to latest month, output
{
list_paths <- list()
list_wl <- list()
}

for(i in wl_type) {
  wl_prepared <- wl_comp %>%
    filter(., wl == i,
           date < train_halt)
  # Filter to speciality
  for (j in speciality) {
    wl_prep <- wl_prepared %>%
      mutate(date = ymd(date)) %>%
      filter(., speciality == j)
    
    # Write historic paths to list
    list_wl[[paste0(i, "_", j)]] <- wl_prep
    wl_keys <- bind_rows(list_wl)
    
    
    # Filter to init date, filling date gaps and imputing missing wl size
    print(glue("Waiting list: {i} \nSpeciality: {j}"))
    
    wl_ready <- wl_prep %>%
      filter(date > train_init) %>%
      as_tsibble(., index = "date") %>%
      fill_gaps(., date) %>%
      fill(., patients, .direction = "up") %>%
      ungroup(.)
    
    
    
    # Script continuance test
    if(dim(wl_ready)[1] >= 5) {
      print(glue("Building models and running simulations..."))
      
      
      
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
          ets = ETS(patients, trace = F),
          stlf = STLF,
          arima = ARIMA(patients, stepwise = F, approximation = F, trace = F),
          nnar = NNETAR(patients, stepwise = F, trace = F)
        ) %>%
        mutate(combination = (ets + stlf + arima + nnar)/4) %>%
        mutate(paramatised = (ets + stlf + arima + nnar)/4)
      
      # Generate future sample paths
      sim_paths <- model_frame %>%
        generate(h = h, times = 25)
      
      # Calculate specialities overall parameter position
      param_filter <- wl_param %>%
        filter(., list == i,
               speciality == j) %>%
        pivot_wider(-c(speciality, time_period, list), 
                    names_from = "metric", values_from = "month_mean")
      
      position = round((((param_filter$demand) - 
                           (param_filter$capacity) - 
                           (param_filter$rott))), 
                       digits = 2)
      print(glue("{position*10}% weighting applied to model"))
      
      # Create vector of weights to apply to combination_2 model
      
      # MAKE IT 0.3* for non-long waiters, 0.1 for lw
      # No instead I want to make it such that it weights it dependent upon the n. So if big list size then higher number (up to 1), small list size = lower number (0.05?)
      weights_vector <- c(0)
      for (v in seq_along(1:h)) {
        weights_vector <- c(weights_vector, 
                            ((position*(0.1*exp(-v/h))) + weights_vector[v-1]))
      }
      weights <- data.frame(
        horizon = 1:h,
        weights_vector)
      plot(weights)
      # have it print the first four numbers as a quick sanity check?
      
      # Filter to only combi model; "combination" is AI forecast only
      combi_path <- sim_paths %>%
        filter(.model == c("combination"))
      
      # Combine AI predictions with weights_vector to produce "paramatised"
      all_path <- sim_paths %>%
        filter(.model == c("paramatised")) %>%
        transform(., horizon=match(date, unique(date))) %>%
        group_by(horizon) %>%
        left_join(., y = weights, by = "horizon") %>%
        mutate(.sim = (.sim + weights_vector)) %>%
        ungroup(.) %>%
        select(-(c(weights_vector, horizon))) %>%
        bind_rows(combi_path) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
        mutate(wl = i,
               spec_desc = j) %>%
        as_tibble(.)
      

      if(dim(all_path)[1] != 0) {
      
        
        # Write forecast paths to list
        list_paths[[paste0(i, "_", j)]] <- all_path
        path_keys <- bind_rows(list_paths)
        print(glue("Model built"))
        
        #Plot and save speciality-specific forecasts
        source(here("scripts", "current", "sourced_scripts", 
                    "sourced-spec_forecasts.R"))
        spec_forecast
      }
    }
  }
}


# ------------------------------------------------------------------------------
#####
# Write rds for speedy testing
{
write_rds(wl_keys, here("rds", "keys", "wl_keys.rds"))
write_rds(path_keys, here("rds", "keys", "path_keys.rds"))
}

# # Read rds for speedy testing
# {
# wl_keys <- read_rds(here("rds", "keys", "wl_keys.rds"))
# path_keys <- read_rds(here("rds", "keys", "path_keys.rds"))
# }


# ------------------------------------------------------------------------------
# Join all specialities into one list and plot
{
source(here("scripts", "current", "sourced_scripts",
            "sourced-overall_forecast.R"))

# Print plot
print(plot_o)
}


# ------------------------------------------------------------------------------
# Long waiters clearance times
{
source(here("scripts", "current", "sourced_scripts", 
            "sourced-clearance_table.R"))
# Print long waiter tables
print(lw_table)

# Print long waiters plot
print(plot_lw)
}

