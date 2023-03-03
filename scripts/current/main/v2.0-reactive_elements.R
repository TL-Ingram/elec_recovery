#####
# Load packages
suppressWarnings(shelf(tidyverse, here, lubridate, forecast, 
                       fpp3, hrbrthemes, odbc, glue))


# ------------------------------------------------------------------------------
#####
# Check date - if start of month pull latest data from Qlik
if (day(Sys.Date()) == 1 | 2 | 3) {
  glue("Ensure an updated hist_wl.csv copy has been loaded")
}


# ------------------------------------------------------------------------------
#####
# Load historic waiting list and clean
{
  data <- read_csv(here("data", "hist_wl.csv"), show_col_types = F)
  source(here("scripts", "current", "sourced_scripts", "sourced-wl_cleaning.R"))
  glue("Historic inpatient waiting lists loaded")
}

# Load parameters and clean
{
  if (day(Sys.Date()) == 1) {
  source(here("scripts", "current", "sourced_scripts", "v2.0-parameter_est.R"))
} else {
  parameters <- read_rds(here("rds", "parameters", "all_spec.rds"))
  glue("Speciality specific parameters loaded")
}
}


# ------------------------------------------------------------------------------
# Time period models trained on
train_halt = date(max(wl_comp$date))
train_init = date(train_halt - 180)
train_period_label = "Training period"
param_start = date(train_halt - 30)
h = 100


# ------------------------------------------------------------------------------
#####
# Produce forecast outputs, csv + plot, for each speciality
list_paths <- list()
list_wl <- list()
for(i in wl_type) {
  wl_prepared <- wl_comp %>%
    filter(., wl == i)
  # Filter to speciality
  for (j in speciality) {
    wl_prep <- wl_prepared %>%
      filter(., spec_desc == j)
    
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
        generate(h = h, times = 50)
      
      # Calculate specialities overall parameter position
      param_filter <- parameters %>%
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
      weights_vector <- c(0)
      for (v in seq_along(1:h)) {
        weights_vector <- c(weights_vector, 
                            ((position*(0.3*exp(-v/h))) + weights_vector[v-1]))
      }
      weights <- data.frame(
        horizon = 1:h,
        weights_vector)
      plot(weights)
      # have it print the first four numbers as a quick sanity check?
      
      # Filter to only combi models. "combination" is only AI forecast
      combi_path <- sim_paths %>%
        filter(.model == c("combination")) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim))
      
      # Combine AI predictions with weights_vector to produce "paramatised"
      all_path <- sim_paths %>%
        filter(.model == c("paramatised")) %>%
        transform(., horizon=match(date, unique(date))) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
        group_by(horizon) %>%
        left_join(., y = weights, by = "horizon") %>%
        mutate(.sim = (.sim + weights_vector)) %>%
        ungroup(.) %>%
        select(-(c(weights_vector, horizon))) %>%
        bind_rows(combi_path) %>%
        mutate(wl = i,
               spec_desc = j) %>%
        as_tibble(.)
      
      
      if(dim(all_path)[1] != 0) {
      
        
        # Write forecast paths to list
        list_paths[[paste0(i, "_", j)]] <- all_path
        path_keys <- bind_rows(list_paths)
        print(glue("Model built"))
        
        sim_results <- all_path %>%
          as_tibble(., index = date) %>%
          mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
          group_by(.model, date, spec_desc) %>%
          summarise(dist = distributional::dist_sample(list(.sim)), 
                    .groups = "drop_last") %>%
          ungroup(.) %>%
          as_fable(index = date, key = .model, distribution = dist, 
                   response="patients")

        sim_results %>%
          autoplot(data = wl_prep, level = 80, size = 0.6, alpha = 0.7) +
          geom_line(data = wl_prep, aes(x = date, y = patients), size = 0.6,
                    alpha = 0.7, colour = "grey50") +
          geom_vline(data = wl_prep, xintercept = train_halt,
                     linetype = "dashed", colour = "grey50",
                     size = 0.5, alpha = 0.8) +
          geom_vline(data = wl_prep, xintercept = train_init,
                     linetype = "dashed", colour = "grey50",
                     size = 0.5, alpha = 0.8) +
          geom_vline(data = wl_prep, xintercept = param_start,
                     linetype = "dashed", colour = "grey50",
                     size = 0.5, alpha = 0.8) +
          annotate("rect", xmin = train_init, xmax = train_halt, 
                   ymin = -Inf, ymax = Inf,
                   alpha = .1, fill = "grey75") +
          geom_text(data = wl_prep, aes(x = (train_init + 90), 
                                        y = Inf), 
                    label = train_period_label,
                    vjust = 1.5, size = 2.5, colour = "grey40") +
          scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
          scale_colour_discrete(guide = "none") +
          plot_defaults +
          labs(fill = "",
               x = "",
               y = "Patients",
               title = glue("{j} - {i} list"),
               level = "",
               subtitle = glue("Forecast horizon begins from {train_halt} 
                               and extends for {h} days"),
               caption = glue("AI Training period is from {train_init}
                              to {train_halt}
                              Parameter weighting estimated from {param_start}
                              to {train_halt}
                              Blue line depicts mean predicted patient number
                              Shaded region depicts 80% prediction interval"))
        
        # Save plot
        file_name <- glue("{i}_{j}_1")
        ggsave(here("plots", "current", "speciality_forecasts", j, 
                    filename=paste0(file_name, ".png")), device = "png")
      }
    }
  }
}

      
      
      

