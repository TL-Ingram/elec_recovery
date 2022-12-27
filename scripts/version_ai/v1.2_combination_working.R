#####
# Load packages
shelf(tidyverse, here, lubridate, forecast, fpp3, hrbrthemes, scales)


#--------------------------------------------------------------------------------
#####
# Load data
data <- read_csv(here("data", "hist_wl.csv"))


#--------------------------------------------------------------------------------
#####
# Time period models trained on
train_init = date("2022-06-01")
train_halt = date("2022-11-01")
# speciality_name = "Gastroenterology"

# Pull speciality names into vector for looping through
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)

##### loop to output graphs for each speciality
wl <- data %>%
  filter(., spec_desc == "Trauma & Orthopaedics",
           !(covid_recovery_priority == "Unknown" 
             | covid_recovery_priority == "Deferred"
             | covid_recovery_priority == "Planned")) %>%
    mutate(date = dmy(date)) %>%
    group_by(date) %>%
    summarise(patients = n()) #%>%
    # filter(date > "2022-01-01")
  
  wl_52 <- data %>%
      filter(., spec_desc == speciality_name,
             wm52 == 1,
             !(covid_recovery_priority == "Unknown" 
               | covid_recovery_priority == "Deferred"
               | covid_recovery_priority == "Planned")) %>%
      mutate(date = dmy(date)) %>%
      group_by(date, wm52) %>%
      summarise(wm52 = n()) %>%
      ungroup(.) %>%
      left_join(parameters_test, by = "date") #%>%
      # filter(date > "2022-01-01")
    
#--------------------------------------------------------------------------------
#####
# Filter to init date, filling date gaps and imputing missing wl size
wl_prepared <- wl %>%
  filter(., date > train_init) %>%
  as_tsibble(.) %>%
  fill_gaps(., date)

# Filter to halt date
train_set <- wl_prepared %>%
  filter(., date < train_halt) %>%
  select(., date, patients)

# horizon (days)
h <- nrow(wl_prepared) - nrow(train_set)


#--------------------------------------------------------------------------------
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
  mutate(combination = (arima + nnar)/2)


#--------------------------------------------------------------------------------
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
title = j
j = "Trauma & Orthopaedics"

sim_results %>%
  filter(.model == "combination") %>%
  autoplot(wl, level = 80, size = 0.6, alpha = 0.9) +
  geom_line(data = wl, aes(x = date, y = patients), size = 0.6,
            alpha = 0.7, colour = "grey50") +
  geom_text(data = wl, aes(x = train_init, y = Inf, label = train_init), vjust = 1.5 + 0.1, hjust = -0.25, size = 4, type = "serif") +
  plot_defaults +
  labs(fill = "",
       x = "",
       y = "Patients",
       title = j,
       level = "",
       subtitle = paste0("Forecast horizon begins from ",
                         train_halt, " and extends for ", h, " days"),
       caption = "Blue line depicts mean predicted patient number
                    Shaded region depicts 80% prediction interval")
  
file_name <- paste0(i, "_", j)
ggsave(here("plots", "combi_plots", filename=paste0("test.png")), 
  device = "png")
# Check accuracy of 95% prediction intervals
sim_results %>% accuracy(wl, measures = interval_accuracy_measures, level=95)
        