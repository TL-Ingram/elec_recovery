#####
# Time period models trained on
train_init = "2022-06-01"
train_halt = "2022-11-01"


--------------------------------------------------------------------------------
#####
# Filter to init date, filling date gaps and imputing missing wl size
wl_prepared <- wl %>%
  filter(., date > train_init) %>%
  as_tsibble(.) %>%
  fill_gaps(., date) %>%
  fill(., speciality, .direction = "up") %>%
  group_by(., speciality) %>%
  fill(., patients, .direction = "up") %>%
  ungroup(.)

# Filter to halt date
train_set <- wl_prepared %>%
  filter(., date < train_halt) %>%
  select(., date, patients)

# horizon (days)
h <- nrow(wl_prepared) - nrow(train_set)


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
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
  #filter(.model == "combination") %>%
  autoplot(wl, level = 80, size = 1, alpha = 0.5)

# Check accuracy of 95% prediction intervals
sim_results %>% accuracy(wl, measures = interval_accuracy_measures, level=95)
        