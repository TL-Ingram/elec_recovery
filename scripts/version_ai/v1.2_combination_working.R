testing <- wl_2  %>%
  fill_gaps() %>%
  fill(speciality, .direction = "up") %>%
  group_by(speciality) %>%
  fill(patients, .direction = "up")


train <- wl_2 %>%
  filter(month(date) > 6) %>%
  select(date, patients)

STLF <- decomposition_model(
  STL((patients) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)
cafe_models <- train %>%
  fill_gaps(patients = mean(patients)) %>%
  fabletools::model(
    ets = ETS(patients, trace = T),
    stlf = STLF,
    arima = ARIMA(patients, stepwise = F, approximation = F, trace = T),
    nnar = NNETAR(patients, stepwise = F, trace = T)
  ) %>%
  mutate(combination = (ets + stlf + arima + nnar)/4)

# Generate future sample paths
cafe_futures <- cafe_models %>%
  generate(h = 300, times = 25)

# Compute forecast distributions from future sample paths and create fable object
cafe_fc2 <- cafe_futures %>%
  as_tibble() %>%
  group_by(date, .model) %>%
  summarise(dist = distributional::dist_sample(list(.sim))) %>%
  ungroup() %>%
  as_fable(index=date, key=.model, distribution=dist, response="patients")


cafe_fc2 %>%
  #filter(.model == "combination") %>%
  autoplot(wl_2, level = 80, size = 1, alpha = 0.5)

# Check accuracy of 95% prediction intervals
cafe_fc2 %>% accuracy(wl_2, measures = interval_accuracy_measures, level=95)
         