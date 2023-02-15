#####
# Dynamic regression model

#####
# Plot graphs of each predictor ------------------------------------------------
# write_rds(parameters_test, here("csv", "parameters", "gastro.rds"))
parameters_xreg = parameters_test
parameters_gastro <- pivot_longer(parameters_test, cols = c("adms", "dtas", "rott"), 
                                  names_to = "names", values_to = "values") %>%
  filter(date < yesterday) %>%
  mutate(across(names, str_replace, c("adms", "dtas", "rott"), c("removals", "additions", "error"))) %>%
  mutate(month = floor_date(date, unit = "month"))

param_mean <- parameters_gastro %>%
  group_by(month, names) %>%
  summarise(mean = mean(values)) %>%
  ungroup(.)
param_mean %>%
  ggplot(aes(x = month, y = mean, colour = names)) +
  geom_line() +
  theme_bw()
  
# Rates
param_now <- param_mean %>%
  filter(month == max(month)) %>%
  pivot_wider(-month, names_from = "names", values_from = "mean")

x = 1
y = 1
z = 1

formula = (x*param_now$additions) - (y*param_now$removals) - (z*param_now$error)

# normalise formula to 1 as a baseline position, then work out 

# additions - removals - error
# (add*x) - (rem*y) - (error*z) = 


#####
# Plot waiting list ------------------------------------------------------------
wl_keys %>%
    ggplot(aes(x = date, y = patients)) +
    geom_line() +
    theme_bw()


# Bring in usual forecast
test_knitted <- knitted %>%
  ungroup(.) %>%
  filter(filter %in% "forecast") %>%
  select(2,5)





# join wl_keys & xreg ----------------------------------------------------------
dyn_fc <- left_join(parameters_xreg, wl_keys, by = "date") %>%
    select(1,8,2,3,4) %>%
    filter(date < "2023-01-17") %>%
    as_tsibble(., index = "date") %>%
    fill_gaps(., date) %>%
    fill(., patients, .direction = "up") %>%
    ungroup(.) %>%
    ts(frequency = 30)
fit <- auto.arima(dyn_fc[, "patients"],
                  xreg = dyn_fc[,4])
fit
checkresiduals(fit)
upcoming_advertising <- -100*dyn_fc[,4]
fcast <- forecast::forecast(fit, xreg = upcoming_advertising)
autoplot(fcast)

# work out additions and removals average rate for past 30 days.
# Then use this calculation to work out waiting list rate change
# Allow interactivity for the additions and removals numbers (multiply by what percent etc)


