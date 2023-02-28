# Read individual spec parameters into df
parameters <- list.files(path = "C:/R_projects/elec_recovery/csv/parameters", full.names = T, pattern = "*.csv") %>%
  map_dfr(read.csv)

# Clean and modify df
param_all <- parameters %>%
  pivot_longer(cols = c("adms", "dtas", "rott"), 
               names_to = "names", values_to = "values") %>%
  mutate(date = ymd(dt)) %>%
  select(-dt) %>%
  filter(date < yesterday) %>%
  mutate(across(names, str_replace, c("adms", "dtas", "rott"), c("removals", "additions", "error"))) %>%
  mutate(month = floor_date(date, unit = "month"))

# Summarise parameters by speciality
param_mean <- param_all %>%
  group_by(speciality, month, names) %>%
  summarise(mean = mean(values)) %>%
  ungroup(.)
param_mean %>%
  ggplot(aes(x = month, y = mean, colour = names)) +
  geom_line() +
  facet_wrap(. ~ speciality, scales = "free_y") +
  theme_bw()

# Rates of latest month
param_xyz <- param_mean %>%
  filter(month == max(month)) %>%
  pivot_wider(-month, names_from = "names", values_from = "mean")

# Mean waiters of latest month
param_n <- wl_keys %>%
  filter(date < yesterday) %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  filter(month == max(month)) %>%
  group_by(speciality, month) %>%
  summarise(mean = mean(patients)) %>%
  ungroup(.)
