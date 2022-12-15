# specialty <- 

data <- read_csv(here("data", "hist_wl.csv"))

specialty <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)

for (specialty_name in specialty) {
wl <- test %>%
  filter(., spec_desc == specialty_name,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarise(patients = n())
wl_52 <- test %>%
  filter(., spec_desc == specialty_name,
         wm52 == 1,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, wm52) %>%
  summarise(wm52 = n())

#cbind by date
colon <- colon_wl %>%
  left_join(colon_52, by = "date") %>%
  pivot_longer(cols = c(2:3), names_to = "metric", values_to = "value")
print(colon)
}



colon %>%
  ggplot(aes(x = date, y = value, colour = metric)) +
  geom_line() +
  theme_bw()

shelf()
