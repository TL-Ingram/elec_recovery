# Filter data appropriate for the models

# Overall WL (P2-P4)
o_wl <- data %>%
  filter(., !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarise(patients = n())

# 52 week waiter WL
wl_52 <- data %>%
  filter(., wm52 == 1,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, wm52) %>%
  summarise(patients = n()) %>%
  ungroup(.)