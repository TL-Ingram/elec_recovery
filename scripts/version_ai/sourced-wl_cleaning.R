# Filter data appropriate for the models

# Overall WL (P2-P4)
o_wl <- data %>%
  filter(., !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, spec_desc) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  mutate(wl = "o_wl")

# 52 week waiter WL. lw = long waiter
lw_wl <- data %>%
  filter(., wm52 == 1,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, spec_desc, wm52) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  select(-(wm52)) %>%
  mutate(wl = "lw_wl")
  
wl_comp <- rbind(o_wl, lw_wl)
rm(data, lw_wl, o_wl)
