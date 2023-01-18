#####
# Clearance date table

# Table of long waiters % diff between train_halt and last horizon date
lw_diff <- path_keys %>%
  filter(., grepl("[[:digit:]]", spec_desc)) %>%
  group_by(., date, spec_desc) %>%
  summarise("lower bound" = round(min(.sim)),
            "upper bound" = round(max(.sim)),
            mean = round(mean(.sim))) %>%
  ungroup(.) %>%
  filter(., date == train_halt | date == (train_halt + h) -1) %>%
  group_by(spec_desc) %>%
  mutate(., "percent_change" = round(-100 + (mean/lag(mean)*100),
                                     digits = 2)) %>%
  filter(., date == ((train_halt +h) -1)) %>%
  select(spec_desc, mean, percent_change)

# Table of clearance dates for long waiters
lw_clear <- path_keys %>%
  filter(., grepl("[[:digit:]]", spec_desc)) %>%
  mutate(clear_date = if_else(.sim == 0, T, F)) %>%
  group_by(clear_date, spec_desc) %>%
  filter(date == min(date)) %>%
  select(date, spec_desc, clear_date) %>%
  distinct(., clear_date, .keep_all = T) %>%
  mutate(., clear_date = if_else(clear_date == T, date, ymd(NA))) %>%
  group_by(., spec_desc) %>%
  slice_max(!is.na(clear_date), with_ties = F) %>%
  select(., spec_desc, clear_date)

# Join tables on spec_desc and make manuscript ready
lw_table <- lw_clear %>%
  left_join(lw_diff, by = "spec_desc") %>%
  mutate(., mean = if_else(!(is.na(clear_date)), 
                           as.numeric(NA), 
                           `mean`),
         percent_change = if_else(!(is.na(clear_date)), 
                                  as.numeric(NA), 
                                  `percent_change`)) %>%
  separate(., spec_desc, into = c("wl", "spec"), sep = "_") %>%
  arrange(., desc(percent_change), by_group = F) %>%
  mutate(across(c("percent_change"), 
                ~ if_else((percent_change >= 0), 
                          sprintf(fmt = "%+2g %%", .x),
                          if_else((percent_change < 0), 
                                  sprintf(fmt = "%-2g %%", .x),
                                  as.character(NA))))) %>%
  rename(., "List" = wl,
         "Speciality" = spec,
         "Date list cleared" = clear_date) %>%
  rename_with(., .fn = ~paste0("List size at ", (train_halt + h) - 1), 
              .cols = mean) %>%
  rename_with(., .fn = ~paste0("Difference from ", train_halt), 
              .cols = percent_change) %>%
  group_by(., List) %>%
  group_split(.)

# Split long waiter lists into two dataframes
lw_52 <- as_tibble(lw_table[[1]])
lw_65 <- as_tibble(lw_table[[2]])


rm(lw_diff, lw_clear)
# ------------------------------------------------------------------------------