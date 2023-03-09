#####
# Clearance date table and graph
# Table of long waiters % diff between train_halt and last horizon date
lw_diff <- path_keys %>%
  filter(., grepl("[[:digit:]]", wl),
         .model == "paramatised") %>%
  group_by(., date, wl, spec_desc) %>%
  summarise("lower bound" = round(min(.sim)),
            "upper bound" = round(max(.sim)),
            mean = round(mean(.sim))) %>%
  ungroup(.) %>%
  filter(., date == train_halt | date == (train_halt + h) -1) %>%
  group_by(wl, spec_desc) %>%
  mutate(., "percent_change" = round(-100 + (mean/lag(mean)*100),
                                     digits = 2)) %>%
  filter(., date == ((train_halt + h) -1)) %>%
  select(wl, spec_desc, mean, percent_change)

# Table of clearance dates for long waiters
lw_clear <- path_keys %>%
  filter(., grepl("[[:digit:]]", wl),
         .model == "paramatised") %>%
  mutate(clear_date = if_else(.sim == 0, T, F)) %>%
  group_by(clear_date, wl, spec_desc) %>%
  filter(date == min(date)) %>%
  select(date, wl, spec_desc, clear_date) %>%
  distinct(., clear_date, .keep_all = T) %>%
  mutate(., clear_date = if_else(clear_date == T, date, ymd(NA))) %>%
  group_by(., wl, spec_desc) %>%
  slice_max(!is.na(clear_date), with_ties = F) %>%
  select(., wl, spec_desc, clear_date)

# Join tables on spec_desc and make manuscript ready
lw_table <- lw_clear %>%
  left_join(lw_diff, by = c("wl", "spec_desc")) %>%
  mutate(., mean = if_else(!(is.na(clear_date)), 
                           as.numeric(NA), 
                           `mean`),
         percent_change = if_else(!(is.na(clear_date)), 
                                  as.numeric(NA), 
                                  `percent_change`)) %>%
  arrange(., desc(percent_change), by_group = F) %>%
  mutate(across(c("percent_change"), 
                ~ if_else((percent_change >= 0), 
                          sprintf(fmt = "%+2g %%", .x),
                          if_else((percent_change < 0), 
                                  sprintf(fmt = "%-2g %%", .x),
                                  as.character(NA))))) %>%
  rename(., "List" = wl,
         "Speciality" = spec_desc,
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

write.csv(lw_65, here("csv", "clear_date", "current", 
                      "65wk_clear_dates.csv"), row.names = F)
write.csv(lw_52, here("csv", "clear_date", "current",
                      "52wk_clear_dates.csv"), row.names = F)

rm(lw_diff, lw_clear)
# ------------------------------------------------------------------------------


# Plot long waiters
plot_lw <- knitted %>%
  ggplot() +
  geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.8, 
            data = knitted %>% 
              filter(wl %in% c("weeks_52", 
                               "weeks_65"))) + 
  geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.8, 
            data = knitted %>% 
              filter(wl %in% c("weeks_52", 
                               "weeks_65"),
            .model == "paramatised")) +
  geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
              fill="slategray3", alpha=.3,
              data = knitted %>% filter(wl %in% "weeks_52")) +
  geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
              fill="slategray3", alpha=.3,
              data = knitted %>% filter(wl %in% "weeks_65")) +
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
  geom_text(data = wl_prep, aes(x = train_init + 90, y = Inf,
                                label = train_period_label),
            vjust = 1.5, size = 4.5, colour = "grey40") +
  scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
  plot_defaults_two +
  scale_colour_manual(values = c("lightsteelblue4", "indianred4")) +
  labs(fill = "",
       x = "",
       y = "Patients",
       title = "WWL inpatient waiting list - long waiters",
       level = "",
       colour = "",
       subtitle = glue("Forecast horizon begins from {train_halt} 
                               and extends for {h} days"),
       caption = glue("AI Training period is from {train_init}
                              to {train_halt}
                              Parameter weighting estimated from {param_start}
                              to {train_halt}
                              Horizon lines depict mean predicted list size
                              Shaded region depicts 80% prediction interval"))

# Save plot
ggsave(here("plots", "current", "overall_therapeutic", "longwaiter_wl.jpg"), 
       device = "jpg", width = 14, height = 10)

# Print plot
print(plot_lw)

