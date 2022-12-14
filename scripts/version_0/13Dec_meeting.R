time_taken <- data.frame(
  time = 
  waits = as.numeric(wl$total_days_wait),
  inpatient_waits = as.numeric(wl$days_wait),
  priority = wl$priority,
  priority_definition = wl$priority_definition,
  planned  = as.numeric(wl$planned_admission_date - init_date),
  stringsAsFactors = FALSE
)


dates <- data.frame(
  time = rep(seq(as.Date('2020-12-01'), as.Date('2022-11-30'), by = 'day')),
  job = NA)
test <- dates %>%
  mutate(time = ymd(time),
         time_month = month(time),
         time_year = year(time),
         job = if_else(time_year == 2021 & time_month >= 3, "model & Qlik work",
                       if_else(time_year == 2022 & time_month == 10, "domain learning",
                               if_else(time_year == 2022 & time_month >= 11, "model & Qlik work",
                                       if_else(time_year == 2020, "domain learning",
                                               if_else(time_year == 2021 & time_month <3, "domain learning",
                                       "no work")))))) %>%
  mutate(team = if_else(time_year == 2020 | time_year == 2021, "team 1", "team 2")) %>%
  group_by(job, team) %>%
  summarise(days = n()) %>%
  filter(!(job == "no work")) %>%
  ggplot(aes(x = team, y = days, fill = job)) +
  geom_col() +
  theme_ipsum_pub(axis_text_size = 16,
                  axis_title_size = 16) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values = c("lightblue", "steelblue")) +
  labs(fill = "",
       x = "",
       y = "Days",
       title = "Total project time")
test                       
ggsave(here("plots", "project_time.jpg"), width = 7.5, height = 10, dpi = 600)
