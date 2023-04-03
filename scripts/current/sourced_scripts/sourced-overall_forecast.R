# Create graph of overall waiting list position
hist_wl <- wl_keys %>%
  group_by(wl, date) %>%
  summarise(patients = sum(patients)) %>%
  mutate(filter = "historic")

# Tidy all_forecast to same format as hist_wl, then knit together
af_test <- path_keys %>%
  group_by(., .model, wl, spec_desc, date) %>%
  summarise(max = quantile(.sim, 0.8),
            mean = mean(.sim),
            min = quantile(.sim, 0.2)) %>%
  ungroup(.) %>%
  select(-(spec_desc)) %>%
  group_by(.model, wl, date) %>%
  summarise(p_mean = sum(`mean`),
            p_upper = sum(`max`),
            p_lower = sum(`min`)) %>%
  filter(date >= train_halt) %>%
  mutate(filter = "forecast")

# Row bind historical and forecast
knitted <- rbind(hist_wl, af_test) %>%
  mutate(wl = str_replace(wl, ">52", ">52 weeks"),
         wl = str_replace(wl, ">65", ">65 weeks"))

# Plot inpatient and planned waiting lists
plot_o <- knitted %>%
  ggplot() +
  geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.8, 
            data = knitted %>% 
              filter(wl %in% c("Planned", 
                               "Inpatient_wl"))) +
  geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.8, 
            data = knitted %>% 
              filter(wl %in% c("Planned", 
                               "Inpatient_wl"),
                     .model == "paramatised")) +
  geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
              fill="slategray3", alpha=.3,
              data = knitted %>% filter(wl %in% "Planned")) +
  geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
              fill="slategray3", alpha=.3,
              data = knitted %>% filter(wl %in% "Inpatient_wl")) +
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
            vjust = 1.5, size = 3, colour = "grey40") +
  scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
  plot_defaults_two +
  scale_colour_manual(values = c("royalblue3", "mediumpurple3")) +
  labs(fill = "",
       x = "",
       y = "Patients",
       title = "WWL inpatient waiting list - forecasted position",
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
ggsave(here( "plots", "current", "overall_therapeutic", "inpatient&planned_wl.jpg"), 
       device = "jpg", width = 14, height = 10)

# Print plot
print(plot_o)
