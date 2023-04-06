# Write speciality forecast path to a fable object
sim_results <- all_path %>%
  as_tibble(., index = date) %>%
  mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
  group_by(.model, date, spec_desc) %>%
  summarise(dist = distributional::dist_sample(list(.sim)), 
            .groups = "drop_last") %>%
  ungroup(.) %>%
  as_fable(index = date, key = .model, distribution = dist, 
           response="patients")

# Join fable object to historic list and plot
spec_forecast <- sim_results %>%
  autoplot(data = wl_prep, level = 80, size = 0.6, alpha = 0.7) +
  geom_line(data = wl_prep, aes(x = date, y = patients), size = 0.6,
            alpha = 0.7, colour = "grey50") +
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
  geom_text(data = wl_prep, aes(x = (train_init + 90), 
                                y = Inf), 
            label = train_period_label,
            vjust = 1.5, size = 3, colour = "grey40") +
  scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
  scale_colour_discrete(guide = "none") +
  plot_defaults +
  labs(fill = "",
       x = "",
       y = "Patients",
       title = glue("{j} - {i} list"),
       level = "",
       subtitle = glue("Forecast begins from {train_halt} and extends for {h} days"),
       caption = glue("AI Training period is from {train_init} to {train_halt}"))

spec_forecast
# Save plot
file_name <- glue("{i}_{j}_2103")
ggsave(here("plots", "current", "speciality_forecasts", j, 
            filename=paste0(file_name, ".png")), device = "png")
