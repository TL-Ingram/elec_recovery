# facet print spec plots that not forecast to recover
hist_wl_test <- wl_keys %>%
  group_by(speciality, wl, date) %>%
  summarise(patients = sum(patients)) %>%
  mutate(filter = "historic")

# Tidy all_forecast to same format as hist_wl, then knit together
af_test_test <- path_keys %>%
  group_by(., .model, wl, spec_desc, date) %>%
  summarise(upper = quantile(.sim, 0.7),
            p_mean = quantile(.sim, 0.3),
            lower = quantile(.sim, 0.05)) %>%
  ungroup(.) %>%
  rename("speciality" = spec_desc) %>%
  filter(date >= train_halt) %>%
  mutate(filter = "forecast")

knitted_test <- rbind(hist_wl_test, af_test_test)

# Plot long waiters
plot_lw <- knitted_test %>%
  mutate(label = ifelse((patients < 10 | p_mean < 10), 1, 0)) %>%
  group_by(speciality, wl) %>%
  filter(any(is.na(label)))
  
  
plot_lw_2 <- plot_lw %>%
  ggplot() +
  facet_wrap(speciality ~ ., scales = "free_y") +
  geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.6, 
            data = plot_lw %>% 
              filter(wl %in% c(">52 weeks", 
                               ">65 weeks"))) + 
  geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.6, 
            data = plot_lw %>% 
              filter(wl %in% c(">52 weeks", 
                               ">65 weeks"),
                     .model == "paramatised")) +
  geom_ribbon(aes(date, ymax = upper, ymin = lower),
              fill="slategray3", alpha=.3,
              data = plot_lw %>% filter(wl %in% ">52 weeks")) +
  geom_ribbon(aes(date, ymax = upper, ymin = lower),
              fill="slategray3", alpha=.3,
              data = plot_lw %>% filter(wl %in% ">65 weeks")) +
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
  scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
  plot_defaults_two +
  scale_colour_manual(values = c("lightsteelblue4", "indianred4")) +
  # ylim(c(0,1200)) +
  labs(fill = "",
       x = "",
       y = "Patients",
       title = "Long waiter lists",
       level = "",
       colour = "")
plot_lw_2

