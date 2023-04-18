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

# Filter groups plotted to those with > 10 patients at h start date
plot_lw <- knitted_test %>%
  mutate(label = ifelse((patients < 10 | p_mean < 10), 1, 0)) %>%
  group_by(speciality, wl) %>%
  filter(any(is.na(label))) %>%
  write_rds(here("rds", "test.rds"))

# Plot faceted long waiters
plot_lw_2 <- plot_lw %>%
  ggplot() +
  facet_wrap(speciality ~ ., scales = "free_y", ncol = 4) +
  geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.2, 
            data = plot_lw %>% 
              filter(wl %in% c(">52 weeks", 
                               ">65 weeks"))) + 
  geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.2, 
            data = plot_lw %>% 
              filter(wl %in% c(">52 weeks", 
                               ">65 weeks"),
                     .model == "paramatised")) +
  geom_ribbon(aes(date, ymax = upper, ymin = lower),
              fill="slategray3", alpha=.3,
              data = plot_lw %>% 
                filter(wl %in% ">52 weeks",
                       .model == "paramatised")) +
  geom_ribbon(aes(date, ymax = upper, ymin = lower),
              fill="slategray3", alpha=.3,
              data = plot_lw %>% 
                filter(wl %in% ">65 weeks",
                       .model == "paramatised")) +
  geom_vline(data = wl_prep, xintercept = train_halt,
             linetype = "dashed", colour = "grey50",
             size = 0.25, alpha = 0.8) +
  geom_vline(data = wl_prep, xintercept = train_init,
             linetype = "dashed", colour = "grey50",
             size = 0.25, alpha = 0.8) +
  geom_vline(data = wl_prep, xintercept = param_start,
             linetype = "dashed", colour = "grey50",
             size = 0.25, alpha = 0.8) +
  annotate("rect", xmin = train_init, xmax = train_halt,
           ymin = -Inf, ymax = Inf,
           alpha = .1, fill = "grey75") +
  scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_rect(fill="grey95"),
        legend.position = "bottom",
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5),
        plot.caption = element_text(size = 6, hjust = 0, face = "bold")) +
  scale_colour_manual(values = c("steelblue", "indianred4")) +
  labs(fill = "",
       x = "",
       y = "Patients",
       level = "",
       colour = "",
       caption = "Figure 1. Long waiters forecast. 
       Specialities shown are those that have >5 long waiters at horizon start date")
plot_lw_2

