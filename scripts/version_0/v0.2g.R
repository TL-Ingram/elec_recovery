# graphing full wl

source(here("scripts", "version_0", "v0.2-full_waiting_list.R"))

horizon_wl <- res.sum %>%
  mutate(date = dmy(date)) %>%
  select(metric,
         mean,
         date)

ggplot(horizon_wl, aes(x = date, y = mean, colour = metric)) +
  geom_line() +
  theme_bw()

# show only 104 weeks
# show only 52 weeks
# show only 13 weeks
# show full and project 5 years