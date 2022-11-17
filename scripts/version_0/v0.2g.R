# graphing full wl

source(here("scripts", "version_0", "v0.2-full_waiting_list.R"))
data <- read_rds(here("rds", "res_sum", "Trauma & Orthopaedics.rds"))
horizon_wl <- data %>%
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


wl %>%
  filter(decision_to_admit_date_dt > "2021-01-01") %>%
  ggplot(aes(x = decision_to_admit_date_dt)) +
  geom_histogram() +
  theme_bw()

# do all specialties individually...
# do current waiting list size and change since historic
# do clearance times with graphs for each specialty

