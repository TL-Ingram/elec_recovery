# graphing full wl

source(here("scripts", "version_0", "v0.2-full_waiting_list.R"))
a_data <- read_rds(here("rds", "forecast_horizon", "Colorectal Surgery.rds"))

a_data <- list.files(path = here("rds", "forecast_horizon"), pattern = "*.rds") %>%
  map_dfr(read_rds) #%>% 
  bind_rows()
getwd() 
setwd("C:/R_projects/elec_recovery/rds/forecast_horizon/")
setwd("C:/R_projects/elec_recovery/")
horizon_wl <- a_data %>%
  mutate(date = dmy(date)) %>%
  select(metric,
         mean,
         date,
         specialty) %>%
  filter(!grepl("Planned", metric),
         !grepl("nwl", metric),
         !grepl("wl_D", metric))
  # filter(!grepl("rtt_n", metric)) %>%
  # filter(!grepl("nwl", metric))

a_horizon <- ggplot(horizon_wl, aes(x = date, y = mean, colour = metric)) +
  geom_line() +
  theme_bw() +
  facet_wrap(. ~ specialty, scales = "free_y") +
  labs(y = "patients")
ggsave(here("plots", "speciality_horizon_free_y.jpg"), width = 15, height = 15, dpi = 300)
a_horizon
?facet_wrap

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

f_data <- read_rds(here("rds", "forecast_horizon_FULL_WL.rds"))
horizon_wl <- f_data %>%
  mutate(date = dmy(date)) %>%
  select(metric,
         mean,
         date) %>%
  filter(!grepl("act", metric)) %>%
  filter(!grepl("rtt_m", metric)) %>%
  filter(!grepl("rtt_n", metric)) %>%
  filter(!grepl("nwl", metric)) %>%
  filter(!grepl(""))

f_horizon <- ggplot(horizon_wl, aes(x = date, y = mean, colour = metric)) +
  geom_line() +
  theme_bw() #+
  # facet_wrap(. ~ specialty, scales = "fixed")
f_horizon
