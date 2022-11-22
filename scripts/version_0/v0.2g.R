# graphing full wl

source(here("scripts", "version_0", "v0.2-full_waiting_list.R"))
data <- read_rds(here("rds", "forecast_horizon", "Ophthalmology.rds"))

a_data <- list.files(path = here("rds", "forecast_horizon"), pattern = "*.rds") %>%
  map_dfr(read_rds) #%>% 
  bind_rows()
getwd() 
setwd("C:/R_projects/elec_recovery/rds/forecast_horizon/")
horizon_wl <- a_data %>%
  mutate(date = dmy(date)) %>%
  select(metric,
         mean,
         date,
         specialty) %>%
  filter(!grepl("act", metric)) %>%
  filter(!grepl("rtt_m", metric)) %>%
  filter(!grepl("rtt_n", metric)) %>%
  filter(!grepl("nwl", metric))

a_horizon <- ggplot(horizon_wl, aes(x = date, y = mean, colour = metric)) +
  geom_line() +
  theme_bw() +
  facet_wrap(. ~ specialty, scales = "fixed") +
  ggsave(here("plots", "speciality_horizon_fixed_y.jpg"), width = 20, height = 20, dpi = 300)

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

