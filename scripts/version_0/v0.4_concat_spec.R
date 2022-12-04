# packages ---------------------------------------------------------------------
# librarian::lib_startup("librarian", global = FALSE)
# library(librarian)
shelf(tidyverse, here, timeDate, parallel, lubridate, odbc, hrbrthemes)
source("db_connect_functions.R")
source("simulation_function.R")
source("helper_functions.R")
setwd("C:/R_projects/elec_recovery/rds/forecast_horizon/")
a_data <- list.files(path = here("rds", "forecast_horizon"), pattern = "*.rds") %>%
    map_dfr(read_rds)

horizon_wl_all <- a_data %>% 
  select(-c("specialty", "q025", "q975")) %>%
  group_by(metric, date) %>%
  mutate(patients = sum(`mean`)) %>%
  select(-("mean")) %>%
  distinct(.) %>%
  mutate(date = dmy(date))
# filter(!grepl("Planned", metric))

waiting_list_only <- horizon_wl_all %>%
  filter(!grepl("nwl", metric),
         !grepl("nwm", metric)) %>%
  ggplot(aes(x = date, y = patients, fill = metric)) +
  geom_area(aes(fill = metric), alpha = 0.8) +
  scale_fill_manual(values = c("darkblue", "steelblue", "lightblue")) +
  theme_ipsum_pub()
# ggsave(here("plots", "wl_all_concat.jpg"), width = 6, height = 6, dpi = 300)
waiting_list_only
# change date start to actual date (Nov 14th 2022)

nwl_nwm <- horizon_wl_all %>%
  filter(!grepl("wl_", metric),
         !grepl("nwl", metric)) %>%
  ggplot(aes(x = date, y = patients, fill = metric)) +
  geom_area(aes(fill = metric), alpha = 0.8) +
  scale_fill_manual(values = c("darkblue", "steelblue", "lightblue")) +
  theme_ipsum_pub()
ggsave(here("plots", "wl_all_nwm.jpg"), width = 6, height = 6, dpi = 300)
# rename legend
nwl_nwm


