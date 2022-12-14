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
  mutate(metric = str_replace(metric, "nwm104_total", ">104"),
         metric = str_replace(metric, "nwm78_total", ">78"),
         metric = str_replace(metric, "nwm52_total", ">52")) %>%
  mutate(date = dmy(date),
         metric = factor(metric, level = c(">104", ">78", ">52", "wl_Deferred", "wl_Planned", "wl_size_active")))
  
# filter(!grepl("Planned", metric))

waiting_list_only <- horizon_wl_all %>%
  filter(!grepl("nwl", metric),
         !grepl(">", metric)) %>%
  ggplot(aes(x = date, y = patients, fill = metric)) +
  geom_area(aes(fill = metric), alpha = 0.8) +
  scale_fill_manual(values = c("darkblue", "steelblue", "lightblue")) +
  theme_ipsum_pub() +
  labs(fill = "",
       title = "Projected number of patients on waiting list",
       subtitle = "Inpatient waiting list only",
       y = "Number of patients",
       x = "")
ggsave(here("plots", "wl_all_concat.jpg"), width = 10, height = 6, dpi = 300)
waiting_list_only
# change date start to actual date (Nov 14th 2022)

nwl_nwm <- horizon_wl_all %>%
  filter(!grepl("wl_", metric),
         !grepl("nwl", metric)) %>%
  ggplot(aes(x = date, y = patients, fill = metric)) +
  geom_area(aes(fill = metric), alpha = 0.8) +
  scale_fill_manual(values = c("darkblue", "steelblue", "lightblue")) +
  theme_ipsum_pub() +
  labs(fill = "Duration on list (weeks)",
       title = "Projected number of patients waiting significant periods",
       subtitle = "Inpatient waiting list only",
       y = "Number of patients",
       x = "")
ggsave(here("plots", "wl_all_nwm.jpg"), width = 10, height = 6, dpi = 300)
# rename legen
nwl_nwm


