#####
# Build list of prev 30 day parameters for forecasting model bias
period = month(train_halt - 25, label = T)


# ------------------------------------------------------------------------------
#####
# Build df of last 30 days to fill in zero demand/capacity/rott days later
days <-
  data.frame(date = seq(
    from = (train_halt - 40),
    to = (train_halt - 10),
    by = 1
  ))

# Filter last 30 days and to each patient's most recent data entry 
data_recent <- data_param %>%
  group_by(RTT_Start_Date, decision_to_admit_date_dt, admission_date_dt, 
           removed_date_dt, rott, speciality, internal_number, PathwayNumber,
           wl) %>%
  summarise(rtt_days_wait = max(rtt_days_wait), .groups = "drop_last")


# ------------------------------------------------------------------------------
#####
# Build parameters list
list_param <- list()

  for (j in speciality){
    # Demand - last 30 days
    demand_daily <- data_recent %>%
      filter(speciality == j) %>%
      group_by(wl, decision_to_admit_date_dt) %>%
      summarise(demand = n(), .groups = "drop_last") %>%
      ungroup(.) %>%
      rename("date" = decision_to_admit_date_dt) %>%
      filter(date > (train_halt - 40) & date < (train_halt - 10)) %>%
      pad(interval = "day") %>%
      mutate(demand = if_else(is.na(demand), 0, as.numeric(demand)))

      demand_mean <- demand_daily %>%
        group_by(wl = grepl("[[:digit:]]", wl), .add = T) %>%
        summarise(month_mean = sum(demand)/30) %>%
        mutate(wl = if_else(wl == T, "Inpatient_wl", "Planned"))

      ggplot(data = demand_daily, aes(x = date, y = demand,
                                     colour = wl)) +
        geom_line(se = F)
    
      parameters <- rbind(
        data.frame(
          speciality = j,
          time_period = period[1],
          metric = "demand",
          list = demand_mean$wl,
          month_mean = demand_mean$month_mean
          )
        )
    
      list_param[[paste0(i, "_", j)]] <- parameters
      wl_param <- bind_rows(list_param)
# Haematology has no one on inpatient, so flip through my loop from earlier to add it as a zero group.
# add the above loop and also fix capacity so it's inpatient_wl and not removed.
      
      capacity_daily <- data_recent %>%
  filter(speciality == j,
         rott == 0) %>%
  group_by(wl, removed_date_dt) %>%
  summarise(capacity = n(), .groups = "drop_last") %>%
  rename("date" = removed_date_dt) %>%
  right_join(days, by = "date") %>%
  filter(date > (train_halt - 40) & date < (train_halt - 10)) %>%
  group_by(wl) %>%
  pad(interval = "day") %>%
  mutate(capacity = if_else(is.na(capacity), 0, as.numeric(capacity)))


capacity_mean <- capacity_daily %>%
  drop_na() %>%
  group_by(wl) %>%
  summarise(month_mean = sum(capacity)/30)

ggplot(data = capacity_daily, aes(x = date, y = capacity, colour = wl)) +
  geom_smooth(se = F)

parameters <- rbind(
  parameters,
  data.frame(
    speciality = j,
    time_period = period[1],
    metric = "capacity",
    list = capacity_mean$wl,
    month_mean = capacity_mean$month_mean
  )
)

list_param[[paste0(i, "_", j)]] <- parameters
wl_param <- bind_rows(list_param)
}
