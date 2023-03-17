#####
# Build list of prev 30 day parameters for forecasting model bias
speciality = speciality
wl_type = c("Inpatient_wl", "Planned")
period = month(train_halt, label = T)
i = "Inpatient_wl"


# ------------------------------------------------------------------------------
#####
# Build df of last 30 days to fill in zero demand/capacity/rott days later
days <-
  data.frame(date = seq(
    from = (train_halt - 30),
    to = train_halt,
    by = 1
  ))

# Filter last 30 days and to each patient's most recent data entry 
data_recent <- data_mod %>%
  filter(date >= (train_halt - 30)) %>%
  group_by(RTT_Start_Date, decision_to_admit_date_dt, admission_date_dt, 
           removed_date_dt, rott, speciality, internal_number, PathwayNumber,
           wl) %>%
  summarise(
    rtt_days_wait = max(rtt_days_wait), .groups = "drop_last")


# ------------------------------------------------------------------------------
#####
# Build parameters list
list_param <- list()

for(i in wl_type) {
  for (j in speciality){
    # Demand - last 30 days
    demand_daily <- data_recent %>%
      filter(wl != "Planned",
             speciality == "Trauma & Orthopaedics") %>%
      group_by(wl, decision_to_admit_date_dt) %>%
      summarise(demand = n(), .groups = "drop_last") %>%
      ungroup(.) %>%
      rename("date" = decision_to_admit_date_dt) %>%
      right_join(days, by = "date") %>%
      filter(date >= (Sys.Date() - 33)) %>%
      pad(interval = "day") %>%
      mutate(demand = if_else(is.na(demand), 0, as.numeric(demand)),
             wl = if_else(is.na(wl), i, wl))

      demand_mean <- demand_daily %>%
        group_by(grepl("[[:digit:]]", wl)) %>%
        summarise(month_mean = sum(demand)/30)
    
      ggplot(data = demand_daily, aes(x = date, y = demand,
                                     colour = wl)) +
        geom_line(se = F)
    
      parameters <- rbind(
        data.frame(
          speciality = j,
          time_period = period[1],
          metric = "demand",
          list = i,
          month_mean = demand_mean$month_mean
          )
        )
    
      list_param[[paste0(i, "_", j)]] <- parameters
      wl_param <- bind_rows(list_param)
    
  }
}
# make the above into something that pulls into a list and then outputs to parameters df
# NEED TO GO BACK TO DRAWING BOARD FOR CAPACITY... do I go back to original QVD and remake the dataframe? This will also help with Snowflake transition
# Capacity: this is the number of people admitted during the period

j = "Trauma & Orthopaedics"
i = "Inpatient_wl"

for (j in speciality){
capacity_daily <- data_recent %>%
  filter(speciality == "Trauma & Orthopaedics",
         rott == 0) %>%
  group_by(wl, removed_date_dt) %>%
  summarise(capacity = n(), .groups = "drop_last") %>%
  ungroup(.) %>%
  rename("date" = removed_date_dt) %>%
  right_join(days, by = "date") %>%
  pad(interval = "day") %>%
  mutate(capacity = if_else(is.na(capacity), 0, as.numeric(capacity)),
         wl = if_else(is.na(wl), "Removed", wl))







capacity_mean <- capacity_daily %>%
  summarise(month_mean = sum(capacity)/30)

ggplot(data = capacity_daily, aes(x = date, y = capacity)) +
  geom_smooth(se = F)

parameters <- rbind(
  parameters,
  data.frame(
    speciality = j,
    time_period = labels[d],
    metric = "capacity",
    list = capacity_mean$covid_recovery_priority,
    month_mean = capacity_mean$month_mean
  )
)
}
}
