#####
# Build list of prev 30 day parameters for forecasting model bias
period = month(train_halt - 15, label = T)


# ------------------------------------------------------------------------------
#####
# Filter last 30 days and to each patient's most recent data entry 
data_recent <- data_param %>%
  group_by(RTT_Start_Date, decision_to_admit_date_dt, admission_date_dt, 
           removed_date_dt, rott, speciality, internal_number, PathwayNumber,
           wl) %>%
  summarise(rtt_days_wait = max(rtt_days_wait), .groups = "drop_last",
            weeks_52 = max(wm52),
            weeks_65 = max(wm65)) %>%
  ungroup(.)


# ------------------------------------------------------------------------------
#####
# Build parameters list
# speciality = "Colorectal Surgery"
# j = "Colorectal Surgery"
rm(wl_param)
rm(parameters)
list_param <- list()
  for (j in speciality){
    # Demand - last 30 days
    demand_daily <- data_recent %>%
      filter(speciality == j) %>%
      group_by(wl, decision_to_admit_date_dt) %>%
      summarise(demand = n(), .groups = "drop_last") %>%
      ungroup(.) %>%
      rename("date" = decision_to_admit_date_dt) %>%
      filter(date > (train_halt - 45) & 
               date < (train_halt - 10)) %>%
      # pad(interval = "day") %>%
      mutate(demand = if_else(is.na(demand), 0, as.numeric(demand)))

    # Check what number demand daily gives for median of last 30 days
    # Check if list has zero waiters; if so then add row that says 0 waiters

      demand_mean <- demand_daily %>%
        group_by(wl = grepl("[[:digit:]]", wl), .add = T) %>%
        summarise(month_mean = sum(demand)/30) %>%
        mutate(wl = if_else(wl == T, "Inpatient_wl", "Planned"))
      
      for (wl in c("Planned", "Inpatient_wl")) {
        if (!wl %in% demand_mean$wl) {
          demand_mean <- rbind(
            demand_mean,
            data.frame(wl = wl,
                       month_mean = 0)
          )
        }
      }

      # Sense check
      ggplot(data = demand_daily, aes(x = date, y = demand, colour = wl)) +
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
    
      # Add parameters to growing list
      list_param[[paste0(i, "_", j)]] <- parameters

      
      # ------------------------------------------------------------------------
      #####
      # Capacity - last 30 days
      capacity_daily <- data_recent %>%
        filter(speciality == j,
               rott == 0) %>%
        group_by(wl, removed_date_dt) %>%
        summarise(capacity = n(), .groups = "drop_last") %>%
        ungroup(.) %>%
        rename("date" = removed_date_dt) %>%
        filter(date > (train_halt - 35) & 
                 date < (train_halt - 5)) %>%
        group_by(wl) %>%
        # pad(interval = "day") %>%
        mutate(capacity = if_else(is.na(capacity), 0, as.numeric(capacity)))
      
      # Check if list has zero waiters; if so then add row that says 0 waiters

      
      capacity_mean <- capacity_daily %>%
        drop_na() %>%
        group_by(wl) %>%
        summarise(month_mean = sum(capacity)/30)
      for (wl in c("Planned", "Inpatient_wl")) {
        if (!wl %in% capacity_mean$wl) {
          capacity_mean <- rbind(
            capacity_mean,
            data.frame(wl = wl,
                       month_mean = 0)
          )
        }
      }
      
      # Sense check
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
      
      # Add parameters to growing list
      list_param[[paste0(i, "_", j)]] <- parameters

      
      # ------------------------------------------------------------------------
      #####
      # ROTT - last 30 days
      rott_daily <- data_recent %>%
        filter(speciality == j,
               rott == 1) %>%
        group_by(wl, removed_date_dt) %>%
        summarise(rott = n(), .groups = "drop_last") %>%
        ungroup(.) %>%
        rename("date" = removed_date_dt) %>%
        filter(date > (train_halt - 35) & 
                 date < (train_halt - 5)) %>%
        group_by(wl) %>%
        # pad(interval = "day") %>%
        mutate(rott = if_else(is.na(rott), 0, as.numeric(rott)))
      
      # Check if list has zero waiters; if so then add row that says 0 waiters

      rott_mean <- rott_daily %>%
        group_by(wl) %>%
        summarise(month_mean = sum(rott)/30)
      for (wl in c("Planned", "Inpatient_wl")) {
        if (!wl %in% rott_mean$wl) {
          rott_mean <- rbind(
            rott_mean,
            data.frame(wl = wl,
                       month_mean = 0)
          )
        }
      }
      
      # Sense check
      ggplot(data = rott_daily, aes(x = date, y = rott, colour = wl)) +
        geom_smooth(se = F)
      
      parameters <- rbind(
        parameters,
        data.frame(
          speciality = j,
          time_period = period[1],
          metric = "rott",
          list = rott_mean$wl,
          month_mean = rott_mean$month_mean
        )
      )
      
      # Add parameters to growing list
      list_param[[paste0(i, "_", j)]] <- parameters
      

# ------------------------------------------------------------------------------
#####
# Separate long waiters that are inpatient only
data_long <- data_recent %>%
  filter(wl != "Planned",
         speciality == j) %>%
  pivot_longer(cols = c("weeks_52", "weeks_65"),
               names_to = "wl_lw", values_to = "value") %>%
  select(decision_to_admit_date_dt, removed_date_dt, rott, wl_lw, value) %>%
  rename("wl" = wl_lw) %>%
  filter(value == 1) 

# Demand - long waiters
demand_long <- data_long %>%
  group_by(decision_to_admit_date_dt, wl) %>%
  rename("date" = decision_to_admit_date_dt) %>%
  summarise(demand = n(), .groups = "drop_last") %>%
  ungroup(.) %>%
  filter(date > (train_halt - 35) & 
           date < (train_halt - 5)) %>%
  # pad(interval = "day") %>%
  mutate(demand = if_else(is.na(demand), 0, as.numeric(demand)))

demand_mean <- demand_long %>%
  group_by(wl) %>%
  summarise(month_mean = sum(demand)/30)
for (wl in c("weeks_52", "weeks_65")) {
  if (!wl %in% demand_mean$wl) {
    demand_mean <- rbind(
      demand_mean,
      data.frame(wl = wl,
                 month_mean = 0)
    )
  }
}

parameters <- rbind(
  parameters,
  data.frame(
    speciality = j,
    time_period = period[1],
    metric = "demand",
    list = demand_mean$wl,
    month_mean = demand_mean$month_mean
  )
)

# Add parameters to growing list
list_param[[paste0(i, "_", j)]] <- parameters

# Capacity - long waiters
capacity_long <- data_long %>%
  filter(rott == 0) %>%
  group_by(removed_date_dt, wl) %>%
  rename("date" = removed_date_dt) %>%
  summarise(capacity = n(), .groups = "drop_last") %>%
  ungroup(.) %>%
  filter(date > (train_halt - 35) & 
           date < (train_halt - 5)) %>%
  # pad(interval = "day") %>%
  mutate(capacity = if_else(is.na(capacity), 0, as.numeric(capacity)))

capacity_mean <- capacity_long %>%
  group_by(wl) %>%
  summarise(month_mean = sum(capacity)/30)

for (wl in c("weeks_52", "weeks_65")) {
  if (!wl %in% capacity_mean$wl) {
    capacity_mean <- rbind(
      capacity_mean,
      data.frame(wl = wl,
                 month_mean = 0)
    )
  }
}

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

# Add parameters to growing list
list_param[[paste0(i, "_", j)]] <- parameters

# ROTT - long waiters
rott_long <- data_long %>%
  filter(rott == 1) %>%
  group_by(removed_date_dt, wl) %>%
  rename("date" = removed_date_dt) %>%
  summarise(rott = n(), .groups = "drop_last") %>%
  ungroup(.) %>%
  filter(date > (train_halt - 35) & 
           date < (train_halt - 5)) %>%
  # pad(interval = "day") %>%
  mutate(rott = if_else(is.na(rott), 0, as.numeric(rott)))

rott_mean <- rott_long %>%
  group_by(wl) %>%
  summarise(month_mean = sum(rott)/30)
for (wl in c("weeks_52", "weeks_65")) {
  if (!wl %in% rott_mean$wl) {
    rott_mean <- rbind(
      rott_mean,
      data.frame(wl = wl,
                 month_mean = 0)
    )
  }
}

parameters <- rbind(
  parameters,
  data.frame(
    speciality = j,
    time_period = period[1],
    metric = "rott",
    list = rott_mean$wl,
    month_mean = rott_mean$month_mean
  )
)

# Add parameters to growing list
list_param[[paste0(i, "_", j)]] <- parameters


# ------------------------------------------------------------------------------
#####
# All parameters bound into final data frame
wl_param <- bind_rows(list_param) %>%
  drop_na()
}

parameters <- wl_param %>%
  drop_na() %>%
  write_rds(here("rds", "parameters", "all_spec.rds"))