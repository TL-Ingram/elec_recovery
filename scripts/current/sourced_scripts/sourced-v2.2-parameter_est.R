source(here("scripts/archive/db_connect_functions.R"))
source(here("scripts/archive/helper_functions.R"))
{
# specialities <- speciality
speciality = c("Urology")
wl_type = c("Inpatient_wl", "Planned")

###### PARAMS #####
date_from = c(Sys.Date() - 60 - 3)
date_to = c(Sys.Date() - 3)
labels = month(date_to, label = T)

parameters <-
  data.frame(
    speciality = NA,
    time_period = NA,
    metric = NA,
    list = NA,
    month_mean = NA
  )

parameters_lw <-
  data.frame(
    speciality = NA,
    time_period = NA,
    metric = NA,
    list = NA,
    month_mean = NA
  )

# use data_mutate from sourced-wl_cleaning
# filter to smaller list 
data_recent <- data_mutate %>%
  mutate(decision_to_admit_date_dt = 
           parse_date_time(c(decision_to_admit_date_dt), 
                           orders = c('dmy_HMS', "dmy"))) %>%
  filter(decision_to_admit_date_dt > (train_halt - 60)) %>%
  mutate(across(.cols = c(admission_date_dt, removed_date), .fns = dmy),
         across(.cols = c(RTT_Start_Date), .fns = dmy_hms)) %>%
  filter(covid_recovery_priority != "Deferred") %>%
  select(date, RTT_Start_Date, decision_to_admit_date_dt, admission_date_dt, 
         removed_date, rtt_days_wait, rott, spec_desc, internal_number, pathway_number,
         covid_recovery_priority, wm52, wm65) %>%
  mutate(rtt_days_wait = if_else(rtt_days_wait < 0, 0, rtt_days_wait)) %>%
  rename(dt = decision_to_admit_date_dt,
         speciality = spec_desc) #%>%
  group_by(RTT_Start_Date, dt, admission_date_dt, 
           removed_date, rott, speciality, internal_number, pathway_number,
           covid_recovery_priority, wm52, wm65) %>%
    summarise(
      rtt_days_wait = max(rtt_days_wait)) %>%
  group_by(covid_recovery_priority) %>%
  summarise(count = n())

# Number of DTAs during the period. For demand we just need number of DTAs per day of the period.
for (j in speciality){
  
  days <-
    data.frame(dt = seq(
      from = (train_halt - 30),
      to = train_halt,
      by = 1
    ))
  wl_type = c("Inpatient_wl", "Planned")
  demand_daily <- data_recent %>%
    filter(speciality == j,
           covid_recovery_priority != "Unknown") %>%
    group_by(covid_recovery_priority, dt) %>%
    summarise(dtas = n()) %>%
    ungroup(.) %>%
    group_by(covid_recovery_priority) %>%
    pad() %>%
    mutate(dtas = if_else(is.na(dtas), 0, as.numeric(dtas))) %>%
    filter(dt >= (train_halt - 30))

    
  for (wl in wl_type) {
    if (!wl %in% demand_daily$covid_recovery_priority) {
      demand_daily <- rbind(
        demand_daily,
        data.frame(covid_recovery_priority = wl,
                   dt = train_halt,
                   dtas = 0)
      )
    }
  }
    demand_mean <- demand_daily %>%
      group_by(covid_recovery_priority) %>%
      summarise(month_mean = sum(dtas)/30)
    
    ggplot(data = demand_daily, aes(x = dt, y = dtas, colour = covid_recovery_priority)) +
      geom_smooth(se = F)
    
    parameters <- rbind(
      parameters,
      data.frame(
        speciality = j,
        time_period = labels[d],
        metric = "demand",
        list = demand_mean$covid_recovery_priority,
        month_mean = demand_mean$month_mean
      )
    )
}

# NEED TO GO BACK TO DRAWING BOARD FOR CAPACITY... do I go back to original QVD and remake the dataframe? This will also help with Snowflake transition
# Capacity: this is the number of people admitted during the period
for (j in speciality){
capacity_daily <- data_recent %>%
  mutate(admitted = if_else(is.na(admission_date_dt) | is.na(removed_date), 0, 1)) %>%
  filter(admitted == 1,
         speciality == j) %>% 
  group_by(covid_recovery_priority, admission_date_dt) %>%
  summarise(capacity = n()) %>%
  ungroup() %>%
  group_by(covid_recovery_priority) %>%
  pad() %>%
  mutate(capacity = if_else(is.na(capacity), 0, as.numeric(capacity))) %>%
  filter(admission_date_dt >= (train_halt - 30))

for (wl in wl_type) {
  if (!wl %in% capacity_daily$covid_recovery_priority) {
    demand_daily <- rbind(
      capacity_daily,
      data.frame(covid_recovery_priority = wl,
                 removed_date = train_halt,
                 capacity = 0)
    )
  }
}
capacity_mean <- capacity_daily %>%
  group_by(covid_recovery_priority) %>%
  summarise(month_mean = sum(capacity)/30)

ggplot(data = capacity_daily, aes(x = admission_date_dt, y = capacity, colour = covid_recovery_priority)) +
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