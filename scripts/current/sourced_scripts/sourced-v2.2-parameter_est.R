source(here("scripts/archive/db_connect_functions.R"))
source(here("scripts/archive/helper_functions.R"))
{
# specialities <- speciality
speciality = c("Urology")
wl_type = c("Inpatient_wl", "Planned")

###### PARAMS #####
labels = month(train_halt, label = T)

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
data_recent <- data_mod %>%
  filter(decision_to_admit_date_dt >= (train_halt - 30)) %>%
  group_by(RTT_Start_Date, decision_to_admit_date_dt, admission_date_dt, 
           removed_date_dt, rott, speciality, internal_number, PathwayNumber,
           wl, wm52, wm65) %>%
  summarise(
    rtt_days_wait = max(rtt_days_wait), .groups = "drop_last")

# Number of DTAs during the period. For demand we just need number of DTAs per day of the period.
i = "Inpatient_wl"
for(i in wl_type) {
  for (j in speciality){
  
  days <-
    data.frame(decision_to_admit_date_dt = seq(
      from = (train_halt - 30),
      to = train_halt,
      by = 1
    ))
  demand_daily <- data_recent %>%
    filter(speciality == j,
           wl == i) %>%
    group_by(wl, decision_to_admit_date_dt) %>%
    summarise(demand = n()) %>%
    ungroup(.) %>%
    right_join(days, by = "decision_to_admit_date_dt") %>%
    pad() %>%
    mutate(demand = if_else(is.na(demand), 0, as.numeric(demand)),
           wl = if_else(is.na(wl), i, wl))
    
  # for (wl in wl_type) {
  #   if (!wl %in% demand_daily$wl) {
  #     demand_daily <- rbind(
  #       demand_daily,
  #       data.frame(covid_recovery_priority = wl,
  #                  dt = train_halt,
  #                  dtas = 0)
  #     )
  #   }
  # }
    demand_mean <- demand_daily %>%
      summarise(month_mean = sum(demand)/30)
    
    ggplot(data = demand_daily, aes(x = decision_to_admit_date_dt, y = demand,
                                    colour = wl)) +
      geom_smooth(se = F)
    
    parameters <- rbind(
      parameters,
      data.frame(
        speciality = j,
        time_period = labels[1],
        metric = "demand",
        list = i,
        month_mean = demand_mean$month_mean
      )
    )
}
# make the above into something that pulls into a list and then outputs to parameters df
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