source(here("scripts/archive/db_connect_functions.R"))
source(here("scripts/archive/helper_functions.R"))

# specialities <- speciality
speciality = c("Trauma & Orthopaedics")
wl_type = c("Inpatient_wl", "Planned")

###### PARAMS #####
date_from = c(Sys.Date() - 60 - 3)
date_to = c(Sys.Date() - 3)
labels = month(date_to, label = T)

parameters <-
  data.frame(
    specialty = NA,
    time_period = NA,
    metric = NA,
    list = NA,
    month_mean = NA
  )

parameters_lw <-
  data.frame(
    specialty = NA,
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
         speciality = spec_desc) %>%
  group_by(RTT_Start_Date, dt, admission_date_dt, 
           removed_date, rott, speciality, internal_number, pathway_number,
           covid_recovery_priority, wm52, wm65) %>%
    summarise(
      rtt_days_wait = max(rtt_days_wait))

# Number of DTAs during the period. For demand we just need number of DTAs per day of the period.
for (i in wl_type){
  demand <- data_recent %>%
    filter(covid_recovery_priority == i) 
    
    ### daily average
    days <-
      data.frame(dt = seq(
        from = (train_halt - 30),
        to = train_halt,
        by = 1
      ))
    dta_daily <- data_recent  %>%
      group_by(covid_recovery_priority, speciality, dt) %>%
      summarise(dtas = n()) %>%
      ungroup(.) %>%
      right_join(days) %>%
      group_by(covid_recovery_priority, speciality) %>%
      pad() %>%
      ungroup(.) %>%
      mutate(dtas = if_else(is.na(dtas), 0, as.numeric(dtas)))
    
    for (j in speciality) {
      if (!j %in% dta_daily$speciality) {
        dta_daily <- rbind(
          dta_daily,
          data.frame(covid_recovery_priority = "Planned",
                     speciality = j,
                     dt = train_halt,
                     dtas = 0)
        )
      }
    }
    
    dta_mean <- dta_daily %>%
      group_by(covid_recovery_priority, speciality) %>%
      summarise(month_mean = sum(dtas)/30)
    
    ggplot(data = dta_daily, aes(x = dt, y = dtas, colour = covid_recovery_priority)) +
      geom_smooth(se = F)
    


inv_unk <- data_recent %>%
  filter(covid_recovery_priority == "Unknown") %>%
  group_by(pathway_number) %>%
  summarise(n = n())
  group_by(is.na(decision_to_admit_date_dt), is.na(admission_date_dt), is.na(removed_date)) #%>%
  # summarise(n = n())
  # filter(covid_recovery_priority == c("Planned", "Inpatient_wl")) %>%
  group_by(
    internal_number,
    pathway_number,
    decision_to_admit_date_dt,
    admission_date_dt,
    removed_date,
    Admis_Method_Desc,
    covid_recovery_priority
  ) %>%
  summarise(
    rtt_days_wait = max(rtt_days_wait)
  ) %>%
  ungroup()
  

dtas <- dtas %>%
  mutate(decision_to_admit_date_dt = date(dta_date)) %>%
  mutate(clock_start_date = date(clock_start_date)) %>%
  mutate(dta_date = date(dta_date)) %>%
  mutate(clock_stop_date = date(clock_stop_date)) %>%
  mutate(clock_start_date = if_else(Admis_Method_Desc %in% c("ELECTIVE WAITING LIST", "ELECTIVE BOOKED") & is.na(clock_start_date), dta_date, clock_start_date)) %>%
  mutate(clock_start_date = if_else(Admis_Method_Desc %in% c("ELECTIVE WAITING LIST", "ELECTIVE BOOKED") & (clock_stop_date < dta_date), dta_date, clock_start_date)) %>%
  mutate(days_to_dta = ifelse(Admis_Method_Desc == "ELECTIVE PLANNED", NA, dta_date - clock_start_date)) %>%
  mutate(days_to_dta = ifelse(days_to_dta < 0, 0, days_to_dta)) %>%
  mutate(days_to_dta = ifelse(is.na(days_to_dta) & Admis_Method_Desc %in% c("ELECTIVE WAITING LIST", "ELECTIVE BOOKED") , 0, days_to_dta)) %>%
  mutate(
    covid_recovery_priority = ifelse(
      covid_recovery_priority == 'Planned',
      'Planned',
      paste0("Priority ", covid_recovery_priority)
    )
  ) %>%
  mutate(covid_recovery_priority = ifelse(covid_recovery_priority == "Priority 1", "Priority 2", covid_recovery_priority)) %>%
  mutate(covid_recovery_priority = ifelse(grepl('Pri*', covid_recovery_priority), "Inpatient_wl", as.character(covid_recovery_priority)))
