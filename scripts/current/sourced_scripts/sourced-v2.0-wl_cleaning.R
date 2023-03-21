#####
# Engineer raw data to include all relevant info for parameters
data_param <- data %>%
  mutate(., decision_to_admit_date_dt = 
           parse_date_time(c(decision_to_admit_date_dt), 
                           orders = c('dmy_HMS', "dmy"))) %>%
  mutate(., across(.cols = c(removed_date_dt, tci_date_dt, snapshot_date_dt,
                             RTT_Start_Date), .fns = dmy_hms),
         across(.cols = c(admission_date_dt), .fns = dmy)) %>%
  filter(., date(snapshot_date_dt) > (Sys.Date() - 365),
         Admis_Method_Desc %in% c("ELECTIVE PLANNED", "ELECTIVE WAITING LIST", 
                                  "ELECTIVE BOOKED")) %>%
  mutate(., admission_date_dt = if_else(is.na(admission_date_dt) & 
                                          (tci_date_dt == removed_date_dt), 
                                        ymd(tci_date_dt), 
                                        ymd(admission_date_dt)),
         ., rtt_days_wait = ifelse(Admis_Method_Desc == "ELECTIVE PLANNED", NA, 
                                   date(snapshot_date_dt) - 
                                     date(RTT_Start_Date))) %>%
  mutate(., wm52 = if_else(Admis_Method_Desc == "ELECTIVE PLANNED", 0,
                           if_else(Admis_Method_Desc != "ELECTIVE PLANNED" & 
                                     rtt_days_wait >= 365, 1, 0)),
         ., wm65 = if_else(Admis_Method_Desc == "ELECTIVE PLANNED", 0,
                           if_else(Admis_Method_Desc != "ELECTIVE PLANNED" & 
                                     rtt_days_wait >= 455, 1, 0))) %>%
  select(., snapshot_date_dt, decision_to_admit_date_dt, admission_date_dt, 
         removed_date_dt, spec_desc, priority_local_code,
         internal_number, PathwayNumber, Admis_Method_Desc, 
         RTT_Start_Date, rtt_days_wait, contains("wm")) %>%
  mutate(., priority_local_code = ifelse(Admis_Method_Desc == 
                                           "ELECTIVE PLANNED", 'Planned', 
                                         glue("Priority 
                                               {priority_local_code}")),
         priority_local_code = ifelse(grepl("NA", priority_local_code), 
                                      "Inpatient_wl", priority_local_code),
         rott = ifelse(is.na(admission_date_dt) & !is.na(removed_date_dt), 
                       1, 0)) %>%
  rename("wl" = priority_local_code,
         "date" = snapshot_date_dt,
         "speciality" = spec_desc)


# Pull speciality names into vector for looping through
speciality <- data_param %>%
  distinct(speciality) %>%
  pull(speciality)

# Overall WL (Inpatient, planned, removed)
o_wl <- data_param %>%
  mutate(wl = if_else(grepl("[[:digit:]]", wl), "Inpatient_wl", wl)) %>%
  group_split(wl) %>%
  map(. %>%
        group_by(date, speciality, wl) %>%
        summarise(patients = n(), .groups = "drop_last") %>%
        ungroup(.))

# 52 & 65 week waiter WL. Only Inpatient_wl
lw_wl <- data_param %>%
  filter(., (wm52 == 1 | wm65 == 1) &
           grepl("[[:digit:]]", wl)) %>%
  mutate(., weeks = if_else(wm52 == 1 & wm65 == 1, "weeks_65", "weeks_52")) %>%
  group_by(date, speciality, weeks) %>%
  summarise(patients = n(), .groups = "drop_last") %>%
  ungroup(.) %>%
  rename("wl" = weeks)

# Bind data.frames of all groups together
wl_comp <- bind_rows(o_wl, lw_wl)

# Pull type of waiting list into vector for looping through
wl_type <- wl_comp %>%
  distinct(wl) %>%
  filter(., wl != c("Removed")) %>%
  pull(wl)

# clean up global env
rm(data, o_wl, lw_wl)
