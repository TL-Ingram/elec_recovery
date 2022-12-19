# specialty <- c("Urology", "Cardiology", "Trauma & Orthopaedics", 
#                "Ophthalmology",
#                "Colorectal Surgery", "Gastroenterology", "Pain Management", 
#                "Rheumatology",
#                "Haematology", "Oral Surgery", "General Surgery", 
#                "Vascular Surgery", "Endocrinology and Metabolic Medicine",
#                "General Medicine", "Gynaecology", "Elderly Medicine", 
#                "Thoracic Medicine", "Plastic Surgery", "ENT", "Breast Surgery", 
#                "Paediatric Dentistry")
# specialty <- q %>%
#   distinct(specialty) %>%
#   pull(specialty)
# setwd("C:/R_projects/elec_recovery")
specialty_name = "Colorectal Surgery"
# specialty_name = "Trauma & Orthopaedics"
# # test <- function(q, specialty_name)
# for (specialty_name in specialty) {
  # packages ---------------------------------------------------------------------
  # librarian::lib_startup("librarian", global = FALSE)
  # library(librarian)
  shelf(tidyverse, here, timeDate, parallel, lubridate, odbc)
  # library(parallel)
  # library(dplyr)
  # library(ggplot2)
  # library(tidyr)
  # library(timeDate)
  # library(stringr)
  # library(lubridate)
  source("db_connect_functions.R")
  
  
  
  # load data --------------------------------------------------------------------
  load(here("data", "Current_Waiting_List.RData"))
  init_date <- as.Date(max(q$decision_to_admit_date_dt), origin = "1900-01-01") - 2
  print(paste0("Initialisation date is ", init_date))
  
  # Set end date
  end_date = as.Date("2023-01-01")
  print(paste0("End date is ", end_date))
  
  # Calculate Horizon Length
  h <- as.numeric(end_date - init_date)
  print(paste0("Horizon length is  ", h))

  ###################################
  # Set up the current waiting list #
  ###################################
  wl <- q %>%  mutate_all(list(~na_if(., ""))) %>%
    mutate_all(list(~na_if(.,"NaN"))) %>%
    mutate(WLApproxAdmissionDate = as.Date(WLApproxAdmissionDate, origin = "1900-01-01") - 2 ) %>%
    mutate(tci_date_dt = as.Date(tci_date_dt, origin = "1900-01-01") - 2) %>%
    mutate(decision_to_admit_date_dt = as.Date(decision_to_admit_date_dt, origin = "1900-01-01") - 2) %>%
    mutate(rtt_weeks_wait = ifelse(is.na(rtt_weeks_wait), inp_wl_weeks_wait, rtt_weeks_wait)) %>%
    filter(specialty == specialty_name)
  
  
  wl$days_wait <- as.numeric(wl$inp_wl_weeks_wait) * 7
  wl$total_days_wait <-  as.numeric(wl$rtt_weeks_wait) * 7
  
  
  wl$priority_definition = wl$priority
  wl$priority  = ifelse(
    wl$current_wl_priority %in% c(8, 9),
    'Deferred',
    ifelse(
      wl$current_wl_priority %in% c('1', '2'),
      'Priority 2', # why did this change matter?
      ifelse(
        wl$current_wl_priority %in% c('Unknown', '7'),
        'Unknown',
        paste0("Priority ", wl$current_wl_priority)
      )
    )
  )
  # ------------------------------------------------------------------------------
  
  wl$planned_month = apply(wl[,"WLApproxAdmissionDate", drop = F], FUN = month, MARGIN = 1)
  wl$planned_year = apply(wl[,"WLApproxAdmissionDate", drop = F], FUN = year, MARGIN = 1)
  wl$inferred_planned_admission_date = make_date(year = as.numeric(wl$planned_year), month = as.numeric(wl$planned_month), day = 28)
  
  wl <- wl %>%
    mutate(planned_admission_date = ifelse(
      is.na(tci_date_dt),
      pmax(WLApproxAdmissionDate, inferred_planned_admission_date, na.rm = T),
      tci_date_dt
    )) %>%
    mutate(
      planned_admission_date = as.Date(planned_admission_date, origin = '1970-01-01')
    )
  
  # If a planned patient has no planned date, set it to be 2 years after the dta:
  wl$planned_admission_date = as.Date(
    ifelse(
      wl$priority_definition == 'Planned' &
        is.na(wl$planned_admission_date),
      as.Date(wl$decision_to_admit_date_dt + 365*2),
      as.Date(wl$planned_admission_date)
    ),
    origin = "1970-01-01"
  )

wl_model_rdy <- wl %>%
  mutate(., planned_date = planned_admission_date - init_date) %>%
  select(., specialty,
         days_wait,
         priority,
         priority_definition,
         planned_date)


# id = 1:dim(wl)[1],
# waits = as.numeric(wl$total_days_wait),
# inpatient_waits = as.numeric(wl$days_wait),
# priority = wl$priority,
# priority_definition = wl$priority_definition,
# planned  = as.numeric(wl$planned_admission_date - init_date),
# stringsAsFactors = FALSE
  
  
  
  print(paste0("Waiting list has been formatted for ", specialty_name))
  