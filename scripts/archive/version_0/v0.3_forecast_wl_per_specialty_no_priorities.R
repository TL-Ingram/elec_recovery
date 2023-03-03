# specialty <- c("Urology", "Cardiology", "Trauma & Orthopaedics", 
#                "Ophthalmology",
#                "Colorectal Surgery", "Gastroenterology", "Pain Management", 
#                "Rheumatology",
#                "Haematology", "Oral Surgery", "General Surgery", 
#                "Vascular Surgery", "Endocrinology and Metabolic Medicine",
#                "General Medicine", "Gynaecology", "Elderly Medicine", 
#                "Thoracic Medicine", "Plastic Surgery", "ENT", "Breast Surgery", 
#                "Paediatric Dentistry")
specialty <- q %>%
  distinct(specialty) %>%
  pull(specialty)
# setwd("C:/R_projects/elec_recovery")
# specialty_name = "Ophthalmology"
# specialty_name = "Trauma & Orthopaedics"
# test <- function(q, specialty_name)
for (specialty_name in specialty) {
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
source("simulation_function.R")
source("helper_functions.R")

  

# load data --------------------------------------------------------------------
load(here("data", "Current_Waiting_List.RData"))
init_date <- as.Date(max(q$decision_to_admit_date_dt), origin = "1900-01-01") - 50
print(paste0("Initialisation date is ", init_date))

# Set end date
end_date = as.Date("2024-01-01")
print(paste0("End date is ", end_date))

# Calculate Horizon Length
h <- as.numeric(end_date - init_date)
print(paste0("Horizon length is  ", h))

# Read in the parameters which drive the model
q_params <-
  paste0("SELECT * FROM elective_recovery_params where creator = 'system' and time_period = 'current' and specialty = '", specialty_name, "'")
parameters <-
  unique(hsql(q_params, db = "nhs_datascience", server = "wwldevsqlfrm1"))
print(paste0("Parameters have been loaded."))

q_times_to_dta <- paste0("SELECT * FROM elective_recovery_times_to_dta where time_period = 'current' and specialty = '", specialty_name, "'")
times_to_dta <-
  hsql(q_times_to_dta, db = "nhs_datascience", server = "wwldevsqlfrm1")
print(paste0("Times to DTA have been loaded."))

q_times_to_planned <- paste0("SELECT * FROM elective_recovery_times_to_planned where time_period = 'current' and specialty = '", specialty_name, "'")
times_to_planned <-
  hsql(q_times_to_planned, db = "nhs_datascience", server = "wwldevsqlfrm1")
print(paste0("Times to planned have been loaded."))

###################################
# Set up the current waiting list #
###################################
wl <- q %>%  mutate_all(list(~na_if(.,""))) %>%
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
    wl$current_wl_priority %in% c('1', '2', '3', '4'),
    'Priority',
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

print(paste0("Waiting list has been formatted for ", specialty_name))

# ------------------------------------------------------------------------------

#########
# START #
#########
type = c("current_rates")
print(paste0("The ", type, " model has been selected."))
if(type == "current_rates")

  
  
  ################
  # Times to DTA
  ###############
  # times until dta
  demand_waits <-
    dplyr::filter(times_to_dta)[c("priority", "days", "prob")]
  
  # Times to DTA
  times_to_dta_spec <-
    dplyr::filter(times_to_dta,
                  time_period == "current")
  
  # Times to Planned
  times_to_planned_spec <-
    dplyr::filter(times_to_planned,
                  time_period == "current")
  
  # print(paste0("Parameters have been filtered to only include ", specialty_name))
  
  ##############
  # Capacity
  ##############
  capacity <-
    rep(
      filter(
        parameters,
        time_period == "current",
        metric == "capacity_mean"
      )$value,
      h
    )
  print(paste0("Capacity is ", capacity[1]))
  
  capacity_priority_splits <-
    dplyr::filter(
      parameters,
      metric == "capacity_priority_splits",
      time_period == "current",
      priority %in% c("Planned", "Unknown", "Priority 2", "Priority 3", "Priority 4")
    )[c("priority", "value")] %>%
    full_join(data.frame(priority = c("Priority 2","Priority 3", "Priority 4", "Unknown", "Planned"))) %>%
    mutate(value = ifelse(is.na(value), 0, value))#P2,P3,P4
  
  capacity_priority_splits$priority <-
    as.factor(x = as.character(capacity_priority_splits$priority))
  
  ##############
  # ROTT
  ##############
  rott <-
    rep(
      filter(
        parameters,
        time_period == 'current',
        metric == 'rott_mean'
      )$value,
      h
    )
  
  ##########
  # DTAs
  ###########
  demand <-
    rep(
      filter(
        parameters,
        time_period == "current",
        metric == "demand_mean"
      )$value,
      h
    )
  print(paste0("Demand is ", demand[1]))
  
  demand_priority_splits <-
    dplyr::filter(
      parameters,
      metric == "demand_priority_splits",
      time_period == "current",
      priority %in% c("Unknown", "Planned", "Priority 2", "Priority 3", "Priority 4")
    )[c("priority", "value")]%>%
    full_join(data.frame(priority = c("Priority 2","Priority 3", "Priority 4", "Unknown", "Planned"))) %>%
    mutate(value = ifelse(is.na(value), 0, value))
  
  
  
  #############################
  #### Run the Simulation #####
  #############################
  
  if(dim(wl)[1] != 0)
    
    
    
    res <- data.frame(
      id = 1:dim(wl)[1],
      waits = as.numeric(wl$total_days_wait),
      inpatient_waits = as.numeric(wl$days_wait),
      priority = wl$priority,
      priority_definition = wl$priority_definition,
      planned  = as.numeric(wl$planned_admission_date - init_date),
      stringsAsFactors = FALSE
    )
  
  
  
  initial.waiting.list<-dim(res)[1]
  print(paste0("The initial waiting list size is of length ", initial.waiting.list))
  
  n.runs <- 1 # number of runs of the simulation
  warm.up.period <- 1 # warm up period (discarded)
  
  
  print(paste0("Number of runs is ", n.runs))
  
  print(paste0("Setting up the cluster..."))
  cl<-makeCluster(8)
  print(paste0("Made cluster..."))
  clusterExport(cl=cl,
                varlist=c("demand","capacity","capacity_priority_splits","demand_priority_splits","rott","times_to_dta_spec","times_to_planned_spec","initial.waiting.list","res"),
                envir=environment())
  print(paste0("Exported to cluster..."))
  clusterEvalQ(cl, library("dplyr"))
  print(paste0("Evaluated dplyr on cluster..."))
  res<-parLapply(cl,1:n.runs,simfn)
  print(paste0("Got results from cluster..."))
  stopCluster(cl)
  print(paste0("Stopped the cluster..."))
  res<-do.call("rbind",res)
  print(paste0("Results obtained from simulations on the cluster...."))
  
  #################
  ### OUTPUTS #####
  #################
  
  res.sum<-res %>%
    mutate(nwl52_total = rowSums(select(., contains('nwl52'))),
           nwm52_total = rowSums(select(., contains('nwm52'))),
           nwm4_total = rowSums(select(., contains('nwm4'))),
           nwm13_total = rowSums(select(., contains('nwm13'))),
           nwm104_total = rowSums(select(., contains('nwm104'))),
           nwl104_total = rowSums(select(., contains('nwl104'))),
           nwm78_total = rtt_nwm78_total) %>%
    select(-c(2:10, 12:18)) %>%
    select(-c(3,4,5,6,7)) %>%
    select(-c(10,11)) %>%
    pivot_longer(cols=-c(day,ref),names_to="metric",values_to="value") %>%
    group_by(day,metric) %>%
    summarise(mean=mean(value, na.rm=T),q025=quantile(value,0.05,na.rm=TRUE),q975=quantile(value,0.95,na.rm=TRUE))
  
  res.sum$date <- as.character(init_date + res.sum$day, format = "%d/%m/%Y")

# overall, not priorities
# remove more than 4 and 13
# wl_size, not planned or deferred
# group of more than 78 as well. This is a big one
# interactivity to modify admissions, 
# clearance not work
  
  # Write to table
  # Convert doubles to floats
  res.sum <- filter(res.sum, !is.na(mean))
  res.sum$specialty = specialty_name
           
           
  
  
  print(paste0("Calculating clearance times... "))
  # Calculate clearence times
  get_clear <- function(data, metric_name = "rtt_nwm52_P2"){
    data_filtered <- filter(data, metric == metric_name)
    clear_dates <- rep(NA,3)
    names <- c("q025","mean","q975")
    results <- data.frame(type = metric_name, quantile = NA, date = NA)
    for (d in 1:3){
      #get first non-zero from the back
      data_filtered_type <- data_filtered[,names(data_filtered) %in% c("date",names[d])]
      # Reverse the data and get the location of the first non zero entry
      point <- which(rev(data_filtered_type[,1][[1]]) > 1)[1]
      
      if (is.na(point)){
        date = data_filtered_type[,2][[1]][1]
      }else if (point == 1){
        date = NA
      }else{
        date <- rev(data_filtered_type[,2][[1]])[point]
      }
      results <- results %>% rbind(data.frame(type = metric_name, quantile = names[d], date = date))
    }
    return(results[-1,])
  }
  
  clear_metrics <- c("nwm52_total","nwm78_total", "nwm104_total")
  
  
  results <- get_clear(data = res.sum, metric_name = clear_metrics[1])
  
  for(metric in clear_metrics[-1]){
    results <- rbind(results, get_clear(data = res.sum, metric_name = metric))
  }

  # Write to table
  print(paste0("Removing old clearance times..."))
  results <- pivot_wider(results, names_from = quantile, values_from = date)
  
  write_rds(res.sum, paste0(here("rds", "forecast_horizon", specialty_name), ".rds"))
  write_csv(res.sum, paste0(here("csv", "forecast_horizon", specialty_name), ".csv"))
  write_rds(results, paste0(here("rds", "clearance_times", specialty_name), ".rds"))
  
  print(results)

}
# test(q, "Trauma & Orthopaedics")

