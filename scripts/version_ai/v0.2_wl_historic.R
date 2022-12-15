library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)


source("db_connect_functions.R")
source("helper_functions.R")
complete_update = FALSE

# Specialties and Divisions

specialties <- "Colorectal Surgery"
specialty_names = "Colorectal Surgery"
specialty = "Colorectal Surgery"
divisions = "Surgery"

###### PARAMS #####
date_from = c(Sys.Date() - 90 - 3)
date_to = c(Sys.Date() - 3)
labels = c("current")
  
  times_to_dta <- data.frame(
    division = NA,
    specialty = NA,
    time_period = NA,
    priority = NA,
    days = NA,
    prob = NA
  )
  
  times_to_planned <- data.frame(
    division = NA,
    specialty = NA,
    time_period = NA,
    priority = 'Planned',
    months = NA,
    prob = NA
  )
  
for (d in 1:length(date_from)) {
    #########################
    ######### DTAs ##########
    #########################
    q_dtas <- paste0(
      "WITH priorities AS (
SELECT DISTINCT
internal_number,
MIN(i.priority_local_code) AS covid_recovery_priority
FROM [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] i
where snapshot_date_dt > CAST('2022-01-01' AS DATE)
GROUP BY internal_number)
SELECT DISTINCT
i.internal_number
, i.PathwayNumber as pathway_number
, i.[decision_to_admit_date_dt] as dta_date
, i.Admis_Method_Desc
, CASE when i.Admis_Method_Desc = 'ELECTIVE PLANNED' THEN 'Planned' ELSE priorities.covid_recovery_priority END as covid_recovery_priority
, rtt.clock_start_date
, rtt.clock_stop_date
, DATEDIFF(DAY, rtt.clock_start_date, i.[decision_to_admit_date_dt]) as days_to_dta
, i.WLApproxAdmissionDate 
, i.tci_date_dt
, i.Admission_Reason
FROM [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] i
LEFT JOIN (SELECT pathway_number, clock_start_date, clock_stop_date FROM [nhs_reporting].[dbo].[reporting_rtt_pathway_summary] WHERE run_type = 'today' AND pathway_status <> 'not applicable') rtt ON i.PathwayNumber = rtt.pathway_number
LEFT JOIN priorities on priorities.internal_number  = i.internal_number
WHERE i.Admis_Method_Desc IN ('ELECTIVE PLANNED', 'ELECTIVE WAITING LIST', 'ELECTIVE BOOKED')
AND waiting_list_type = 'SNAP'
AND spec_desc = '",
      specialty,
      "'
AND decision_to_admit_date_dt BETWEEN CAST('",
      date_from[d],
      "' AS DATE) AND CAST('",
      date_to[d],
      "' AS DATE);"
    )
    
    dtas <-
      hsql(q = q_dtas, db = "nhs_reporting", server = "WWLDWSQL1") %>%
      mutate(decision_to_admit_date_dt = as.Date(dta_date, format = "%Y-%m-%d")) %>%
      mutate(clock_start_date = as.Date(clock_start_date, format = "%Y-%m-%d")) %>%
      mutate(dta_date = as.Date(dta_date, format = "%Y-%m-%d")) %>%
      mutate(clock_stop_date = as.Date(clock_stop_date)) %>%
      mutate(clock_start_date = as.Date(clock_start_date)) %>%
      mutate(clock_start_date = ifelse(Admis_Method_Desc %in% c("ELECTIVE WAITING LIST", "ELECTIVE BOOKED") & is.na(clock_start_date), dta_date, clock_start_date)) %>%
      mutate(clock_start_date = ifelse(Admis_Method_Desc %in% c("ELECTIVE WAITING LIST", "ELECTIVE BOOKED") & (clock_stop_date < dta_date), dta_date, clock_start_date)) %>%
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
      mutate(covid_recovery_priority = ifelse(covid_recovery_priority == "Priority 1", "Priority 2", covid_recovery_priority))
      
    # Check for any duplicates
    dtas <-
      dtas %>% group_by(
        internal_number,
        pathway_number,
        dta_date,
        Admis_Method_Desc,
        covid_recovery_priority,
        clock_start_date,
        clock_stop_date,
        days_to_dta,
        decision_to_admit_date_dt
      ) %>%
      summarise(
        WLApproxAdmissionDate = max(WLApproxAdmissionDate),
        tci_date_dt = max(tci_date_dt),
        Admission_Reason = paste(Admission_Reason, collapse = " ")
      ) %>%
      ungroup()
    
    
    ### daily average
    days <-
      data.frame(dt = seq(
        from = date_from[d],
        to = date_to[d],
        by = 1
      ))
    dta_daily <- dtas  %>%
      group_by(decision_to_admit_date_dt) %>%
      summarise(dtas = n()) %>%
      ungroup() %>%
      rename(dt = decision_to_admit_date_dt) %>%
      right_join(days) %>%
      mutate(dtas = ifelse(is.na(dtas), 0, dtas))
    
    ggplot(data = dta_daily, aes(x = dt, y = dtas)) +
      geom_line()
    
    (daily_av = mean(dta_daily$dtas))
    sqrt(var(dta_daily$dtas))
    
    
    if (date_from[d] >= as.Date("2022-09-01")) {
      ## priorities
      total = dim(dplyr::filter(
        dtas,
        covid_recovery_priority %in% c(
          "Unknown",
          "Planned",
          "Priority 2",
          "Priority 3",
          "Priority 4"
        )
      ))[1]
      
      if (total > 0){
        (
          demand_priority_splits <- dtas %>%
            dplyr::filter(
              covid_recovery_priority %in% c(
                "Unknown",
                "Planned",
                "Priority 2",
                "Priority 3",
                "Priority 4"
              )
            ) %>%
            group_by(covid_recovery_priority) %>%
            summarise(prop = n() / total) %>%
            ungroup()
        )
        
        ################
        ## days to DTA #
        ################
        
        p2_dtas <-
          dplyr::filter(dtas, covid_recovery_priority == "Priority 2")
        p2_dtas_table <- table(p2_dtas$days_to_dta) / dim(p2_dtas)[1]
        
        if (dim(p2_dtas)[1] != 0) {
          times_to_dta <- rbind(
            times_to_dta,
            data.frame(
              division = division,
              specialty = specialty,
              time_period = labels[d],
              priority = c("Priority 2"),
              days = as.numeric(names(p2_dtas_table)),
              prob = c(p2_dtas_table)
            )
          )
        }
        
        p3_dtas <-
          dplyr::filter(dtas, covid_recovery_priority == "Priority 3")
        p3_dtas_table <- table(p3_dtas$days_to_dta) / dim(p3_dtas)[1]
        
        if (dim(p3_dtas)[1] != 0) {
          times_to_dta <- rbind(
            times_to_dta,
            data.frame(
              division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
              specialty = specialty,
              time_period = labels[d],
              priority = c("Priority 3"),
              days = as.numeric(names(p3_dtas_table)),
              prob = c(p3_dtas_table)
            )
          )
        }
        
        p4_dtas <-
          dplyr::filter(dtas, covid_recovery_priority == "Priority 4")
        p4_dtas_table <- table(p4_dtas$days_to_dta) / dim(p4_dtas)[1]
        
        if (dim(p4_dtas)[1] != 0) {
          times_to_dta <- rbind(
            times_to_dta,
            data.frame(
              division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
              specialty = specialty,
              time_period = labels[d],
              priority = c("Priority 4"),
              days = as.numeric(names(p4_dtas_table)),
              prob = c(p4_dtas_table)
            )
          )
        }
        
        unknown_dtas <-
          dplyr::filter(dtas, covid_recovery_priority == "Unknown")
        unknown_dtas_table <-
          table(unknown_dtas$days_to_dta) / dim(unknown_dtas)[1]
        
        if (dim(unknown_dtas)[1] != 0) {
          times_to_dta <- rbind(
            times_to_dta,
            data.frame(
              division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
              specialty = specialty,
              time_period = labels[d],
              priority = c("Unknown"),
              days = as.numeric(names(unknown_dtas_table)),
              prob = c(unknown_dtas_table)
            )
          )
        }
      }
    }}
  