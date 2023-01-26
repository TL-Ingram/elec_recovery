library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)


source("db_connect_functions.R")
source("helper_functions.R")
complete_update = FALSE


log_con <- file("inpatient_wl_params.log")
sink(log_con, append=TRUE)
sink(log_con, append=TRUE, type="message")

# Specialties and Divisions

q_spec <-
  "SELECT DISTINCT spec_desc, division from [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List];"
specialty_names = hsql(q = q_spec, db = "nhs_reporting", server = "WWLDWSQL1") %>% filter(spec_desc != "System Generated")
specialty_names$spec_desc <- trimws(specialty_names$spec_desc, which = c("right"))

specialties <- specialty_names$spec_desc
divisions = specialty_names$division

###### PARAMS #####
if (complete_update) {
  date_from = c(Sys.Date() - 90 - 3,
                seq(
                  from = as.Date("2022-09-01"),
                  to =  as.Date("2023-12-01"),
                  by = "month"
                ))
  date_to = c(Sys.Date() - 3, seq(
    from = as.Date("2022-09-01"),
    to =  as.Date("2023-12-01"),
    by = "month"
  ) - 1)
  
  labels = c("current", paste0(
    lubridate::month(date_from[-1], label = T),
    lubridate::year(date_from[-1])
  ))
} else{
  date_from = c(Sys.Date() - 90 - 3)
  date_to = c(Sys.Date() - 3)
  labels = c("current")
}


for (specialty in specialties){
  
  ##  Initialise data frames  ###
  parameters <-
    data.frame(
      division = NA,
      specialty = NA,
      time_period = NA,
      metric = NA,
      priority = NA,
      value = NA
    )
  
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
    
    parameters <- rbind(
      parameters,
      data.frame(
        division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
        specialty = specialty,
        time_period = labels[d],
        metric = "demand_mean",
        priority = "All",
        value = daily_av
      )
    )
    
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
        
        parameters <- rbind(
          parameters,
          data.frame(
            division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
            specialty = specialty,
            time_period = labels[d],
            metric = "demand_priority_splits",
            priority = demand_priority_splits$covid_recovery_priority,
            value = demand_priority_splits$prop
          )
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
              division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
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
      
      ##################################
      # Days to planned admission date #
      ##################################
      planned_dtas <- filter(dtas, Admis_Method_Desc == 'ELECTIVE PLANNED' )
      
      # Work out the planned admission date
      planned_dtas$planned_month = apply(planned_dtas[,"Admission_Reason", drop = F], FUN = get_month, MARGIN = 1)
      planned_dtas$planned_year = apply(planned_dtas[,"Admission_Reason", drop = F], FUN = get_year, MARGIN = 1)
      planned_dtas$inferred_planned_admission_date = make_date(year = as.numeric(planned_dtas$planned_year), month = as.numeric(planned_dtas$planned_month), day = 28)
      
      planned_dtas <- planned_dtas %>%
        mutate(planned_admission_date = ifelse(
          is.na(tci_date_dt),
          pmax(WLApproxAdmissionDate, inferred_planned_admission_date, na.rm = T),
          tci_date_dt
        )) %>%
        mutate(
          planned_admission_date = as.POSIXct(planned_admission_date, origin = '1970-01-01', format = "%Y-%m-%d")
        ) %>%
        mutate(
          planned_admission_date = as.Date(planned_admission_date, origin = '1970-01-01', format = "%Y-%m-%d")
        )
      
      # Now calculate the time between their DTA and date to be admitted
      planned_dtas$days_to_planned = as.numeric(planned_dtas$planned_admission_date - planned_dtas$decision_to_admit_date_dt)
      
      # Filter any missing or negative and then group into months
      days_to_planned <- filter(planned_dtas, !is.na(days_to_planned), days_to_planned > 0) %>%
        dplyr::select(days_to_planned) %>%
        mutate(months_to_planned = ceiling(days_to_planned / 31))
      
      months_to_planned = table(days_to_planned$months_to_planned)/dim(days_to_planned)[1]
      
      
      if (dim(days_to_planned)[1] != 0) {
        times_to_planned <- rbind(
          times_to_planned,
          data.frame(
            division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
            specialty = specialty,
            time_period = labels[d],
            priority = c("Planned"),
            months = as.numeric(names(months_to_planned)),
            prob = c(months_to_planned)
          )
        )
      }
      
    }
    
    
    #########################
    ######### adms ##########
    #########################
    
    
    q_adms <- paste0(
      "WITH priorities AS (
SELECT DISTINCT
internal_number,
MIN(i.priority_local_code) AS covid_recovery_priority
FROM [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] i
where snapshot_date_dt > CAST('2022-09-01' AS DATE)
GROUP BY internal_number)
SELECT DISTINCT
  i.internal_number
, i.PathwayNumber as pathway_number
, removed_date_dt as removed_date
, CASE WHEN ([admission_date_dt] IS NULL AND [tci_date_dt] = [removed_date_dt]) THEN [tci_date_dt] ELSE [admission_date_dt] END as admission_date
, CASE when i.Admis_Method_Desc = 'ELECTIVE PLANNED' THEN 'Planned' ELSE priorities.covid_recovery_priority END as covid_recovery_priority
FROM [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] i
LEFT JOIN priorities on priorities.internal_number  = i.internal_number
WHERE i.Admis_Method_Desc IN ('ELECTIVE PLANNED', 'ELECTIVE WAITING LIST', 'ELECTIVE BOOKED')
AND waiting_list_type = 'SNAP'
AND spec_desc = '",
      specialty,
      "'
AND removed_date_dt BETWEEN CAST('",
      date_from[d],
      "' AS DATE) AND CAST('",
      date_to[d],
      "' AS DATE);"
    )
    
    removals <-
      hsql(q = q_adms, db = "nhs_reporting", server = "WWLDWSQL1")
    
    # QUESTION: Do we take into account admissions which do not result in a removal? 
    adms <-
      removals %>%
      dplyr::filter(!is.na(admission_date)) %>%
      mutate(admission_date = as.Date(admission_date, format = "%Y-%m-%d")) %>%
      mutate(removed_date = as.Date(removed_date, format = "%Y-%m-%d")) %>%
      mutate(covid_recovery_priority = ifelse(
        covid_recovery_priority == 'Planned',
        'Planned',
        paste0("Priority ", covid_recovery_priority)
      )) %>%
      mutate(
        covid_recovery_priority = ifelse(
          covid_recovery_priority == "Priority 1",
          "Priority 2",
          covid_recovery_priority
        )
      ) %>%
      mutate(flag = ifelse(admission_date == removed_date, 1, 0)) %>%
      dplyr::filter(flag == 1) # ANSWER - we do now
    
    rott <-
      removals %>%
      mutate(flag = ifelse(is.na(admission_date),1, ifelse(admission_date!=removed_date,1,0) )) %>%  # ANSWER - we do now
      dplyr::filter(flag == 1) %>%
      mutate(removed_date = as.Date(removed_date, format = "%Y-%m-%d")) %>%
      mutate(
        covid_recovery_priority = ifelse(
          covid_recovery_priority == 'Planned',
          'Planned',
          paste0("Priority ", covid_recovery_priority)
        )
      )%>%
      mutate(covid_recovery_priority = ifelse(covid_recovery_priority == "Priority 1", "Priority 2", covid_recovery_priority))
    
    
    ### daily average
    days <-
      data.frame(dt = seq(
        from = date_from[d],
        to = date_to[d],
        by = 1
      ))
    
    #Admissions
    adms_daily <- adms  %>%
      group_by(admission_date) %>%
      summarise(adms = n()) %>%
      ungroup() %>%
      rename(dt = admission_date) %>%
      right_join(days) %>%
      mutate(adms = ifelse(is.na(adms), 0, adms))
    
    ggplot(data = adms_daily, aes(x = dt, y = adms)) +
      geom_line()
    
    (daily_av = mean(adms_daily$adms))
    
    parameters <- rbind(
      parameters,
      data.frame(
        division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
        specialty = specialty,
        time_period = labels[d],
        metric = "capacity_mean",
        priority = "All",
        value = daily_av
      )
    )
    
    #Removals for reasons other than treatment
    rott_daily <- rott  %>%
      group_by(removed_date) %>%
      summarise(rott = n()) %>%
      ungroup() %>%
      rename(dt = removed_date) %>%
      right_join(days) %>%
      mutate(rott = ifelse(is.na(rott), 0, rott))
    
    ggplot(data = rott_daily, aes(x = dt, y = rott)) +
      geom_line()
    
    (rott_daily_av = mean(rott_daily$rott))
    
    
    parameters <- rbind(
      parameters,
      data.frame(
        division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
        specialty = specialty,
        time_period = labels[d],
        metric = "rott_mean",
        priority = "All",
        value = rott_daily_av
      )
    )
    
    
    if (date_from[d] >= as.Date("2022-09-01")) {
      ## adms priorities
      total = dim(dplyr::filter(
        adms,
        covid_recovery_priority %in% c(
          "Planned",
          "Unknown",
          "Priority 2",
          "Priority 3",
          "Priority 4"
        )
      ))[1]
      
      if (total > 0){
        (
          capacity_priority_splits <- adms %>%
            dplyr::filter(
              covid_recovery_priority %in% c(
                "Planned",
                "Unknown",
                "Priority 2",
                "Priority 3",
                "Priority 4"
              )
            ) %>%
            group_by(covid_recovery_priority) %>%
            summarise(prop = n() / total) %>%
            ungroup()
        )
        
        parameters <- rbind(
          parameters,
          data.frame(
            division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
            specialty = specialty,
            time_period = labels[d],
            metric = "capacity_priority_splits",
            priority = capacity_priority_splits$covid_recovery_priority,
            value = capacity_priority_splits$prop
          )
        )
      }
      
    
      ## priorities
      total_rott = dim(dplyr::filter(
        rott,
        covid_recovery_priority %in% c(
          "Unknown",
          "Planned",
          "Priority 2",
          "Priority 3",
          "Priority 4"
        )
      ))[1]
      
      if(total_rott>0){
        (
          rott_priority_splits <- rott %>%
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
            summarise(prop = n() / total_rott) %>%
            ungroup()
        )
        
        parameters <- rbind(
          parameters,
          data.frame(
            division = dplyr::filter(specialty_names, spec_desc == specialty)$division,
            specialty = specialty,
            time_period = labels[d],
            metric = "rott_priority_splits",
            priority = rott_priority_splits$covid_recovery_priority,
            value = rott_priority_splits$prop
          )
        )
        
      }
      
    }
  }
  

  # Write to DB
  parameters$creator = "system"
  
  if (complete_update) {
    q_rm <- paste0("DELETE from elective_recovery_params where specialty  = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = parameters[-1,],
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_params"
    )
    
    q_rm <- paste0("DELETE from elective_recovery_times_to_dta where specialty  = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = times_to_dta[-1,],
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_times_to_dta"
    )
    
    q_rm <- paste0("DELETE from elective_recovery_times_to_planned where specialty  = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = times_to_planned[-1,],
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_times_to_planned "
    )
  } else{
    q_rm <-
      paste0("DELETE from elective_recovery_params where time_period = 'current' AND specialty  = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = parameters,
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_params"
    )
    
    q_rm <-
      paste0("DELETE from elective_recovery_times_to_dta where time_period = 'current' AND specialty   = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = times_to_dta,
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_times_to_dta"
    )
    
    q_rm <-
      paste0("DELETE from elective_recovery_times_to_planned where time_period = 'current' AND specialty   = '", specialty,"'")
    hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")
    hsqlTable(
      value = times_to_planned,
      server = "wwldevsqlfrm1",
      database = "nhs_datascience",
      tablename = "elective_recovery_times_to_planned"
    )

  }
  
  print(paste0("Parameters updated for: ", specialty))
}


# Clear parameters which are all NA;s
q_rm <-
  paste0("DELETE from elective_recovery_params where specialty IS NULL")
hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")

q_rm <-
  paste0("DELETE from elective_recovery_times_to_dta where specialty IS NULL")
hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")

q_rm <-
  paste0("DELETE from elective_recovery_times_to_planned where specialty IS NULL")
hsql(q = q_rm, db = "nhs_datascience", server = "wwldevsqlfrm1")

# Restore output to console
sink() 
sink(type="message")

# Close connection
close(log_con)
print("hi")

