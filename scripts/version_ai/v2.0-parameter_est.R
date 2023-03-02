source(here("scripts/archive/db_connect_functions.R"))
source(here("scripts/archive/helper_functions.R"))

specialities <- speciality
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

for (specialty in speciality){
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
where snapshot_date_dt > CAST('2023-01-01' AS DATE)
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
      mutate(covid_recovery_priority = ifelse(covid_recovery_priority == "Priority 1", "Priority 2", covid_recovery_priority)) %>%
      mutate(covid_recovery_priority = ifelse(grepl('Pri*', covid_recovery_priority), "Inpatient_wl", as.character(covid_recovery_priority)))
       
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
      group_by(covid_recovery_priority, decision_to_admit_date_dt) %>%
      summarise(dtas = n()) %>%
      ungroup() %>%
      rename(dt = decision_to_admit_date_dt) %>%
      right_join(days) %>%
      drop_na(covid_recovery_priority) %>%
      mutate(dtas = ifelse(is.na(dtas), 0, dtas)) %>%
      filter(dt >= (max(dt)-30))
    
    for (wl in wl_type) {
      if (!wl %in% dta_daily$covid_recovery_priority) {
        dta_daily <- rbind(
          dta_daily,
          data.frame(covid_recovery_priority = wl,
                     dt = Sys.Date() -3,
                     dtas = 0)
        )
      }
    }

    dta_mean <- dta_daily %>%
      group_by(covid_recovery_priority) %>%
      summarise(month_mean = mean(dtas))

    ggplot(data = dta_daily, aes(x = dt, y = dtas, colour = covid_recovery_priority)) +
      geom_smooth(se = F)
    

# table I need    
    parameters <- rbind(
      parameters,
      data.frame(
        specialty = specialty,
        time_period = labels[d],
        metric = "demand",
        list = dta_mean$covid_recovery_priority,
        month_mean = dta_mean$month_mean
      )
    )
# later will add long waiters to these parameters

    
    #########################
    ######### adms ##########
    #########################
    
    
    q_adms <- paste0(
      "WITH priorities AS (
SELECT DISTINCT
internal_number,
MIN(i.priority_local_code) AS covid_recovery_priority
FROM [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] i
where snapshot_date_dt > CAST('2023-01-01' AS DATE)
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
      dplyr::filter(flag == 1) %>%
      mutate(covid_recovery_priority = ifelse(grepl('Pri*', covid_recovery_priority), "Inpatient_wl", as.character(covid_recovery_priority)))
    
    
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
      mutate(covid_recovery_priority = ifelse(covid_recovery_priority == "Priority 1", "Priority 2", covid_recovery_priority)) %>%
      mutate(covid_recovery_priority = ifelse(grepl('Pri*', covid_recovery_priority), "Inpatient_wl", as.character(covid_recovery_priority)))
    
    #Admissions
    adms_daily <- adms  %>%
      group_by(covid_recovery_priority, admission_date) %>%
      summarise(adms = n()) %>%
      ungroup() %>%
      rename(dt = admission_date) %>%
      right_join(days) %>%
      drop_na(covid_recovery_priority) %>%
      mutate(adms = ifelse(is.na(adms), 0, adms)) %>%
      filter(dt >= (max(dt)-30))
    
    for (wl in wl_type) {
      if (!wl %in% adms_daily$covid_recovery_priority) {
        adms_daily <- rbind(
          adms_daily,
          data.frame(covid_recovery_priority = wl,
                     dt = Sys.Date() -3,
                     adms = 0)
        )
      }
    }
      
    ggplot(data = adms_daily, aes(x = dt, y = adms, colour = covid_recovery_priority)) +
      geom_smooth(se = F)
    
    adms_mean <- adms_daily %>%
      group_by(covid_recovery_priority) %>%
      summarise(month_mean = mean(adms))
    
    parameters <- rbind(
      parameters,
      data.frame(
        specialty = specialty,
        time_period = labels[d],
        metric = "capacity",
        list = adms_mean$covid_recovery_priority,
        month_mean = adms_mean$month_mean
      )
    )
    
    #Removals for reasons other than treatment
    rott_daily <- rott  %>%
      group_by(covid_recovery_priority, removed_date) %>%
      summarise(rott = n()) %>%
      ungroup() %>%
      rename(dt = removed_date) %>%
      right_join(days) %>%
      drop_na(covid_recovery_priority) %>%
      mutate(rott = ifelse(is.na(rott), 0, rott)) %>%
      filter(dt >= (max(dt)-30))
    
    for (wl in wl_type) {
      if (!wl %in% rott_daily$covid_recovery_priority) {
        rott_daily <- rbind(
          rott_daily,
          data.frame(covid_recovery_priority = wl,
                     dt = Sys.Date() -3,
                     rott = 0)
        )
      }
    }
    
    ggplot(data = rott_daily, aes(x = dt, y = rott, colour = covid_recovery_priority)) +
      geom_smooth(se = F)
    
    rott_mean <- rott_daily %>%
      group_by(covid_recovery_priority) %>%
      summarise(month_mean = mean(rott))
    
    parameters <- rbind(
      parameters,
      data.frame(
        specialty = specialty,
        time_period = labels[d],
        metric = "rott",
        list = rott_mean$covid_recovery_priority,
        month_mean = rott_mean$month_mean
      )
    )
  }
}
parameters <- parameters %>%
  drop_na() %>%
  write_rds(here("rds", "parameters", "all_spec.rds"))

