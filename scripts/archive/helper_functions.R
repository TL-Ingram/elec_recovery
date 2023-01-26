# Function to extract Month
get_month <- function(string){
  # Look up vectors
  months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  full_months <- c("JANUARY", "FEBRUARY","MARCH","APRIL","MAY","JUNE", "JULY", "AUGUST", "SEPTEMBER","OCTOBER", "NOVEMBER","DECEMBER")
  
  #convert string to upper case
  string <- str_to_upper(string)
  
  # Get any month matches
  scan <- str_extract(string, pattern = c(months, full_months))
  scan <- scan[!is.na(scan)]
  month <- unique(scan)
  
  if(length(month)>0){
    # Convert any short months to full months
    for (m in 1:length(month)){
      if(month[m] %in% full_months){
        
      }else{
        month[m] <- full_months[month[m] == months]
      }
    }
    full_month <- unique(month)
    
    # Order the months
    full_month <- factor(full_month, levels = full_months, ordered = T)
    
    # Take the maximum month
    full_month = max(full_month)
    
    # Return as character
    return(full_month)
  }else{
    return(NA)
  }
  
}

# Function to extract year
get_year <- function(string){
  # Look up vectors
  years <-c(" 19", " 20", " 21", " 22", " 23", " 24", " 25", " 26", " 27")
  full_years  <- c("2019","2020","2021", "2022", "2023", "2024", "2025", "2026", "2027")
  
  #convert string to upper case
  string <- str_to_upper(string)
  
  # Get any month matches
  scan <- str_extract(string, pattern = c(years, full_years))
  scan <- scan[!is.na(scan)]
  year <- unique(scan)
  
  if(length(year)>0){
    # Convert any short months to full months
    for (y in 1:length(year)){
      if(year[y] %in% full_years){
        
      }else{
        year[y] <- full_years[year[y] == years]
      }
    }
    full_year <- unique(year)
    
    # Order the months
    full_year <- factor(full_year, levels = full_years, ordered = T)
    
    # Take the maximum month
    full_year = max(full_year)
    
    # Return as character
    return(as.character(full_year))
  }else{
    return(NA)
  }
  
}

# Function to get current waitinglist
get_current_wl <- function(init_date, specialty_name){
  q_wl <- paste0(
    "With max_snapshot AS (
  select CAST('",
    as.character(init_date),
    "' AS DATE) as max_date
)
SELECT
  wl.internal_number
, wl.PathwayNumber as pathway_number
, RTT_Start_Date
, clock_start_date as clock_start_date
, clock_stop_date
, wl.[decision_to_admit_date_dt] as dta_date
, wl.Admis_Method_Desc
, wl.WLApproxAdmissionDate as WLApproxAdmissionDate
, tci_date_dt
, Admission_Reason
, DATEDIFF(DAY, clock_start_date, CAST( '",
  init_date,
  "' AS date)) as total_days_wait
, DATEDIFF(DAY, [decision_to_admit_date_dt], CAST( '",
  init_date,
  "' AS date)) as days_wait
, DATEDIFF(DAY, clock_start_date, [decision_to_admit_date_dt]) as days_to_dta
,  CASE
    WHEN wl.Admis_Method_Desc IN ('ELECTIVE PLANNED') THEN 'Planned'
    When wl.priority_local_code IS NULL THEN 'Unknown'
    ELSE wl.priority_local_code END AS covid_recovery_priority
, CASE
    WHEN wl.priority_local_code IS NULL THEN 'Unknown'
    ELSE wl.priority_local_code END AS priority_local_code
FROM max_snapshot
LEFT JOIN [nhs_reporting].[dbo].[reporting_Inpatient_Waiting_List] wl on wl.snapshot_date_dt = max_snapshot.max_date
LEFT JOIN (SELECT * FROM [nhs_reporting].[dbo].[reporting_rtt_pathway_summary] WHERE run_type = 'today' AND pathway_status <> 'not applicable') rtt ON wl.PathwayNumber = rtt.pathway_number
WHERE wl.Admis_Method_Desc IN ('ELECTIVE PLANNED', 'ELECTIVE WAITING LIST', 'ELECTIVE BOOKED')
AND spec_desc = '"
  ,
  specialty_name
  ,
  "';
"
  )

wl <- hsql(q = q_wl, db = "nhs_reporting", server = "WWLDWSQL1")
return(wl)
}

# Function to Reformat the priority of the patients on the waiting list
format_priority <- function(wl){
  # Set the priority definition (Planned or their P-Code)
  wl$priority_definition = ifelse(
    wl$covid_recovery_priority == 'Planned',
    'Planned',
    ifelse(
      wl$covid_recovery_priority %in% c(8, 9),
      'Deferred',
      ifelse(
        wl$covid_recovery_priority %in% c('1', '2'),
        'Priority 2',
        ifelse(
          wl$covid_recovery_priority %in% c('Unknown', '7'),
          'Unknown',
          paste0("Priority ", wl$covid_recovery_priority)
        )
      )
    )
  )
  
  # Set their priority (P-Code for all patients including planned)
  wl$priority = ifelse(
    wl$priority_local_code %in% c(8, 9),
    'Deferred',
    ifelse(
      wl$priority_local_code %in% c('1', '2'),
      'Priority 2',
      ifelse(
        wl$priority_local_code %in% c('Unknown', '7'),
        'Unknown',
        paste0("Priority ", wl$priority_local_code)
      )
    )
  )
  return(wl)
}

# Function to Infer the planned admission date for all planned patients
get_planned_adm_date <- function(wl){
  wl$planned_month = apply(wl[,"Admission_Reason", drop = F], FUN = get_month, MARGIN = 1)
  wl$planned_year = apply(wl[,"Admission_Reason", drop = F], FUN = get_year, MARGIN = 1)
  wl$inferred_planned_admission_date = make_date(year = as.numeric(wl$planned_year), month = as.numeric(wl$planned_month), day = 28)
  
  wl <- wl %>%
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
  
  # If a planned patient has no planned date, set it to be 2 years after the dta:
  wl$planned_admission_date = as.Date(
    ifelse(
      wl$covid_recovery_priority == 'Planned' &
        is.na(wl$planned_admission_date),
      as.Date(wl$dta_date + 2 * 365 * 24 * 60 * 60),
      as.Date(wl$planned_admission_date)
    ),
    origin = "1970-01-01"
  )
  return(wl)
}

# Function to determine the waiting time for all patients on the waiting list
get_wait_time <- function(wl){
  # If a patient has a missing clock start, or they have a non-missing clock stop, then set their total days wait to their days wait (day on the inpatient WL only)
  wl$total_days_wait = ifelse(
    wl$covid_recovery_priority != 'Planned' & (is.na(wl$clock_start_date) | !is.na(wl$clock_stop_date)),
    wl$days_wait, 
    wl$total_days_wait)
  
  # If a patient is planned, set their total_days_wait to days_wait
  wl$total_days_wait = ifelse(
    wl$covid_recovery_priority == 'Planned',
    wl$days_wait,
    wl$total_days_wait
  )
  return(wl)
}

# Function to calculate clearance times
get_clear <- function(data, metric_name = "rtt_nwm52_P2") {
  data_filtered <- filter(data, metric == metric_name)
  clear_dates <- rep(NA, 3)
  names <- c("q025", "mean", "q975")
  results <-
    data.frame(type = metric_name,
               quantile = NA,
               date = NA)
  for (d in 1:3) {
    #get first non-zero from the back
    data_filtered_type <-
      data_filtered[, names(data_filtered) %in% c("date", names[d])]
    # Reverse the data and get the location of the first non zero entry
    point <- which(rev(data_filtered_type[, 1][[1]]) > 1)[1]
    
    if (is.na(point)) {
      date = data_filtered_type[, 2][[1]][1]
    } else if (point == 1) {
      date = NA
    } else{
      date <- rev(data_filtered_type[, 2][[1]])[point]
    }
    results <-
      results %>% rbind(data.frame(
        type = metric_name,
        quantile = names[d],
        date = date
      ))
  }
  return(results[-1, ])
}

