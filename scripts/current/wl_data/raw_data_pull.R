# get raw the same as data

raw <- read_csv(here("data", "raw.csv"))
data <- read_csv(here("data", "hist_wl-2.csv"), show_col_types = F)
  
ps_raw <- raw %>%
  filter(spec_desc == "Plastic Surgery") %>%
  filter(dmy_hms(decision_to_admit_date_dt) > date(date_from),
         Admis_Method_Desc %in% c("ELECTIVE PLANNED", "ELECTIVE WAITING LIST", "ELECTIVE BOOKED"),
         waiting_list_type == "SNAP") %>%
  mutate(across(.cols = c(decision_to_admit_date_dt, admission_date_dt, 
                          removed_date_dt, tci_date_dt), .fns = dmy_hms)) #%>%
  select(internal_number, decision_to_admit_date_dt, tci_date_dt, removed_date_dt, 
         admission_date_dt, priority_local_code) %>%
  mutate(admission_date = if_else(is.na(admission_date_dt) & 
                                    (tci_date_dt == removed_date_dt), 
                                  tci_date_dt, admission_date_dt),
         rott = if_else(is.na(admission_date), 1, 0))
  group_by(internal_number) %>%
  summarise(priority_local_code = min(priority_local_code))
  

  

# amalgamate both raw dta and adm into one script. Only use QVD.
# then work out how the long wait numbers were generated, then use this to work out parameters

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
