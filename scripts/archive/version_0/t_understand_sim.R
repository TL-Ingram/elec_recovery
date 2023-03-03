# forming the required dataframe
simfn <- function(SEED) 
    set.seed(SEED)
  res <- data.frame(
      id = 1:dim(wl)[1],
      waits = as.numeric(wl$total_days_wait),
      inpatient_waits = as.numeric(wl$days_wait),
      priority = wl$priority,
      priority_definition = wl$priority_definition,
      planned  = as.numeric(wl$planned_admission_date - init_date)
)

#initial conditions
initial.waiting.list <- initial.waiting.list
demand <- demand
capacity <- capacity

# Arrival and Service Rates
rates <- data.frame(arr = demand, srv = capacity, rott = rott)

ID <- initial.waiting.list + 1

# Convert waits to time array
# this is converting waits and inpatient_waits to columns and then selecting and ordering columns
res$time_arr = 0 - res$waits
res$time_arr_inp = 0 - res$inpatient_waits
res$eligible = NA
res = res[c(
  "id",
  "time_arr",
  "time_arr_inp",
  "planned",
  "priority", # Order by this priority
  "priority_definition",# Define by this priority (can capture planned patients)
  "eligible"
)]
#####
# Output Data Frame
outp <- data.frame(
  day = 0,
  rtt_nwl52_P2 = length(which(
    -res$time_arr <= 364 & res$priority_definition == "Priority 2"
  )),
  rtt_nwm52_P2 = length(which(
    -res$time_arr > 364 & res$priority_definition == "Priority 2"
  )),
  rtt_nwl52_P3 = length(which(
    -res$time_arr <= 364 & res$priority_definition ==  "Priority 3"
  )),
  rtt_nwm52_P3 = length(which(
    -res$time_arr > 364 & res$priority_definition ==  "Priority 3"
  )),
  rtt_nwl52_P4 = length(which(
    -res$time_arr <= 364 & res$priority_definition ==  "Priority 4"
  )),
  rtt_nwm52_P4 = length(which(
    -res$time_arr > 364 & res$priority_definition ==  "Priority 4"
  )),
  rtt_nwl52_Unknown = length(which(
    -res$time_arr <= 364 & res$priority_definition ==  "Unknown"
  )),
  rtt_nwm52_Unknown = length(which(
    -res$time_arr > 364 & res$priority_definition ==  "Unknown"
  )),
  rtt_nwm52_Deferred = length(which(
    -res$time_arr > 364 & res$priority_definition ==  "Deferred"
  )),
  rtt_nwm78_P2 = length(which(
    -res$time_arr > 546 & res$priority_definition == "Priority 2"
  )),
  rtt_nwm78_P3 = length(which(
    -res$time_arr > 546 & res$priority_definition == "Priority 3"
  )),
  rtt_nwm78_P4 = length(which(
    -res$time_arr > 546 & res$priority_definition == "Priority 4"
  )),
  rtt_nwm78_Unknown = length(which(
    -res$time_arr > 546 & res$priority_definition == "Unknown"
  )),
  rtt_nwm78_Deferred = length(which(
    -res$time_arr > 546 & res$priority_definition == "Deferred"
  )),
  rtt_nwm4_P2 = length(which(
    -res$time_arr > 28 & res$priority_definition == "Priority 2"
  )),
  rtt_nwm13_P3 = length(which(
    -res$time_arr > 91 & res$priority_definition == "Priority 3"
  )), 
  rtt_nwm104_P2 = length(which(
    -res$time_arr > 728 & res$priority_definition == "Priority 2"
  )),      
  rtt_nwm104_P3 = length(which(
    -res$time_arr > 728 & res$priority_definition == "Priority 3"
  )),
  rtt_nwm104_P4 = length(which(
    -res$time_arr > 728 & res$priority_definition == "Priority 4"
  )),
  rtt_nwm104_Unknown = length(which(
    -res$time_arr > 728 & res$priority_definition == "Unknown"
  )),
  rtt_nwm104_Deferred = length(which(
    -res$time_arr > 728 & res$priority_definition ==  "Deferred"
  )),
  wl_Deferred = length(which(res$priority_definition == "Deferred")),
  wl_Planned = length(which(res$priority_definition == "Planned")),
  act_demand = NA,
  act_activity = NA,
  act_rott = NA, 
  rtt_maxwt = NA,
  rtt_meanwt = NA,
  rtt_medianwt = NA
)

#####
#step through and simulate each day in considered period
for (d in (1:nrow(rates))) {
  ################
  ### ARRIVALS ###
  ################
  
  #number of arrivals
  rate.arr <- rates$arr[1]
  #add noise
  d_arr <- rpois(1, lambda = rate.arr)
  # add new clock starts onto the waiting list
  if (d_arr > 0 & round(sum(demand_priority_splits$value)) == 1.0) {
    res <- rbind(
      res)
  data.frame(
  id = ID:(ID + d_arr - 1),
  #Simulated time already waiting before converting to inpatient wl
  time_arr = NA,
  time_arr_inp = 1, # patient arrives at time d
  # Simulated priority
  priority_definition = as.character(
    sample(
      demand_priority_splits$priority,
      d_arr,
      replace = TRUE,
      demand_priority_splits$value
    )
  ),
  priority = NA,
  #Planned date
  planned = NA,
  eligible = NA,
  stringsAsFactors = FALSE
)
}}
# the above code does react to the length of time being projected. So it is actually setting up the simulation from here.
rm(res)
print("hi")
