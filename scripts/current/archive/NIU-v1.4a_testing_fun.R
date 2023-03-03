#####
# Load packages
shelf(tidyverse, here, lubridate, forecast, fpp3, hrbrthemes)


# ------------------------------------------------------------------------------
#####
# Load data
data <- read_csv(here("data", "hist_wl.csv"))

# Pull speciality names into vector for looping through
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)

# Source scripts
source(here("scripts", "version_ai", "sourced-wl_cleaning.R"))

# Pull type of waiting list into vector for looping through
wl_type <- wl_comp %>%
  distinct(wl) %>%
  pull(wl)


# ------------------------------------------------------------------------------
#####
# Time period models trained on
train_init = date("2022-08-01")
train_halt = date("2022-12-14") # eventually change this to sys.date - 1
train_period_label = "Training period"
train_period_days = as.numeric(train_halt - train_init)/2
train_period_date = train_init + train_period_days
yesterday = train_halt - 1
h = 365

# ------------------------------------------------------------------------------
#####

spec_forecast <- function(wl_type, speciality){
  # Filter to WL type
  for(i in wl_type) {
    wl_prepared <- wl_comp %>%
      filter(., wl == i)
    # Filter to speciality
    for (j in speciality) {
      wl_prep <- wl_prepared %>%
        filter(., spec_desc == j)
      # Filter to init date, filling date gaps and imputing missing wl size
      wl_ready <- wl_prep %>%
        filter(date > train_init) %>%
        as_tsibble(.) %>%
        fill_gaps(., date) %>%
        fill(., patients, .direction = "up") %>%
        ungroup(.)
      
      
      
      # Script continuance test
      if(dim(wl_ready)[1] != 0) {
        
        
        
        # Filter to halt date
        train_set <- wl_ready %>%
          filter(., date < train_halt) %>%
          select(., date, patients)
      
        # Build forecasting models to test
        STLF <- decomposition_model(
          STL((patients) ~ season(window = Inf)),
          ETS(season_adjust ~ season("N"))
        )
        model_frame <- train_set %>%
          fill_gaps(patients = mean(patients)) %>%
          fabletools::model(
            ets = ETS(patients, trace = T),
            stlf = STLF,
            arima = ARIMA(patients, stepwise = F, approximation = F, trace = T),
            nnar = NNETAR(patients, stepwise = F, trace = T)
          ) %>%
          mutate(combination = (ets + stlf + arima + nnar)/4)
      
        # Generate future sample paths
        sim_paths <- model_frame %>%
          generate(h = h, times = 5)

        
        
        # Script continuance test
        cont_test <- sim_paths %>%
          filter(.sim > 4)
        if(dim(cont_test)[1] != 0) {

          

          # Compute forecast distributions from future sample paths and create 
          # fable object
          sim_results <- sim_paths %>%
            as_tibble() %>%
            group_by(date, .model) %>%
            summarise(dist = distributional::dist_sample(list(.sim))) %>%
            ungroup() %>%
            as_fable(index=date, key=.model, distribution=dist, 
                     response="patients")

          # Plot results over-laid on wl and filter for combination model
          sim_results %>%
            filter(.model == "combination") %>%
            autoplot(wl_prep, level = 80, size = 0.6, alpha = 0.9) +
            geom_line(data = wl_prep, aes(x = date, y = patients), size = 0.6,
                      alpha = 0.7, colour = "grey50") +
            geom_vline(data = wl_prep, xintercept = train_halt, 
                       linetype = "dashed", colour = "grey50",
                       size = 0.5, alpha = 0.8) +
            geom_vline(data = wl_prep, xintercept = train_init,
                       linetype = "dashed", colour = "grey50",
                       size = 0.5, alpha = 0.8) +
            geom_text(data = wl_prep, aes(x = train_period_date, y = Inf, 
                                          label = train_period_label), 
                      vjust = 1.5, size = 2.5, colour = "grey40") +
            scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
            plot_defaults +
            labs(fill = "",
                 x = "",
                 y = "Patients",
                 title = j,
                 level = "",
                 subtitle = paste0("Forecast horizon begins from ",
                                   train_halt, " and extends for ", h, " days"),
                 caption = paste0("Training period is the set of data fed into the model to generate the forecast
                                  Training period is from ", train_init, 
                                  " to ", yesterday, 
                                  "\nBlue line depicts mean predicted patient number
                                  Shaded region depicts 80% prediction interval"))
          
          # Save plot
          file_name <- paste0(i, "_", j)
          ggsave(here("plots", "combi_plots", filename=paste0(
            file_name, ".png")), 
            device = "png")
          
          # Create csv of history plus forecast horizon
          sim_results_raw <- sim_paths %>%
            as_tibble(.) %>%
            filter(., .model == "combination") %>%
            select(-(.model)) %>%
            mutate("speciality" = j) %>%
            write.csv(here("csv", "history+horizon", "horizon", filename = paste0(file_name, ".csv")))
          
          # sim_results_table <- sim_results_raw %>%
          #   group_by(., date) %>%
          #   summarise("lower bound" = round(min(.sim)),
          #             "upper bound" = round(max(.sim)),
          #             mean = round(mean(.sim))) %>%
          #   ungroup(.) %>%
          #   filter(., date == train_halt | date == (train_halt + 10) | date == (train_halt + 20)) %>%
          #   mutate(., "percent change" = round(-100 + (mean/lag(mean)*100), digits = 2))
            # create another mutate if else patients <0 then 0 for c(2:4)
        }
      }
    }
  }
}
spec_forecast(wl_type, speciality)        


# ------------------------------------------------------------------------------
#####
all_forecast <-list.files(path = here("csv/history+horizon/horizon/"), pattern = "*.csv", full.names = T) %>%
  map_dfr(read_csv) #%>% 
bind_rows()
?list.files
