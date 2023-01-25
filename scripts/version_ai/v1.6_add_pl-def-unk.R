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
  filter(., wl != c("Deferred", "Unknown")) %>%
  pull(wl)


# ------------------------------------------------------------------------------
#####
# Time period models trained on
train_init = date("2022-10-17")
train_halt = date("2023-01-17") # eventually change this to sys.date - 1
train_period_label = "Training period"
train_period_days = as.numeric(train_halt - train_init)/2
train_period_date = train_init + train_period_days
yesterday = train_halt - 1
h = 365
# h = as.numeric(date("2025-12-31") - train_halt)
# wl_type = c("Planned", "Inpatient_wl")
# i = c("Planned", "Inpatient_wl")
# j = "Haematology"
# speciality = c("General Medicine", "Haematology", "Gastroenterology", "Urology")


# ------------------------------------------------------------------------------
#####
# Produce forecast outputs, csv + plot, for each speciality
# spec_forecast <- function(wl_type, speciality){
#   # Filter to WL type
spec_forecast <- function(wl_type, speciality) {
  list_paths <- list()
  list_wl <- list()
  for(i in wl_type) {
    wl_prepared <- wl_comp %>%
         filter(., wl == i)
    # Filter to speciality
       for (j in speciality) {
         wl_prep <- wl_prepared %>%
           filter(., spec_desc == j)
         
         # Write historic paths to list
         list_wl[[paste0(i, "_", j)]] <- wl_prep
         wl_keys <- bind_rows(list_wl, .id = "spec_desc")

         
      # Filter to init date, filling date gaps and imputing missing wl size
         writeLines(paste0("Waiting list: ", i,
                           "\nSpeciality: ", j))
         
         wl_ready <- wl_prep %>%
           filter(date > train_init) %>%
           as_tsibble(., index = "date") %>%
           fill_gaps(., date) %>%
           fill(., patients, .direction = "up") %>%
           ungroup(.)
      
      
      
         # Script continuance test
         if(dim(wl_ready)[1] >= 10) {
           writeLines(paste0("Building models and running simulations..."))
        
        
        
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
               ets = ETS(patients, trace = F),
               stlf = STLF,
               arima = ARIMA(patients, stepwise = F, approximation = F, trace = F),
               nnar = NNETAR(patients, stepwise = F, trace = F)
               ) %>%
             mutate(combination = (ets + stlf + arima + nnar)/4)
      
           # Generate future sample paths
           sim_paths <- model_frame %>%
             generate(h = h, times = 15)

        
           # Script continuance test
           cont_test <- sim_paths %>%
             filter(.model == "combination") %>%
             mutate(.sim = if_else(.sim < 1, 0, .sim),
                    wl = i) %>%
             select(-(.model)) %>%
             as_tibble(.)
           
           
           # if(dim(cont_test)[1] != 0) {
           
               # Write forecast paths to list
             list_paths[[paste0(i, "_", j)]] <- cont_test
             path_keys <- bind_rows(list_paths, .id = "spec_desc")
             writeLines(c(""))
         }
       }
  }
  # Write rds for speedy testing
  write_rds(wl_keys, here("rds", "keys", "wl_keys.rds"))
  write_rds(path_keys, here("rds", "keys", "path_keys.rds"))
  # Read rds for speedy testing
  wl_keys <- read_rds(here("rds", "keys", "wl_keys.rds"))
  path_keys <- read_rds(here("rds", "keys", "path_keys.rds"))
  
  # Create long waiters "clearance date" table
  source(here("scripts", "version_ai", "sourced-clearance_table.R"))
  write.csv(lw_65, here("csv", "clear_date", "v1.4", 
                               "65wk_clear_dates.csv"), row.names = F)
  write.csv(lw_52, here("csv", "clear_date", "v1.4", 
                        "52wk_clear_dates.csv"), row.names = F)
  print(lw_table)
  
  
  # Create graph of overall waiting list position
  # source("")
  
    hist_wl <- wl_keys %>%
      group_by(wl, date) %>%
      summarise(patients = sum(patients)) %>%
      mutate(filter = "historic")
    
    # Tidy all_forecast to same format as hist_wl, then knit together
    af_test <- path_keys %>%
      group_by(., wl, spec_desc, date) %>%
      summarise(max = quantile(.sim, 0.8),
                mean = mean(.sim),
                min = quantile(.sim, 0.2)) %>%
      ungroup(.) %>%
      select(-(spec_desc)) %>%
      group_by(wl, date) %>%
      summarise(p_mean = sum(`mean`),
                p_upper = sum(`max`),
                p_lower = sum(`min`)) %>%
      filter(date >= train_halt) %>%
      mutate(filter = "forecast")
    
    # Row bind historical and forecast
    knitted <- rbind(hist_wl, af_test) %>%
      mutate(wl = str_replace(wl, ">52", ">52 weeks"),
             wl = str_replace(wl, ">65", ">65 weeks"))
    
    # Plot inpatient and planned waiting lists
    plot_o <- knitted %>%
      ggplot() +
      geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.8, 
                data = knitted %>% 
                  filter(wl %in% c("Planned", 
                                   "Inpatient_wl"))) + 
      geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.8, 
                data = knitted %>% 
                  filter(wl %in% c("Planned", 
                                   "Inpatient_wl"))) +
      geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                  fill="slategray3", alpha=.3,
                  data = knitted %>% filter(wl %in% "Planned")) +
      geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                  fill="slategray3", alpha=.3,
                  data = knitted %>% filter(wl %in% "Inpatient_wl")) +
      geom_vline(data = wl_prep, xintercept = train_halt,
                   linetype = "dashed", colour = "grey50",
                   size = 0.8, alpha = 0.8) +
      geom_vline(data = wl_prep, xintercept = train_init,
                   linetype = "dashed", colour = "grey50",
                   size = 0.8, alpha = 0.8) +
      geom_text(data = wl_prep, aes(x = train_period_date, y = Inf,
                                      label = train_period_label),
                  vjust = 1.5, size = 4.5, colour = "grey40") +
      scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
      plot_defaults_two +
      scale_colour_manual(values = c("royalblue3", "mediumpurple3")) +
      labs(fill = "",
           x = "",
           y = "Patients",
           title = "WWL inpatient waiting list - overall position",
           level = "",
           colour = "",
           subtitle = paste0("Forecast horizon begins from ",
                             train_halt, " and extends for ", h, " days"),
           caption = paste0("Training period is the set of data fed into the model to generate the forecast
                                  Training period is from ", train_init,
                            " to ", yesterday,
                            "\nHorizon lines depict mean predicted patient number
                                  Shaded regions depict 80% prediction interval"))
    
    # Save plot
    ggsave(here("plots", "all_spec_wl", "overall_wl.jpg"), 
           device = "jpg", width = 14, height = 10)
    
    # Print plot
    print(plot_o)
    
    # Plot long waiters
    plot_lw <- knitted %>%
      ggplot() +
      geom_line(aes(date, patients, colour = wl), alpha = 0.8, size = 0.8, 
                data = knitted %>% 
                  filter(wl %in% c(">52 weeks", 
                                   ">65 weeks"))) + 
      geom_line(aes(date, p_mean, colour = wl), alpha = 0.8, size = 0.8, 
                data = knitted %>% 
                  filter(wl %in% c(">52 weeks", 
                                   ">65 weeks"))) +
      geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                  fill="slategray3", alpha=.3,
                  data = knitted %>% filter(wl %in% ">52 weeks")) +
      geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                  fill="slategray3", alpha=.3,
                  data = knitted %>% filter(wl %in% ">65 weeks")) +
      geom_vline(data = wl_prep, xintercept = train_halt,
                 linetype = "dashed", colour = "grey50",
                 size = 0.8, alpha = 0.8) +
      geom_vline(data = wl_prep, xintercept = train_init,
                 linetype = "dashed", colour = "grey50",
                 size = 0.8, alpha = 0.8) +
      geom_text(data = wl_prep, aes(x = train_period_date, y = Inf,
                                    label = train_period_label),
                vjust = 1.5, size = 4.5, colour = "grey40") +
      scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
      plot_defaults_two +
      scale_colour_manual(values = c("lightsteelblue4", "indianred4")) +
      labs(fill = "",
           x = "",
           y = "Patients",
           title = "WWL inpatient waiting list - long waiters",
           level = "",
           colour = "",
           subtitle = paste0("Forecast horizon begins from ",
                             train_halt, " and extends for ", h, " days"),
           caption = paste0("Training period is the set of data fed into the model to generate the forecast
                                  Training period is from ", train_init,
                            " to ", yesterday,
                            "\nHorizon lines depict mean predicted patient number
                                  Shaded regions depict 80% prediction interval"))
    
    # Save plot
    ggsave(here("plots", "all_spec_wl", "longwaiter_wl.jpg"), 
           device = "jpg", width = 14, height = 10)
    
    # Print plot
    print(plot_lw)
    
    
}


# --------------
spec_forecast(wl_type, speciality)
# --------------


# ------------------------------------------------------------------------------
#####
# Compute forecast distributions from future sample paths and create 
             # fable object
           
             sim_results <- path_keys %>%
               as_tibble(., index = "date") %>%
               mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
               group_by(date, spec_desc) %>%
               summarise(dist = distributional::dist_sample(list(.sim)), .groups = "drop_last") %>%
               ungroup(.) %>%
               as_fable(index=date, key=spec_desc, distribution=dist, 
                        response="patients")

#####       
             # test <- wl_keys %>% 
             #   select(-(wl)) %>%
             #   as_tibble(., key = "spec_desc", index = "date") #%>%
               # as_fable(index="date", key="spec_desc", dist  = wl, response = "patients")
             
             #Plot results over-laid on wl and filter for combination model

          sim_results %>%
            autoplot()#, level = 80, size = 0.6, alpha = 0.9) #+
          #   geom_line(data = wl_prep, aes(x = date, y = patients), size = 0.6,
          #             alpha = 0.7, colour = "grey50") +
          #   geom_vline(data = wl_prep, xintercept = train_halt,
          #              linetype = "dashed", colour = "grey50",
          #              size = 0.5, alpha = 0.8) +
          #   geom_vline(data = wl_prep, xintercept = train_init,
          #              linetype = "dashed", colour = "grey50",
          #              size = 0.5, alpha = 0.8) +
          #   geom_text(data = wl_prep, aes(x = train_period_date, y = Inf,
          #                                 label = train_period_label),
          #             vjust = 1.5, size = 2.5, colour = "grey40") +
          #   scale_x_date(breaks = "3 months", date_labels = "%b-%Y") +
          #   plot_defaults +
          #   labs(fill = "",
          #        x = "",
          #        y = "Patients",
          #        title = j,
          #        level = "",
          #        subtitle = paste0("Forecast horizon begins from ",
          #                          train_halt, " and extends for ", h, " days"),
          #        caption = paste0("Training period is the set of data fed into the model to generate the forecast
          #                         Training period is from ", train_init,
          #                         " to ", yesterday,
          #                         "\nBlue line depicts mean predicted patient number
          #                         Shaded region depicts 80% prediction interval"))

          # # Save plot
          # file_name <- paste0(i, "_", j)
          # ggsave(here("plots", "combi_plots", filename=paste0(
          #   file_name, ".png")),
          #   device = "png")
          
          # Create csv of history plus forecast horizon
