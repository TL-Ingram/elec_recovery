##### load packages ------------------------------------------------------------
shelf(tidyverse, here, lubridate, forecast, fpp3)

##### load data ----------------------------------------------------------------
data <- read_csv(here("data", "hist_wl.csv"))

##### pull speciality names for use in for loop below ---------------------------
speciality <- data %>%
  distinct(spec_desc) %>%
  pull(spec_desc)
# speciality_name = "Colorectal Surgery"

##### loop to output graphs for each speciality
for (speciality_name in speciality) {
wl <- data %>%
  filter(., spec_desc == speciality_name,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarise(patients = n())

wl_52 <- data %>%
  filter(., spec_desc == speciality_name,
         wm52 == 1,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, wm52) %>%
  summarise(wm52 = n()) %>%
  ungroup(.)

#####
#cbind by date
# colon <- wl %>%
#   left_join(wl_52, by = "date") %>%
#   pivot_longer(cols = c(2:3), names_to = "metric", values_to = "value")
# print(colon)
# }
#####
ts.data_52<- ts(wl_52%>%dplyr::select(wm52))
ARIMA_52<- auto.arima(ts.data_52, seasonal = F, stepwise = F, approximation = F, trace = T)
png(here("plots/forecast_arima/", paste0(speciality_name, "_wl52.png")))
plot(forecast::forecast(ARIMA_52,h=180))
dev.off()

ts.data_wl<- ts(wl%>%dplyr::select(patients))
ARIMA_wl<- auto.arima(ts.data_wl, seasonal = T, stepwise = F, approximation = F, trace = T)
png(here("plots/forecast_arima/", paste0(speciality_name, "_wl.png")))
plot(forecast::forecast(ARIMA_wl, h=365))
dev.off()
}

# output these as tables for when clearance
# think more about data to use before
# think about adding covariate columns for additions, removals and ROTT. The number of rows for this = horizon. This should be usable for reactivity too!
## create matrix with forecasted covariates for future?
# created matrix with them now. Just need to add
parameters <- parameters

#####
# ts.data<- ts(wl_52%>%dplyr::select(wm52))
# ARIMA<- auto.arima(ts.data, seasonal = T, stepwise = F, approximation = F, trace = T)
# plot(forecast::forecast(ARIMA,h=365))
# plot(forecast(auto.arima(ts(fit_wl52, frequency = 7), D=1),h=100))
# plot(forecast(auto.arima(ts(temps,frequency=365)),h=365))
# fit <- auto.arima(wl_52[,"wm52"], seasonal=FALSE)
# summary(fit)
# fit_wl52 %>%
#   forecast(h = "3 years") %>%
#   autoplot(wl_52)
# ggsave(here("plots", "forecast_arima", paste0(speciality_name, "_wl52.jpg")), width = 10, height = 6, dpi = 600)
# 
# 
# 
# Box.test(diff(ts.data), lag=10, type="Ljung-Box")
# 
# library(urca)
# ts.data %>% diff() %>% ur.kpss() %>% summary()
# nsdiffs(ts.data)

#####
# fit_wl <- wl %>%
#   as_tsibble() %>%
#   fill_gaps() %>%
#   model(ARIMA(patients))
# fit_wl %>%
#   forecast(h = "3 years") %>%
#   autoplot(wl)
# ggsave(here("plots", "forecast_arima", paste0(speciality_name, "_wl.jpg")), width = 10, height = 6, dpi = 600)
#####
# ts.data<- ts(wl_52%>%dplyr::select(mean))
# ARIMA<- auto.arima(ts.data, seasonal = F)
# forecast(ARIMA)
# 
# ts.data <- ts(wl_52 %>% dplyr::select(wm52), start = c(2022,1,2), frequency = 365)
# print(ts.data)
# summary(ts.data)
# test <- auto.arima(ts.data, trace=TRUE, d = 1) 
# test2 <- forecast(test, h = 1[1])


# fit <- auto.arima(wl[,"patients"], seasonal=FALSE)
# summary(fit)

# test2 <- predict(test,n.ahead = 365)
# futurVal <- forecast(test2, h=10)
# plot.forecast(futurVal)
# 



# test <- auto.arima(ts.data, trace = T)
# summary(test)
# testing <- predict(test, 365)
# forecast %>%
#   autoplot(wl_52)
# ??forecast
# ??forecast::ts
# plot(forecast(testing,h=20))




# colon %>%
#   ggplot(aes(x = date, y = value, colour = metric)) +
#   geom_line() +
#   theme_bw()
