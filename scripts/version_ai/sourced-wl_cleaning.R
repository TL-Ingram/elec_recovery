#####
# Filter data appropriate for the models
data_mutate <- data %>%
  mutate(., date = dmy(date),
         covid_recovery_priority = if_else(
           grepl("[[:digit:]]", covid_recovery_priority), 
           "Inpatient_wl", covid_recovery_priority))

# Overall WL (P2-P4, planned, deferred, unknown)
o_wl <- data_mutate %>%
  group_split(covid_recovery_priority) %>%
  map(. %>%
  group_by(date, spec_desc, covid_recovery_priority) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  rename("wl" = covid_recovery_priority))

# 52 & 65 week waiter WL. Only Inpatient_wl
lw_wl <- data_mutate %>%
  filter(., (wm52 == 1 | wm65 == 1) &
         covid_recovery_priority == "Inpatient_wl") %>%
  mutate(., "temp" = if_else(wm52 == 1 & wm65 == 1, ">65", ">52")) %>%
  group_by(date, spec_desc, temp) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  rename("wl" = temp)

# Bind data.frames of all groups together
wl_comp <- bind_rows(o_wl, lw_wl)

# clean up global env
rm(data, data_mutate, o_wl, lw_wl)


# ------------------------------------------------------------------------------
#####
# ggplot defaults
plot_defaults <- list(theme_ipsum_pub(axis_text_size = 8,
                                    axis_title_size = 8,
                                    subtitle_size = 8,
                                    plot_title_size = 12) +
                  theme(panel.grid.major = element_line(colour = "grey50"),
                        panel.grid.minor = element_line(colour = "grey50"),
                        plot.caption = element_text(size = 6),
                        legend.position = "none",
                        axis.text.x = element_text(angle = 0, vjust = 1, 
                                                   hjust=0.5),
                        axis.title.y = element_text(face = "bold")) 
)
       
plot_defaults_two <- list(theme_ipsum_pub(axis_text_size = 12,
                                      axis_title_size = 14,
                                      subtitle_size = 14,
                                      plot_title_size = 18) +
                        theme(panel.grid.major = element_line(colour = "grey50"),
                              panel.grid.minor = element_line(colour = "grey50"),
                              plot.caption = element_text(size = 10),
                              legend.text = element_text(size = 10),
                              axis.text.x = element_text(angle = 0, vjust = 1, 
                                                         hjust=0.5),
                              axis.title.y = element_text(face = "bold")) 
)

# Plot labels
{
# wl_label <- wl_type
# names(wl_label) <- c("lw_wl", "o_wl")
}


# ------------------------------------------------------------------------------