#####
# Filter data appropriate for the models
# Overall WL (P2-P4)
o_wl <- data %>%
  filter(., !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, spec_desc) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  mutate(wl = "o_wl")

# 52 week waiter WL. lw = long waiter
lw_wl <- data %>%
  filter(., wm52 == 1,
         !(covid_recovery_priority == "Unknown" 
           | covid_recovery_priority == "Deferred"
           | covid_recovery_priority == "Planned")) %>%
  mutate(date = dmy(date)) %>%
  group_by(date, spec_desc, wm52) %>%
  summarise(patients = n()) %>%
  ungroup(.) %>%
  select(-(wm52)) %>%
  mutate(wl = "lw_wl")
  
wl_comp <- rbind(o_wl, lw_wl)
rm(data, lw_wl, o_wl)


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
       
