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
                              axis.text.x = element_text(angle = 0, vjust = 1, 
                                                         hjust=0.5),
                              axis.title.y = element_text(face = "bold")) 
)

plot_defaults_two <- list(theme_ipsum(axis_text_size = 12,
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
# ------------------------------------------------------------------------------