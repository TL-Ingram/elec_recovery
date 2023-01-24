#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Elective recovery: forecast"),
    tabPanel(
        title = "Reactive plot",
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                uiOutput('selectUI'), # this is to have the list of places
            ), 
            mainPanel = mainPanel(
                plotOutput("plot")
            )
        )
    )
)

knitted$wl <- as.factor(knitted$wl)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactive({
        return(knitted)
    })
    wl_choices <- reactive({
        choices <- levels(data()$wl)
        return(choices)
    })
    output$selectUI <- renderUI({
        selectInput("wl", "wl variable", choices = wl_choices())  
    })
    data_filtered <- reactive({
        req(input$wl)
        df <-  data() %>%
            filter(wl %in% input$wl)
        return(df)
    })
    
    output$plot <- renderPlot({
            ggplot(data = data_filtered()) +
            geom_line(aes(x = date, y = patients, colour = wl), alpha = 0.8, size = 0.8) +
            geom_line(aes(x = date, y = p_mean, colour = wl), alpha = 0.8, size = 0.8) +
            geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                        fill="slategray3", alpha=.3) +
            geom_ribbon(aes(date, ymax = p_upper, ymin = p_lower), 
                        fill="slategray3", alpha=.3) +
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
    }, res = 75)
}

# Run the application 
shinyApp(ui = ui, server = server)


