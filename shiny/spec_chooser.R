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
    fluidRow(
        column(2,
                uiOutput('selectUI'),
                uiOutput("selectUI2"), # this is to have the list of places
            ), 
        column(9,
                plotOutput("plot"))
            )
        )

wl_keys$wl <- as.factor(wl_keys$wl)
wl_keys$spec_desc <- as.factor(wl_keys$spec_desc)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactive({
        return(wl_keys)
    })
    wl_choices <- reactive({
        choices <- levels(data()$wl)
        return(choices)
    })
    spec_choices <- reactive({
        choices <- levels(data()$spec_desc)
        return(choices)
    })
    output$selectUI <- renderUI({
        selectInput("wl", "wl variable", choices = wl_choices()) 
    })
    output$selectUI2 <- renderUI({
        selectInput("spec_desc", "spec variable", choices = spec_choices())
    })
    data_filtered <- reactive({
        req(input$wl)
        df <-  data() %>%
            filter(spec_desc %in% input$spec_desc,
                   wl %in% input$wl)
        return(df)
    })
    
    output$plot <- renderPlot({
        ggplot(data = data_filtered()) +
            geom_line(aes(x = date, y = patients))
    }, res = 75)
}

# Run the application 
shinyApp(ui = ui, server = server)