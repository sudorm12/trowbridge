library(shiny)

ui <- fluidPage(
  titlePanel("Cambridge, MA Property Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "bedrooms",
        label = "Number of Bedrooms",
        min = 1,
        max = 10,
        value = 1
      )
    ),
    
    mainPanel(
      plotOutput(
        outputId = "priceHist"
      )
    )
  )
)

server <- function(input, output, session) {
  source("LoadPropertyData.R")
  propData <- load_all_property_data()
  
  output$priceHist <- renderPlot(
    propData %>%
      filter(Interior_Bedrooms == input$bedrooms) %>%
      ggplot(aes(x = AssessedValue)) + geom_histogram(bins = 50) + scale_x_log10()
  )
}

shinyApp(ui = ui, server = server)
