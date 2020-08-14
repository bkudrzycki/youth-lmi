library(shiny)
library(here)
library(leaflet)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Youth Labor Market Index For Low-Income Countries"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel("Interactive Data"),
    mainPanel("World Map")
  ),
  
      leafletOutput("mymap"),
      p()
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
    
  output$mymap <- renderLeaflet({
      
    leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        )
      
    
  })
  
}

shinyApp(ui = ui, server = server)

