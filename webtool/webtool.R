library(tidyverse)
library(shiny)
library(DT)
library(here)
library(leaflet)
library(lamadex, lib(here("/lamadex/lamadex")))

##globals: load list of countries and raw data
source(here("lamadex", "R", "source", "countryList.R"))
source(here("lamadex", "R", "source", "data_loader.R"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Youth Labor Market Index For Low-Income Countries"),
  
fluidRow(
  
  column(3,
         selectInput("gender", "Gender", c("Total", "Male", "Female")),
         checkboxInput("impute", "Impute missing values", value = TRUE),
         submitButton("Submit"),
         br()
         ),
  column(3,
         checkboxGroupInput("checkGroup", 
                     h5("Aggregation of "), 
                     choices = list("Arithmetic" = 1, 
                                    "Geometric" = 2),
                     selected = 1))),
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Total", DT::dataTableOutput("total_ranks")),
      tabPanel("Male", DT::dataTableOutput("male_ranks")),
      tabPanel("Female", DT::dataTableOutput("female_ranks"))
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
    
  output$mymap <- renderLeaflet({
      
    leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
                )
  })
  
  output$total_ranks <- DT::renderDataTable({
    rank <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = input$impute) %>% 
      select(
        Country = country,
        "YLILI score" = index_mean,
        "Transition dimension" = transition_mean,
        "Working conditions dimension" = working_conditions_mean,
        "Education dimension" = education_mean,
        "NEET rate" = neet,
        "Working Conditions Ratio" = relative_wc,
        "Mismatch rate" = mismatch,
        "Working poverty rate" = workingpov,
        "Under- employment rate" = underemp,
        "Informal work rate" = informal,
        "Elementary occupation rate" = elementary,
        "No secondary schooling" = nosecondary,
        "Literacy rate" = literacy,
        "Harmonized test scores" = test_scores
      ) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank), backgroundColor = styleInterval(brks, clrs))
  })
  
  output$male_ranks <- DT::renderDataTable({
    rank <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = 2010, impute = input$impute) %>% 
      select(
        Country = country,
        "YLILI score" = index_mean,
        "Transition dimension" = transition_mean,
        "Working conditions dimension" = working_conditions_mean,
        "Education dimension" = education_mean,
        "NEET rate" = neet,
        "Working Conditions Ratio" = relative_wc,
        "Mismatch rate" = mismatch,
        "Working poverty rate" = workingpov,
        "Under- employment rate" = underemp,
        "Informal work rate" = informal,
        "Elementary occupation rate" = elementary,
        "No secondary schooling" = nosecondary,
        "Literacy rate" = literacy,
        "Harmonized test scores" = test_scores
      ) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank), backgroundColor = styleInterval(brks, clrs))
  })
  
  output$female_ranks <- DT::renderDataTable({
    rank <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = 2010, impute = input$impute) %>% 
      select(
        Country = country,
        "YLILI score" = index_mean,
        "Transition dimension" = transition_mean,
        "Working conditions dimension" = working_conditions_mean,
        "Education dimension" = education_mean,
        "NEET rate" = neet,
        "Working Conditions Ratio" = relative_wc,
        "Mismatch rate" = mismatch,
        "Working poverty rate" = workingpov,
        "Under- employment rate" = underemp,
        "Informal work rate" = informal,
        "Elementary occupation rate" = elementary,
        "No secondary schooling" = nosecondary,
        "Literacy rate" = literacy,
        "Harmonized test scores" = test_scores
      ) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank), backgroundColor = styleInterval(brks, clrs))
  })
  
}

shinyApp(ui = ui, server = server)

