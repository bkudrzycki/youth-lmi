library(devtools)
library(DT)
library(leaflet)
library(openxlsx)
library(shiny)
library(tidyverse)
library(usethis)
library(shinythemes)
library(viridis)
library(tigris)
library(rgdal)
library(leaflet.extras)
library(shinyWidgets)
library(gridExtra)
#devtools::install_github("bkudrzycki/youth-lmi/lamadex", quiet = TRUE, upgrade = "always")
library(lamadex)
library(RColorBrewer)
library(shinyBS)

## ---------------------------------------

# load map, shapefile name "countries", country names saved as NAME
source("data/recode_shapeFile.R")
country_list_for_dropdown <- countryLists()[[3]][[1]] %>% sort()
# globals: load list of countries and raw data, define geometric mean function

gm_mean = function(x, na.rm = FALSE) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x[!is.na(x)]))
}

# background color
css <- HTML(" body {
    background-color: #4e5d6c;
}")

tags$head(
  tags$style(
    ".radio-inline{margin-left:10px;"
  )
)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: #4e5d6c; }")),
    navbarPage("YLILI", theme = shinytheme("superhero"),
               tabPanel("Youth Labor Index for Low-Income Countries", fluid = TRUE,
                        tags$head(
                          tags$style(css)),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                          sidebarPanel(
                            # App title ----
                            titlePanel("Data Explorer"),
                            fluidRow(
                              column(9,
                                     sliderInput("years", h6(style="height:20px;", "Data range:",
                                                             bsButton("q1", label = "", icon = icon("fal fa-question-circle"))),
                                                 min = 2000, max = 2020,
                                                 value = c(2010, 2020), sep = "", ticks = FALSE),
                                     bsPopover(id = "q1", title = "",
                                               content = "Must start in 2017 or earlier. For each indicator, the most recent observation available in the chosen range will be used to generate YLILI.",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")
                                     ),
                                     selectInput("score_agg", h6(style="height:20px;", "Index aggregation:",
                                                                 bsButton("q2", label = "", icon = icon("fal fa-question-circle"))),
                                                                 c("Arithmetic", "Geometric")),
                                     bsPopover(id = "q2", title = "",
                                               content = "Arithmetic = simple average <br> Geometric = cubed root of the product of the three dimensions",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")
                                     ),
                                     selectInput("dim_agg", label = h6(style="height:20px;", "Dimension aggregation:",
                                                                       bsButton("q3", label = "", icon = icon("fal fa-question-circle"))),
                                                 c("Arithmetic", "Geometric")),
                                     bsPopover(id = "q3", title = "",
                                               content = "Arithmetic = simple average <br> Geometric = <i>n</i><sup>th</sup> root of product of <i>n</i> indicators available for dimension",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")
                                     ),
                                     selectInput("gender", label = h6(style="height:20px;", "Gender:",
                                                                      bsButton("q4", label = "", icon = icon("fal fa-question-circle"))), c("Total", "Male", "Female")),
                                     bsPopover(id = "q4", title = "",
                                               content = "Generate YLILI for the total youth population or for males and females separately.",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")
                                     ),
                                     div(
                                     checkboxInput("impute", label = h6(style="position: relative;top: -18px; height:0px;", "Impute missing values:", bsButton("q5", label = "", icon = icon("fal fa-question-circle"))), value = TRUE)
                                     ),
                                     bsPopover(id = "q5", title = "",
                                               content = "Replaces missing observations with estimated values. Imputation is based on countries\\' relative performance on indicators in the same dimension as the missing value.",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")
                                     ),
                                     radioButtons("subset", label = h6(style="height:20px;", "Countries Ranked:", bsButton("q6", label = "", icon = icon("fal fa-question-circle"))), choices = list("All", "LICs/LMICs"), inline = T, selected = "LICs/LMICs"),
                                     bsPopover(id = "q6", title = "",
                                               content = "LICs/LMICs = low-income and lower-middle income countries, 2021 World Bank classification",
                                               placement = "right", 
                                               trigger = "hover", 
                                               options = list(container = "body")),
                                     hr(),
                                     downloadButton("dl", "Download .Excel"))
                            ),
                            width = 3,
                          ),
                          mainPanel(
                            tabsetPanel(
                              id = 'dataset',
                              tabPanel("Map",
                                       style = "height:92vh;",
                                       leafletOutput("map", width = "110%", height = "93%"),
                                       tags$style("
        #controls {
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
                                       absolutePanel(id = "controls", top = 50, left = 10,
                                                     selectizeInput("select", "Show on map:",
                                                                    choices = list(
                                                                      "YLILI Score" = "YLILI score",
                                                                      Dimensions = c("Transition" = "Transition",
                                                                                     "Working conditions" = "Working conditions",
                                                                                     "Education" = "Education"),
                                                                      Transition = c("NEET score" = "NEET score",
                                                                                     "Working conditions ratio" =  "Working conditions ratio",
                                                                                     "Mismatch score" = "Mismatch score"),
                                                                      `Working Conditions` = c("Working poverty score" = "Working poverty score",
                                                                                               "Underemployment score" = "Underemployment score",
                                                                                               "Informal work score" = "Informal work score",
                                                                                               "Elementary occupation score" = "Elementary occupation score"),
                                                                      Education = c("Secondary schooling rate" = "Secondary schooling rate",
                                                                                    "Literacy rate" = "Literacy rate",
                                                                                    "Harmonized tests score" = "Harmonized tests score")),
                                                                      multiple = FALSE))),
                                       tabPanel("Scores", radioButtons("table", label = "", choices = list("Scores", "Ranks"),  inline = T),
                                                DT::dataTableOutput("scores")),
                                       tabPanel("Country Comparison", 
                                                fluidRow(
                                                  column(4,
                                                         selectInput("country1", "", choices = country_list_for_dropdown)),
                                                  column(4,
                                                  ),
                                                  column(4,
                                                         selectInput("country2", "", choices = country_list_for_dropdown)),
                                                  plotOutput("test", width = "100%")
                                                )
                                       )
                              )
                            )
                          )
                        ),
                        tabPanel("About",
                        fluidRow(
                          column(8,
                                 tags$h3("About the YLILI"),
                                 tags$h5("The Youth Labor Market Index for Low Income Countries scores countries on the strength of the opportunities for decent and gainful work afforded their youth. The work done by youth in developing countries is overwhelmingly informal, and thus not well captured by a single indicator (like the youth unemployment rate). Instead, the YLILI ranks economies on a scale from 1-100 on various measures of labor market quality, with a higher score always indicating better performance -- for instance, a high underemployment score means that youth underemployment is low. Indicators are grouped into three categories, each consisting of 3-4 indicators, thus giving equal weight to each of the following topics: transition smoothness, working conditions, and education. Users can recreate the scores and rankings used in the paper by Kudrzycki, Lefoll, and Günther (2021), or adjust the parameters used to generate a new set of scores."),
                                 tags$h4("Data sources"),
                                 tags$h5("- The indicators underlying the youth NEET score, working conditions ratio, mismatch score, working poverty score, underemployment score, and elementary occupation score were obtained from the International Labour Organization's", tags$a("ILOSTAT", href="https://ilostat.ilo.org/data/"), "."),
                                 tags$h5("- The youth literacy indicator was obtained from ", tags$a("UNESCO", href="http://data.uis.unesco.org/"), "."),
                                 tags$h5("- The data for youth secondary school attainment was obtained from the", tags$a("DHS program", href="https://www.statcompiler.com/en/"), "."),
                                 tags$h5("- Harmonized test score data was obtained from the", tags$a("World Bank DataBank", href="https://databank.worldbank.org/reports.aspx?source=3698&series=HD.HCI.HLOS"), "."))
                        )
               )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # generate index according to user specification
  country_input <- reactive(
    if (input$subset == "All") {
      "all"
    }
    else {
      "dev"
    }
  )
  
  reactiveIndex <- reactive(rank_generator(bygender = input$gender, countries = country_input(), years = input$years, impute = input$impute) %>% 
                              rowwise() %>%
                              mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                                     wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                                     educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom),
                                     arith_score = mean(c(transdim,wcdim,educdim)),
                                     geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>%  # don't generate if missing dims
                              mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)) %>% 
                              select(
                                Country = country,
                                "YLILI score" = score,
                                "Transition" = transdim,
                                "NEET score" = neet,
                                "Working conditions ratio" = relative_wc,
                                "Mismatch score" = mismatch,
                                "Working conditions" = wcdim,
                                "Working poverty score" = workingpov,
                                "Underemployment score" = underemp,
                                "Informal work score" = informal,
                                "Elementary occupation score" = elementary,
                                "Education" = educdim,
                                "Secondary schooling rate" = nosecondary,
                                "Literacy rate" = literacy,
                                "Harmonized tests score" = test_scores
                              )
  )
  
  
  tot_ylili <- reactive(rank_generator(bygender = "Total", countries = country_input(), years = input$years, impute = input$impute) %>% 
                          rowwise() %>%
                          rename("Country" = "country") %>% 
                          mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                                 wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                                 educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom),
                                 arith_score = mean(c(transdim,wcdim,educdim)),
                                 geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>%  # don't generate if missing dims
                          mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)))
  
  observeEvent(input$subset, {
    reactive_country_list_for_dropdown <- reactive({
      reactiveIndex()[rowSums(!is.na(reactiveIndex()[6:15])) > 0, ] %>% 
        select(Country) %>% 
        t() %>% 
        as.vector()
    })
    
    updateSelectInput(session, "country1",
                      choices = reactive_country_list_for_dropdown(),
                      selected = sample(reactive_country_list_for_dropdown(), 1))
    updateSelectInput(session, "country2",
                      choices = reactive_country_list_for_dropdown(),
                      selected = sample(reactive_country_list_for_dropdown(), 1))
  })
  
  ## Last year cannot be earlier than 2018
  observeEvent(input$years,{
    lastyear <- c(min((input$years[1]), 2017), input$years[2])
    updateSliderInput(session, "years", min = 2000, max = 2020, value = lastyear)
  })
  
  
  observe({
    
    indicator <- input$select
    gender <- input$gender
    subset <- input$subset
    chosen_indicator <- reactive(reactiveIndex()[, c("Country", as.character(input$select)), drop=FALSE])
    scores<-reactive(left_join(data.frame(Country = countries$NAME%>%as.character()), chosen_indicator(), by = "Country"))
    
    pal <- reactive(colorNumeric(inferno(256, begin = 0.1), domain = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)), na.color = "#D3D3D3"))
    
    countries1 <- reactive(merge(countries,
                                 tot_ylili(),
                                 by.x = "NAME",
                                 by.y = "Country",
                                 duplicateGeoms = TRUE,
                                 sort = FALSE))
    
    countries2 <- reactive(merge(countries,
                                 reactiveIndex(),
                                 by.x = "NAME",
                                 by.y = "Country",
                                 duplicateGeoms = TRUE,
                                 sort = FALSE))
    
    table <- data.frame(a = 1:3, b= c("a", "b", "c"))
    
    if (indicator == "YLILI score" & gender == "Total") {
      country_popup <- paste0("<h4><strong>", countries2()$NAME, "</strong>",
                              "<br><strong><h5>",
                              hr(),
                              input$gender, " ", as.character(indicator),": </strong>",
                              round(countries2()[[indicator]],2), " (", rank(-countries2()[[indicator]], na.last = "keep"), "/", length(na.omit(countries2()[[indicator]])), ")",
                              hr())
    } else {
      country_popup <- paste0("<h4><strong>", countries2()$NAME, "</strong>",
                              "<br><strong><h5>",
                              hr(),
                              input$gender, " ", as.character(indicator),": </strong>",
                              round(countries2()[[indicator]],2), " (", rank(-countries2()[[indicator]], na.last = "keep"), "/", length(na.omit(countries2()[[indicator]])), ")",
                              hr(),
                              "<h6><strong> Total YLILI score: </strong>", round(countries1()[["score"]],2), " (", rank(-countries1()[["score"]], na.last = "keep"), "/", length(na.omit(countries1()[["score"]])), ")<br>")
    }
    
    output$map <- renderLeaflet({
      
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      leaflet(countries2(), options = leafletOptions(
        minZoom = 2, maxZoom = 5,
        zoomControl = FALSE,
        attributionControl=FALSE)) %>% 
        addPolygons(data = countries2(),
                    fillColor = ~pal()(countries2()[[indicator]]),
                    layerId = ~NAME, weight = 1, smoothFactor = 0.5,
                    opacity = .8, fillOpacity = .8,  color = "#808080",
                    highlightOptions = highlightOptions(color = "black", weight = 2, opacity = .8),
                    popup = country_popup) %>% 
        setView(15, 20, zoom = 2) %>% 
        setMaxBounds(lng1 = -200,
                     lat1 = -90,
                     lng2 = 200,
                     lat2 = 90) %>% 
        addLegend(position = "bottomright",
                  opacity = 0.8,
                  pal = pal(),
                  value = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)))
    })
  })
  
  
  observeEvent(input$table, {
    
    # generate table with dynamic color-coding
    scores <- DT::renderDataTable({
      rank <- reactiveIndex() %>% 
        mutate_if(is.numeric, round, 3) %>% 
        arrange(desc(`YLILI score`))
      nums <- rank %>% select_if(is.numeric)
      brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs_index <- viridis::inferno(n=length(brks)+1, alpha=.5, direction = -1)
      clrs_trans <- colorRampPalette(brewer.pal(4, "Blues"))(length(brks)+1)
      clrs_wc <- colorRampPalette(brewer.pal(4, "Greens"))(length(brks)+1)
      clrs_educ <- colorRampPalette(brewer.pal(4, "Oranges"))(length(brks)+1)
      DT::datatable(rank, options = list(paging = FALSE,
                                         searching = FALSE,
                                         headerCallback = JS(
                                           "function( thead, data, start, end, display ) {
                                           $(thead).closest('thead').find('th').eq(1).css('color', 'white');
      $(thead).closest('thead').find('th').eq(2).css('color', 'white');
      $(thead).closest('thead').find('th').eq(3).css('color', 'white');
      $(thead).closest('thead').find('th').eq(4).css('color', 'white');
      $(thead).closest('thead').find('th').eq(5).css('color', 'white');
      $(thead).closest('thead').find('th').eq(6).css('color', 'white');
      $(thead).closest('thead').find('th').eq(7).css('color', 'white');
      $(thead).closest('thead').find('th').eq(8).css('color', 'white');
      $(thead).closest('thead').find('th').eq(9).css('color', 'white');
      $(thead).closest('thead').find('th').eq(10).css('color', 'white');
      $(thead).closest('thead').find('th').eq(11).css('color', 'white');
      $(thead).closest('thead').find('th').eq(12).css('color', 'white');
      $(thead).closest('thead').find('th').eq(13).css('color', 'white');
      $(thead).closest('thead').find('th').eq(14).css('color', 'white');
      $(thead).closest('thead').find('th').eq(15).css('color', 'white');
              }"))) %>% 
        formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
        formatStyle(names(rank[c("YLILI score", "Transition", "Working conditions", "Education")]), fontWeight = 'bold') %>% 
        formatStyle(names(rank[c("Transition", "NEET score", "Working conditions ratio", "Mismatch score")]), backgroundColor = styleInterval(brks, clrs_trans)) %>% 
        formatStyle(names(rank[c("Working conditions", "Working poverty score", "Underemployment score", "Informal work score", "Elementary occupation score")]), backgroundColor = styleInterval(brks, clrs_wc)) %>% 
        formatStyle(names(rank[c("Education", "Secondary schooling rate", "Literacy rate", "Harmonized tests score")]), backgroundColor = styleInterval(brks, clrs_educ))
    })
    
    ## find countries with at least one observed data point (for country comparison)
    
    ranks <- DT::renderDataTable({
      rank <- reactiveIndex() %>% 
        ungroup() %>% 
        mutate(
          "YLILI score" = rank(-`YLILI score`, na.last = "keep"),
          "Transition" = rank(-`Transition`, na.last = "keep"),
          "NEET score" = rank(-`NEET score`, na.last = "keep"),
          "Working conditions ratio" = rank(-`Working conditions ratio`, na.last = "keep"),
          "Mismatch score" = rank(-`Mismatch score`, na.last = "keep"),
          "Working conditions" = rank(-`Working conditions`, na.last = "keep"),
          "Working poverty score" = rank(-`Working poverty score`, na.last = "keep"),
          "Underemployment score" = rank(-`Underemployment score`, na.last = "keep"),
          "Informal work score" = rank(-`Informal work score`, na.last = "keep"),
          "Elementary occupation score" = rank(-`Elementary occupation score`, na.last = "keep"),
          "Education" = rank(-`Education`, na.last = "keep"),
          "Secondary schooling rate" = rank(-`Secondary schooling rate`, na.last = "keep"),
          "Literacy rate" = rank(-`Literacy rate`, na.last = "keep"),
          "Harmonized tests score" = rank(-`Harmonized tests score`, na.last = "keep")
        ) %>%      
        mutate_if(is.numeric, round, 3) %>% 
        arrange(`YLILI score`)
      nums <- rank %>% select_if(is.numeric)
      brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs_index <- viridis::inferno(n=length(brks)+1, alpha=.5, direction = -1)
      clrs_trans <- colorRampPalette(brewer.pal(4, "Blues"))(length(brks)+1)
      clrs_wc <- colorRampPalette(brewer.pal(4, "Greens"))(length(brks)+1)
      clrs_educ <- colorRampPalette(brewer.pal(4, "Oranges"))(length(brks)+1)
      DT::datatable(rank, options = list(paging = FALSE,
                                         searching = FALSE,
                                         headerCallback = JS(
                                           "function( thead, data, start, end, display ) {
                                           $(thead).closest('thead').find('th').eq(1).css('color', 'white');
      $(thead).closest('thead').find('th').eq(2).css('color', 'white');
      $(thead).closest('thead').find('th').eq(3).css('color', 'white');
      $(thead).closest('thead').find('th').eq(4).css('color', 'white');
      $(thead).closest('thead').find('th').eq(5).css('color', 'white');
      $(thead).closest('thead').find('th').eq(6).css('color', 'white');
      $(thead).closest('thead').find('th').eq(7).css('color', 'white');
      $(thead).closest('thead').find('th').eq(8).css('color', 'white');
      $(thead).closest('thead').find('th').eq(9).css('color', 'white');
      $(thead).closest('thead').find('th').eq(10).css('color', 'white');
      $(thead).closest('thead').find('th').eq(11).css('color', 'white');
      $(thead).closest('thead').find('th').eq(12).css('color', 'white');
      $(thead).closest('thead').find('th').eq(13).css('color', 'white');
      $(thead).closest('thead').find('th').eq(14).css('color', 'white');
      $(thead).closest('thead').find('th').eq(15).css('color', 'white');
              }"))) %>% 
        formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
        formatStyle(names(rank[c("YLILI score", "Transition", "Working conditions", "Education")]), fontWeight = 'bold') %>% 
        formatStyle(names(rank[c("Transition", "NEET score", "Working conditions ratio", "Mismatch score")]), backgroundColor = styleInterval(brks, clrs_trans)) %>% 
        formatStyle(names(rank[c("Working conditions", "Working poverty score", "Underemployment score", "Informal work score", "Elementary occupation score")]), backgroundColor = styleInterval(brks, clrs_wc)) %>% 
        formatStyle(names(rank[c("Education", "Secondary schooling rate", "Literacy rate", "Harmonized tests score")]), backgroundColor = styleInterval(brks, clrs_educ))
    })
    
    if (input$table == "Scores") {
      output$scores <- scores
    } else {
      output$scores <- ranks
    }
    
    #generate data
    data_list <- reactive({
      list(
        total = rank_generator(bygender = input$gender, countries <- country_input(), years = input$years, impute = input$impute) %>% 
          rowwise() %>%
          mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                 wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                 educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom)) %>% 
          mutate(arith_score = mean(c(transdim,wcdim,educdim)),
                 geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>% 
          mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)) %>% 
          select(
            Country = country,
            "YLILI score" = score,
            "Transition" = transdim,
            "NEET score" = neet,
            "Working conditions ratio" = relative_wc,
            "Mismatch score" = mismatch,
            "Working conditions" = wcdim,
            "Working poverty score" = workingpov,
            "Underemployment score" = underemp,
            "Informal work score" = informal,
            "Elementary occupation score" = elementary,
            "Education" = educdim,
            "Secondary schooling rate" = nosecondary,
            "Literacy rate" = literacy,
            "Harmonized tests score" = test_scores
          ) %>% 
          arrange(desc(`YLILI score`)),
        male = rank_generator(bygender = "Male", countries <- country_input(), years = input$years, impute = input$impute) %>% 
          rowwise() %>%
          mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                 wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                 educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom)) %>% 
          mutate(arith_score = mean(c(transdim,wcdim,educdim)),
                 geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>% 
          mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)) %>% 
          select(
            Country = country,
            "YLILI score" = score,
            "Transition" = transdim,
            "NEET score" = neet,
            "Working conditions ratio" = relative_wc,
            "Mismatch score" = mismatch,
            "Working conditions" = wcdim,
            "Working poverty score" = workingpov,
            "Underemployment score" = underemp,
            "Informal work score" = informal,
            "Elementary occupation score" = elementary,
            "Education" = educdim,
            "Secondary schooling rate" = nosecondary,
            "Literacy rate" = literacy,
            "Harmonized tests score" = test_scores
          ) %>% 
          arrange(desc(`YLILI score`)),
        female = rank_generator(bygender = "Female", countries <- country_input(), years = input$years, impute = input$impute) %>% 
          rowwise() %>%
          mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                 wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                 educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom)) %>% 
          mutate(arith_score = mean(c(transdim,wcdim,educdim)),
                 geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>% 
          mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)) %>% 
          select(
            Country = country,
            "YLILI score" = score,
            "Transition" = transdim,
            "NEET score" = neet,
            "Working conditions ratio" = relative_wc,
            "Mismatch score" = mismatch,
            "Working conditions" = wcdim,
            "Working poverty score" = workingpov,
            "Underemployment score" = underemp,
            "Informal work score" = informal,
            "Elementary occupation score" = elementary,
            "Education" = educdim,
            "Secondary schooling rate" = nosecondary,
            "Literacy rate" = literacy,
            "Harmonized tests score" = test_scores
          ) %>% 
          arrange(desc(`YLILI score`))
      )
    })
    
    output$dl <- downloadHandler(
      filename = function() {"ylili.xlsx"},
      content = function(file) {write.xlsx(data_list(), file)}
    )
    
  })
  
  toListen <- reactive({
    list(input$country1, input$country2, input$years, input$dim_agg, input$score_agg, input$gender, input$impute)
  })
  
  observeEvent(toListen(), {
    
    # back-to-back bar graph
    
    left <- reactive({
      reactiveIndex() %>% 
        filter(Country == input$country1) %>% 
        select(-Country) %>% 
        t() %>%
        as.data.frame() %>%
        mutate(var = rownames(.),
               var = fct_relevel(var, "Harmonized tests score",
                                 "Literacy rate",
                                 "Secondary schooling rate",
                                 "Elementary occupation score",
                                 "Informal work score",
                                 "Underemployment score",
                                 "Working poverty score",
                                 "Mismatch score",
                                 "Working conditions ratio",
                                 "NEET score",
                                 "Education",
                                 "Working conditions",
                                 "Transition",
                                 "YLILI score"),
               label = ifelse(V1 > 19.99, round(V1, 2), NA))
    })
    
    right <- reactive({
      reactiveIndex() %>% 
        filter(Country == input$country2) %>% 
        select(-Country) %>% 
        t() %>%
        as.data.frame() %>%
        mutate(var = rownames(.),
               var = fct_relevel(var, "Harmonized tests score",
                                 "Literacy rate",
                                 "Secondary schooling rate",
                                 "Elementary occupation score",
                                 "Informal work score",
                                 "Underemployment score",
                                 "Working poverty score",
                                 "Mismatch score",
                                 "Working conditions ratio",
                                 "NEET score",
                                 "Education",
                                 "Working conditions",
                                 "Transition",
                                 "YLILI score"),
               label = ifelse(V1 > 19.99, round(V1, 2), NA))
    })
    
    # Center labels
    g.mid<-ggplot(left(), aes(x=1,y=left()$var)) + geom_text(aes(label=left()$var, color = "NEET score"), size = rel(5), na.rm = TRUE) +
      scale_colour_manual(values = c("white")) +
      ggtitle("")+
      ylab(NULL)+
      scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065)) +
      theme_minimal() +
      theme(text = element_text(color="white"),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_line(color=NA),
            plot.margin = unit(c(-5,-5,5,-5), "mm"),
            legend.position="none")
    
    g1 <- ggplot(data = left(),  aes(y= V1, x = var, label = label)) +
      geom_bar(stat = "identity", fill = "white") +
      geom_text(size = 4, hjust = -.3, na.rm=TRUE) +
      theme_minimal() +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.x=element_text(color = "white"),
            plot.margin = unit(c(1,-1,1,-1), "mm")) +
      scale_y_reverse(limits=c(100,0)) + coord_flip()
    
    
    g2 <- ggplot(data = right(),  aes(y= V1, x = var, label = label)) +
      geom_bar(stat = "identity", aes(y= V1, x = var), fill = "white") +
      geom_text(size = 4, hjust = 1.15, na.rm=TRUE) +
      ylim(0,100) +
      theme_minimal() +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.x=element_text(color = "white"),
            plot.margin = unit(c(1,-1,1,-1), "mm")) +
      coord_flip()
    
    output$test <- renderPlot({
      grid.arrange(g1,g.mid, g2,ncol=3, widths = c(3,2,3))
    }, bg="transparent")
    
  })
  
}


app <- shinyApp(ui = ui, server = server)

#runApp(app, launch.browser = TRUE)
#runGitHub("webtool_test", "bkudrzycki", launch.browser = TRUE)
