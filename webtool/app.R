# Package names
packages <- c("devtools", "DT", "here", "leaflet", "openxlsx", "shiny", "tidyverse", "usethis", "shinythemes", "viridis", "tigris", "rgdal", "leaflet.extras", "shinyWidgets")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], quiet = TRUE)
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))
## ---------------------------------------

devtools::install_github("bkudrzycki/youth-lmi/lamadex", quiet = TRUE, upgrade = "always")

library(lamadex)

# load map, shapefile name "countries", country names saved as NAME

load(here("data", "shapeFile.RData"))

# globals: load list of countries and raw data, define geometric mean function
source(here("lamadex", "R", "source", "countryList.R"))
source(here("lamadex", "R", "source", "data_loader.R"))
gm_mean = function(x, na.rm = FALSE) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x[!is.na(x)]))
}

# background color
css <- HTML(" body {
    background-color: #4e5d6c;
}")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: #4e5d6c; }"))
  ),
  navbarPage("YLILI", theme = shinytheme("superhero"),
             tags$head(
               tags$style(css)),
             tabPanel("Youth Labor Market Index for Low-Income Countries", fluid = TRUE),
             # Sidebar layout with a input and output definitions
             sidebarLayout(
               sidebarPanel(
                 # App title ----
                 titlePanel("Data Explorer"),
                 fluidRow(
                   column(8,
                          checkboxInput("impute", "Impute missing values", value = TRUE),
                          sliderInput("lastyear", "Last year:",
                                      min = 2000, max = 2017,
                                      value = 2010, sep = "", ticks = FALSE),
                          selectInput("dim_agg", "Dimension aggregation", c("Arithmetic", "Geometric")),
                          selectInput("score_agg", "Index aggregation", c("Arithmetic", "Geometric")),
                          selectInput("gender", "Gender", c("Total", "Male", "Female")),
                          hr())
                 ),
                 fluidRow(
                   column(4,
                          submitButton("Update")
                   ),
                   column(4,
                          downloadButton("dl", "Download .Excel")
                   )
                 ),
                 fluidRow(
                   column(8,
                          hr(),
                          selectizeInput("select", "Show on map:",
                                         c("YLILI Score" = "YLILI score",
                                           "Transition" = "Transition",
                                           "Working conditions" = "Working conditions",
                                           "Education" = "Education",
                                           "NEET score" = "NEET score",
                                           "Working conditions ratio" =  "Working conditions ratio",
                                           "Mismatch score" = "Mismatch score",
                                           "Working poverty score" = "Working poverty score",
                                           "Under- employment score" = "Under- employment score",
                                           "Informal work score" = "Informal work score",
                                           "Elementary occupation score" = "Elementary occupation score",
                                           "Secondary schooling rate" = "Secondary schooling rate",
                                           "Literacy rate" = "Literacy rate",
                                           "Harmonized tests score" = "Harmonized tests score"),
                                         multiple = FALSE)
                   )
                 ),
                 width = 2,
               ),
               mainPanel(
                 tabsetPanel(
                   id = 'dataset',
                   tabPanel("Map",
                            style = "height:92vh;",
                            leafletOutput("map", width = "120%", height = "93%")),
                   tabPanel("Scores", DT::dataTableOutput("scores")),
                   tabPanel("Ranks", DT::dataTableOutput("ranks"))
                 )
               )
             )
  )
)


# Define server logic
server <- function(input, output) {
  
  # generate index according to user specification
  reactiveIndex <- reactive(rank_generator(dfList, country_lists[[3]], bygender = input$gender, lastyear = input$lastyear, impute = input$impute) %>% 
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
                                "Working conditions" = wcdim,
                                "Education" = educdim,
                                "NEET score" = neet,
                                "Working conditions ratio" = relative_wc,
                                "Mismatch score" = mismatch,
                                "Working poverty score" = workingpov,
                                "Under- employment score" = underemp,
                                "Informal work score" = informal,
                                "Elementary occupation score" = elementary,
                                "Secondary schooling rate" = nosecondary,
                                "Literacy rate" = literacy,
                                "Harmonized tests score" = test_scores
                              )
  )
  
  
  observe({
    
    indicator <- input$select
    chosen_indicator <- reactive(reactiveIndex()[, c("Country", as.character(input$select)), drop=FALSE])
    
    scores<-reactive(left_join(data.frame(Country = countries$NAME%>%as.character()), chosen_indicator()))
    
    pal <- reactive(colorNumeric(c("#FFFFFFFF", viridis(256)), domain = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)), na.color = "white"))
    
    # total number of countries ranked
    num_ranked <- reactive(colSums(!is.na(reactiveIndex()["YLILI score"])))
    

    countries2 <- reactive(merge(countries,
                                 reactiveIndex(),
                                 by.x = "NAME",
                                 by.y = "Country",
                                 sort = FALSE))
    
    table <- data.frame(a = 1:3, b= c("a", "b", "c"))
    
    country_popup <- paste0("<h4><strong>Country:  </strong>",
                            countries2()$NAME,
                            "<br><strong>",
                            input$gender, " ", as.character(indicator),": </strong>",
                            round(countries2()[[indicator]],2), " (", rank(-countries2()[[indicator]], na.last = "keep"), "/", num_ranked(), ")",                             " </h4>",
                            hr(),
                            "<strong>", input$gender, " YLILI score: </strong>", round(countries2()[["YLILI score"]],2), " (", rank(-countries2()$`YLILI score`, na.last = "keep"), "/", num_ranked(), ")",
                            hr(),
                            "<strong>", input$gender, " Transition score: </strong>", round(countries2()[["Transition"]],2), " (", rank(-countries2()$Transition, na.last = "keep"), "/", num_ranked(), ")",
                            "<br> NEET rate: ", round(countries2()[["NEET score"]],2),
                            "<br> Working conditions ratio: ", round(countries2()[["Working conditions ratio"]],2),
                            "<br> Mismatch rate: ", round(countries2()[["Mismatch score"]],2),
                            hr(),
                            "<strong>", input$gender, " Working Conditions score: </strong>", round(countries2()[["Working conditions"]],2), " (", rank(-countries2()$`Working conditions`, na.last = "keep"), "/", num_ranked(), ")",
                            "<br> Working poverty rate: ", round(countries2()[["Working poverty score"]],2),
                            "<br> Underemployed rate: ", round(countries2()[["Under- employment score"]],2),
                            "<br> Informal work rate: ", round(countries2()[["Informal work score"]],2),
                            "<br> Elementary occupations: ", round(countries2()[["Elementary occupation score"]],2),
                            hr(),
                            "<strong>", input$gender, " Education score: </strong>", round(countries2()[["Education"]],2), " (", rank(-countries2()$Education, na.last = "keep"), "/", num_ranked(), ")",
                            "<br> Secondary school rate: ", round(countries2()[["Secondary schooling rate"]],2),
                            "<br> Literacy rate: ", round(countries2()[["Literacy rate"]],2),
                            "<br> Harmonized test scores: ", round(countries2()[["Harmonized tests score"]],2)
                            )
    
    output$map <- renderLeaflet({
      
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      leaflet(countries2()) %>% 
        addPolygons(data = countries2(),
                    fillColor = ~pal()(countries2()[[indicator]]),
                    layerId = ~NAME, weight = 1, smoothFactor = 0.5,
                    opacity = .8, fillOpacity = .8,  color = "#BDBDC3",
                    highlightOptions = highlightOptions(color = "black", weight = 2, opacity = .8),
                    popup = country_popup) %>% 
        setView(10, 20, zoom = 3) %>% 
        addLegend(position = "bottomright",
                  pal = pal(),
                  value = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)))
    })
  })
  
  
  # generate table with dynamic color-coding
  output$scores <- DT::renderDataTable({
    rank <- reactiveIndex() %>% 
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs_index <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    clrs_dims <- round(seq(150, 80, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", 75+.,",", ., ")")}
    clrs <- round(seq(200, 120, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", ., ",255)")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
      formatStyle(names(rank[c("Transition", "Working conditions", "Education")]), backgroundColor = styleInterval(brks, clrs_dims)) %>% 
      formatStyle(names(rank[c(6:ncol(rank))]), backgroundColor = styleInterval(brks, clrs))
  })
  
  output$ranks <- DT::renderDataTable({
    rank <- reactiveIndex() %>% 
      ungroup() %>% 
      mutate(
        "YLILI score" = rank(-`YLILI score`, na.last = "keep"),
        "Transition" = rank(-`Transition`, na.last = "keep"),
        "Working conditions" = rank(-`Working conditions`, na.last = "keep"),
        "Education" = rank(-`Education`, na.last = "keep"),
        "NEET score" = rank(-`NEET score`, na.last = "keep"),
        "Working conditions ratio" = rank(-`Working conditions ratio`, na.last = "keep"),
        "Mismatch score" = rank(-`Mismatch score`, na.last = "keep"),
        "Working poverty score" = rank(-`Working poverty score`, na.last = "keep"),
        "Under- employment score" = rank(-`Under- employment score`, na.last = "keep"),
        "Informal work score" = rank(-`Informal work score`, na.last = "keep"),
        "Elementary occupation score" = rank(-`Elementary occupation score`, na.last = "keep"),
        "Secondary schooling rate" = rank(-`Secondary schooling rate`, na.last = "keep"),
        "Literacy rate" = rank(-`Literacy rate`, na.last = "keep"),
        "Harmonized tests score" = rank(-`Harmonized tests score`, na.last = "keep")
      ) %>%      
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs_index <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    clrs_dims <- round(seq(150, 80, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", 75+.,",", ., ")")}
    clrs <- round(seq(200, 120, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", ., ",255)")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
      formatStyle(names(rank[c("Transition", "Working conditions", "Education")]), backgroundColor = styleInterval(brks, clrs_dims)) %>% 
      formatStyle(names(rank[c(6:ncol(rank))]), backgroundColor = styleInterval(brks, clrs))
  })
  
  #generate data
  data_list <- reactive({
    list(
      total = rank_generator(dfList, country_lists[[3]], bygender = input$gender, lastyear = input$lastyear, impute = input$impute) %>% 
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
          "Working conditions" = wcdim,
          "Education" = educdim,
          "NEET score" = neet,
          "Working conditions ratio" = relative_wc,
          "Mismatch score" = mismatch,
          "Working poverty score" = workingpov,
          "Under- employment score" = underemp,
          "Informal work score" = informal,
          "Elementary occupation score" = elementary,
          "Secondary schooling rate" = nosecondary,
          "Literacy rate" = literacy,
          "Harmonized tests score" = test_scores
        ) %>% 
        arrange(desc(`YLILI score`)),
      male = rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = input$lastyear, impute = input$impute) %>% 
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
          "Working conditions" = wcdim,
          "Education" = educdim,
          "NEET score" = neet,
          "Working conditions ratio" = relative_wc,
          "Mismatch score" = mismatch,
          "Working poverty score" = workingpov,
          "Under- employment score" = underemp,
          "Informal work score" = informal,
          "Elementary occupation score" = elementary,
          "Secondary schooling rate" = nosecondary,
          "Literacy rate" = literacy,
          "Harmonized tests score" = test_scores
        ) %>% 
        arrange(desc(`YLILI score`)),
      female = rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = input$lastyear, impute = input$impute) %>% 
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
          "Working conditions" = wcdim,
          "Education" = educdim,
          "NEET score" = neet,
          "Working conditions ratio" = relative_wc,
          "Mismatch score" = mismatch,
          "Working poverty score" = workingpov,
          "Under- employment score" = underemp,
          "Informal work score" = informal,
          "Elementary occupation score" = elementary,
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
  
}

app <- shinyApp(ui = ui, server = server)

#runApp(app, launch.browser = TRUE)
#runGitHub("webtool_test", "bkudrzycki", launch.browser = TRUE)