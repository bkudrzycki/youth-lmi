# Package names
packages <- c("devtools", "DT", "here", "leaflet", "openxlsx", "shiny", "tidyverse", "usethis", "shinythemes", "viridis")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
## ---------------------------------------

devtools::load_all(here())

# load map, shapefile name "countries", country names saved as NAME
load(here("data", "shapeFile.RData"))

##globals: load list of countries and raw data, define geometric mean function
source(here("R", "source", "countryList.R"))
source(here("R", "source", "data_loader.R"))
gm_mean = function(x, na.rm = FALSE) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x[!is.na(x)]))
}

# Define UI
ui <- fluidPage(
  navbarPage("YLILI", theme = shinytheme("lumen"),
             tabPanel("Rankings", fluid = TRUE),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          # App title ----
                          titlePanel("Youth Labor Market Index For Low-Income Countries"),
                          fluidRow(
                            column(4,
                                   checkboxInput("impute", "Impute missing values", value = TRUE),
                                   sliderInput("lastyear", "Last year:",
                                               min = 2000, max = 2017,
                                               value = 2010, sep = "", ticks = FALSE),
                                   submitButton("Submit"),
                                   br()
                                   ),
                            column(4,
                                   selectInput("dim_agg", "Dimension aggregation", c("Arithmetic", "Geometric")),
                                   selectInput("score_agg", "Index aggregation", c("Arithmetic", "Geometric")),
                                   br(),
                                   downloadButton("dl", "Download .Excel"))
                            )
                          ),
                          mainPanel(
                            tabsetPanel(
                              id = 'dataset',
                              tabPanel("Total", DT::dataTableOutput("total_ranks")),
                              tabPanel("Male", DT::dataTableOutput("male_ranks")),
                              tabPanel("Female", DT::dataTableOutput("female_ranks"))
                              )
                            )),
             tabPanel("Map", fluid = TRUE,
                      titlePanel("Map"),
                      style = "height:92vh;",
                      leafletOutput("map", width = "100%", height = "93%"))
             )
  )


# Define server logic
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet(data = countries) %>%
      
      setView(0, 30, zoom = 3)
    
    
  })
  
  
  reactiveIndex <- reactive(rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = input$lastyear, impute = input$impute) %>% 
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
  
  pal <- colorNumeric(c("#FFFFFFFF", rev(inferno(256))), domain = c(0,100))
  
  
  
  observe({
    countries2 <- merge(countries, reactiveIndex(), by.x = "NAME", by.y = "ref_area.label", sort = FALSE)
    
    leafletProxy("map", data = countries2) %>%
      addPolygons(fillColor = pal(countries2[["YLILI score"]]),
                  layerId = ~NAME,
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  weight = 1)
  }
  )
  

  output$total_ranks <- DT::renderDataTable({
    rank <- reactiveIndex() %>% 
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs_index <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    clrs_dims <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", 75+.,",", ., ")")}
    clrs <- round(seq(200, 120, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", ., ",255)")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
      formatStyle(names(rank[c("Transition", "Working conditions", "Education")]), backgroundColor = styleInterval(brks, clrs_dims)) %>% 
      formatStyle(names(rank[c(6:ncol(rank))]), backgroundColor = styleInterval(brks, clrs))
  })
  
  output$male_ranks <- DT::renderDataTable({
    rank <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = input$lastyear, impute = input$impute) %>% 
      rowwise() %>%
      mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
             wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
             educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom))
    rank <- rank %>% 
      rowwise() %>% 
      mutate(arith_score = mean(c(transdim,wcdim,educdim)),
             geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) # don't generate if missing dims
    rank <- rank %>% 
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
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs_index <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    clrs_dims <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", 75+.,",", ., ")")}
    clrs <- round(seq(200, 120, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", ., ",255)")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
      formatStyle(names(rank[c("Transition", "Working conditions", "Education")]), backgroundColor = styleInterval(brks, clrs_dims)) %>% 
      formatStyle(names(rank[c(6:ncol(rank))]), backgroundColor = styleInterval(brks, clrs))
  })
  
  output$female_ranks <- DT::renderDataTable({
    rank <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = input$lastyear, impute = input$impute) %>% 
      rowwise() %>%
      mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
             wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
             educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom))
    rank <- rank %>% 
      rowwise() %>% 
      mutate(arith_score = mean(c(transdim,wcdim,educdim)),
             geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) # don't generate if missing dims
    rank <- rank %>% 
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
      mutate_if(is.numeric, round, 3) %>% 
      arrange(desc(`YLILI score`))
    nums <- rank %>% select_if(is.numeric)
    brks <- quantile(nums, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs_index <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    clrs_dims <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", 75+.,",", ., ")")}
    clrs <- round(seq(200, 120, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(", ., ",", ., ",255)")}
    DT::datatable(rank, options = list(paging = FALSE, searching = FALSE)) %>% 
      formatStyle(names(rank["YLILI score"]), backgroundColor = styleInterval(brks, clrs_index)) %>% 
      formatStyle(names(rank[c("Transition", "Working conditions", "Education")]), backgroundColor = styleInterval(brks, clrs_dims)) %>% 
      formatStyle(names(rank[c(6:ncol(rank))]), backgroundColor = styleInterval(brks, clrs))
  })
  
  data_list <- reactive({
    list(
      total = rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = input$lastyear, impute = input$impute) %>% 
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

shinyApp(ui = ui, server = server)