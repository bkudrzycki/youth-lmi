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

## ---------------------------------------

# load map, shapefile name "countries", country names saved as NAME
source("data/recode_shapeFile.R")
country_list_for_dropdown <- read.csv("data/country_list_for_dropdown.csv")

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
               tabPanel("Youth Labor Market Index for Low-Income Countries", fluid = TRUE,
                        tags$head(
                          tags$style(css)),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                          sidebarPanel(
                            # App title ----
                            titlePanel("Data Explorer"),
                            fluidRow(
                              column(8,
                                     sliderInput("years", "Data range:",
                                                 min = 2000, max = 2020,
                                                 value = c(2010, 2020), sep = "", ticks = FALSE),
                                     selectInput("dim_agg", "Dimension aggregation", c("Arithmetic", "Geometric")),
                                     selectInput("score_agg", "Index aggregation", c("Arithmetic", "Geometric")),
                                     selectInput("gender", "Gender", c("Total", "Male", "Female")),
                                     checkboxInput("impute", "Impute missing values", value = TRUE),
                                     radioButtons("subset", label = "Countries Ranked:", choices = list("All", "LICs/LMICs"), inline = T, selected = "LICs/LMICs"),
                                     hr())
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
                                       absolutePanel(id = "controls", bottom = 50, left = 10,
                                                     tags$head(tags$style(HTML('#select+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}'))),
                                                     selectizeInput("select", "Show on map:",
                                                                    c("YLILI Score" = "YLILI score",
                                                                      "Transition" = "Transition",
                                                                      "Working conditions" = "Working conditions",
                                                                      "Education" = "Education",
                                                                      "NEET score" = "NEET score",
                                                                      "Working conditions ratio" =  "Working conditions ratio",
                                                                      "Mismatch score" = "Mismatch score",
                                                                      "Working poverty score" = "Working poverty score",
                                                                      "Underemployment score" = "Under- employment score",
                                                                      "Informal work score" = "Informal work score",
                                                                      "Elementary occupation score" = "Elementary occupation score",
                                                                      "Secondary schooling rate" = "Secondary schooling rate",
                                                                      "Literacy rate" = "Literacy rate",
                                                                      "Harmonized tests score" = "Harmonized tests score"),
                                                                    multiple = FALSE))),
                              tabPanel("Scores", radioButtons("table", label = "", choices = list("Scores", "Ranks"),  inline = T),
                                       downloadButton("dl", "Download .Excel"),
                                       DT::dataTableOutput("scores")),
                              tabPanel("Country Comparison", 
                                       fluidRow(
                                         column(4,
                                                selectInput("country1", "", sort(country_list_for_dropdown[,1]), selected = "Benin")),
                                         column(4,
                                         ),
                                         column(4,
                                                selectInput("country2", "", sort(country_list_for_dropdown[,1]), selected = "Nepal"))),
                                       plotOutput("test", width = "100%")
                              )
                            )
                          )
                        )
               ),
               tabPanel("About",
                        fluidRow(
                          column(6,
                        tags$h5("The Youth Labor Market Index for Low Income Countries (YLILI) is a composite index that scores countries on the strength of their youth labor markets. This website allows users to recreate the scores and rankings used in the paper by Kudrzycki, Lefoll, and Günther (2021) and adjust the parameters used to generate them. The parameters are: gender (male, female, or total), the cutoff year for the oldest data to be entered into the index, and whether missing data should be imputed (for countries which have a sufficient number of 'true' observations. This tool was designed by NADEL of the ETH Zürich."))
                        )
               )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # generate index according to user specification
  country_list <- reactive(
    if (input$subset == "All") {
      countryLists()[[9]][[1]]
    }
    else {
      countryLists()[[3]][[1]]
    }
  )
  
  reactiveIndex <- reactive(rank_generator(bygender = input$gender, years = input$years, impute = input$impute) %>% 
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
                                "Underemployment score" = underemp,
                                "Informal work score" = informal,
                                "Elementary occupation score" = elementary,
                                "Secondary schooling rate" = nosecondary,
                                "Literacy rate" = literacy,
                                "Harmonized tests score" = test_scores
                              )
                            )
                            
  tot_ylili <- reactive(rank_generator(bygender = "Total", years = input$years, impute = input$impute) %>% 
                          rowwise() %>%
                          rename("Country" = "country") %>% 
                          mutate(transdim = ifelse(input$dim_agg == "Arithmetic", transition_mean, transition_geom),
                                 wcdim = ifelse(input$dim_agg == "Arithmetic", working_conditions_mean, working_conditions_geom),
                                 educdim = ifelse(input$dim_agg == "Arithmetic", education_mean, education_geom),
                                 arith_score = mean(c(transdim,wcdim,educdim)),
                                 geom_score = gm_mean(c(transdim,wcdim,educdim)), na.rm = FALSE) %>%  # don't generate if missing dims
                          mutate(score = ifelse(input$score_agg == "Arithmetic", arith_score, geom_score)))
  
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
    
    pal <- reactive(colorNumeric(c("#FFFFFFFF", viridis(256)), domain = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)), na.color = "white"))
    
    countries1 <- reactive(merge(countries,
                                 tot_ylili() %>% 
                                   filter(Country %in% country_list()),
                                 by.x = "NAME",
                                 by.y = "Country",
                                 sort = FALSE))
    
    countries2 <- reactive(merge(countries,
                                 reactiveIndex() %>% 
                                   filter(Country %in% country_list()),
                                 by.x = "NAME",
                                 by.y = "Country",
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
        attributionControl=FALSE)) %>% 
        addPolygons(data = countries2(),
                    fillColor = ~pal()(countries2()[[indicator]]),
                    layerId = ~NAME, weight = 1, smoothFactor = 0.5,
                    opacity = .8, fillOpacity = .8,  color = "#BDBDC3",
                    highlightOptions = highlightOptions(color = "black", weight = 2, opacity = .8),
                    popup = country_popup) %>% 
        setView(10, 20, zoom = 2) %>% 
        setMaxBounds(lng1 = -200,
                     lat1 = -90,
                     lng2 = 200,
                     lat2 = 90) %>% 
        addLegend(position = "bottomright",
                  pal = pal(),
                  value = c(min(scores()[2], na.rm = T), max(scores()[2], na.rm = T)))
    })
  })
  
  toListen <- reactive({
    list(input$table, input$country1, input$country2)
  })
  
  observeEvent(toListen(), {
    
    # generate table with dynamic color-coding
    scores <- DT::renderDataTable({
      rank <- reactiveIndex() %>% 
        mutate_if(is.numeric, round, 3) %>% 
        filter(Country %in% country_list()) %>% 
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
    
    ranks <- DT::renderDataTable({
      rank <- reactiveIndex() %>% 
        filter(Country %in% country_list()) %>% 
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
          "Underemployment score" = rank(-`Underemployment score`, na.last = "keep"),
          "Informal work score" = rank(-`Informal work score`, na.last = "keep"),
          "Elementary occupation score" = rank(-`Elementary occupation score`, na.last = "keep"),
          "Secondary schooling rate" = rank(-`Secondary schooling rate`, na.last = "keep"),
          "Literacy rate" = rank(-`Literacy rate`, na.last = "keep"),
          "Harmonized tests score" = rank(-`Harmonized tests score`, na.last = "keep")
        ) %>%      
        mutate_if(is.numeric, round, 3) %>% 
        arrange(`YLILI score`)
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
    
    if (input$table == "Scores") {
      output$scores <- scores
    } else {
      output$scores <- ranks
    }
    
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
    g.mid<-ggplot(left(), aes(x=1,y=left()$var)) + geom_text(aes(label=left()$var, color = "NEET score"), size = rel(5)) +
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
  
  #generate data
  data_list <- reactive({
    list(
      total = rank_generator(bygender = input$gender, years = input$years, impute = input$impute) %>% 
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
      male = rank_generator(bygender = "Male", years = input$years, impute = input$impute) %>% 
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
      female = rank_generator(bygender = "Female", years = input$years, impute = input$impute) %>% 
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
