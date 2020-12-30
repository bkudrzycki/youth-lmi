# Package names
packages <- c("tidyverse", "RColorBrewer", "GISTools", "fmsb")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# function for adding line breaks in labels where needed
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

## ---------------------------

rank <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

regions <- read_csv(here("data", "raw", "country_regions.csv")) %>% 
  dplyr::select("country" = "Country or Area",
         "Region Name",
         "Sub-region Name")

regions$`Sub-region Name` <- regions$`Sub-region Name` %>% #fix country names to match ILOSTAT for joining
  recode("Latin America and the Caribbean" = "Latin America")

regions$country <- regions$country %>% #fix country names to match ILOSTAT for joining
  recode("Democratic Republic of the Congo" = "Congo, Democratic Republic of the",
         "Republic of Moldova" = "Moldova, Republic of",
         "United Republic of Tanzania" = "Tanzania, United Republic of",
         "State of Palestine" = "Occupied Palestinian Territory",
         "Côte d’Ivoire" = "Côte d'Ivoire",
         "Bolivia (Plurinational State of)" = "Bolivia",
         "Cabo Verde" = "Cape Verde",
         "Micronesia (Federated States of)" = "Micronesia, Federated States of",
         "Democratic People's Republic of Korea" = "Korea, Democratic People's Republic of"
  )

rank <- left_join(rank, regions, by = c("country")) %>% 
  filter(!is.na(index_mean))

## REGIONAL ANALYSIS

# color palette
colors_border <- add.alpha(brewer.pal(9, "Set1"), .9)
colors_in <- add.alpha(brewer.pal(9, "Set1"), .4)

# 4D

x <- rank %>% 
  dplyr::select(country, `Region Name`, "Overall YLILI" = index_mean, "Transition" = transition_mean, "Working\nConditions Ratio" = working_conditions_mean, "Education" = education_mean) %>% 
  group_by(`Region Name`) %>% 
  summarise_at(vars(-country), funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

rownames(x) <- x[,1]
x <- x[,-1]

x <- rbind(rep(90,5) , rep(50,5) , x)

radarchart( x , axistype=6, 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=1, legend = rownames(x[-c(1,2),]), bty = "n", pch=20 , col=colors_in)

#11D

x <- rank %>% 
  dplyr::select(country, `Region Name`, "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(`Region Name`) %>% 
  summarise_at(vars(-country), funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

rownames(x) <- x[,1]
x <- x[,-1]

x <- rbind(rep(100,5) , rep(0,5) , x)

radarchart( x , axistype=6, 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",
            #custom labels
            vlcex=0.8 
)


legend(x=1.2, y=1.2, legend = rownames(x[-c(1,2),]), bty = "n", pch=20 , col=colors_in)

## SUB-REGIONAL ANALYSIS

#4D for Non-Asia and Asia as separate sub-region

rank <- rank %>% 
  mutate(region2 = ifelse(`Region Name` == "Asia", "Asia", `Sub-region Name`))

x <- rank %>% 
  dplyr::select(country, region2, "Overall YLILI" = index_mean, "Transition  " = transition_mean, "Working\nConditions Ratio" = working_conditions_mean, "Education" = education_mean) %>% 
  group_by(region2) %>% 
  summarise_at(vars(-country), funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

rownames(x) <- x[,1]
x <- x[,-1]

x <- rbind(rep(88,5) , rep(48,5) , x)

radarchart( x , axistype=6, 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",
            #custom labels
            vlcex=0.75)

legend(x=1.2, y=1.2, legend = rownames(x[-c(1,2),]), bty = "n", pch=20 , col=colors_in)


#11D for Asia

x <- rank %>% 
  filter(`Region Name` == "Asia") %>% 
  dplyr::select(country, `Sub-region Name`, "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(`Sub-region Name`) %>% 
  summarise_at(vars(-country), funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

rownames(x) <- x[,1]
x <- x[,-1]

x <- rbind(rep(100,5) , rep(0,5) , x)

radarchart( x , axistype=6, 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",
            #custom labels
            vlcex=0.8 
)


legend(x=1.2, y=1.2, legend = rownames(x[-c(1,2),]), bty = "n", pch=20 , col=colors_in)

#11D for Non-Asia + Asia a one sub-region

rank <- rank %>% 
  mutate(region2 = ifelse(`Region Name` == "Asia", "Asia", `Sub-region Name`))

x <- rank %>% 
  dplyr::select(country, region2, "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(region2) %>% 
  summarise_at(vars(-country), funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

rownames(x) <- x[,1]
x <- x[,-1]

x <- rbind(rep(100,5) , rep(0,5) , x)

radarchart( x , axistype=6, 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",
            #custom labels
            vlcex=0.8 ,
            vlabels = addline_format(c("Overall YLILI", "NEET", "Working Conditions Ratio", "Mismatch", "Working Poverty", "Under- employment",  "Informality",  "Elementary", "No Secondary", "Literacy", "Test Scores"))
)

legend(x=1.2, y=1.2, legend = rownames(x[-c(1,2),]), bty = "n", pch=20 , col=colors_in)
