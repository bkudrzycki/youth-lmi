# Package names
packages <- c("tidyverse", "here", "RColorBrewer", "GISTools", "fmsb", "viridis")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load lamadex package 
devtools::load_all(here("lamadex"))
source(here("lamadex", "R", "source", "countryList.R"))
source(here("lamadex", "R", "source", "data_loader.R"))

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
         "Sub-region Name",
         "Intermediate Region Name")

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
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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

#11D for Africa

x <- rank %>% 
  filter(`Sub-region Name` == "Sub-Saharan Africa") %>% 
  dplyr::select(country, `Intermediate Region Name` , "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(`Intermediate Region Name`) %>% 
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
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


## WHEEL GRAPH

df <- rank %>% 
  dplyr::select(c(country, region2, "Transition" = transition_mean, "Working Conditions" = working_conditions_mean, "Education" = education_mean, index_mean)) %>% 
  mutate(Transition = Transition/3,
         "Working Conditions" = `Working Conditions`/3,
         Education = Education/3) %>% 
  pivot_longer(cols = c("Transition", "Working Conditions", "Education"), names_to = "observation", values_to = "value")

df$region2 <- df$region2 %>% 
  recode("Sub-Saharan Africa" = "SSA",
         "Eastern Europe" = "EE",
         "Latin America" = "LAC",
         "Northern Africa" = "NA") %>% 
  factor()

df$country <- df$country %>% 
  recode("Occupied Palestinian Territory" = "Palestinian Territories",
         "Moldova, Republic of" = "Moldova",
         "Lao People's Democratic Republic" = "Laos",
         "Congo, Democratic Republic of the" = "DR Congo",
         "Tanzania, United Republic of" = "Tanzania")

empty_bar <- 2
nObsType <- nlevels(as.factor(df$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(df$region2)*nObsType, ncol(df)) )
colnames(to_add) <- colnames(df)
to_add$region2 <- rep(levels(df$region2), each=empty_bar*nObsType )
df <- rbind(df, to_add)
df <- df %>% arrange(region2, index_mean)
df$id <- rep( seq(1, nrow(df)/nObsType) , each=nObsType)
df <- df %>% 
  dplyr::select(-index_mean)

label_data <- df %>% group_by(id, country) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- df %>% 
  group_by(region2) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))            

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

ggplot(df) +  
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(df$id),5), y = c(0, 25, 50, 75, 100), label = c("0", "25", "50", "75", "100") , color="grey", size=4 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-50,max(label_data$tot+10, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=country, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend = -1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -5, label=region2), hjust=c(.8,1,.8,.5,0), vjust=c(1.5,0,-.1,-.2,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

