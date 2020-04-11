library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(rvest)
library(tidyverse)
library(haven)
library(leaflet)
library(ggiraph)
library(maps)
library(sf)
library(RColorBrewer)
library(plotly)
#library(ComplexHeatmap)
#library(dendextend)
#library(circlize)
#library(rgdal)

#function to extract the questions from the survey
survey_questions <- function(data) {
  
  qsts <- lapply(data, function (x) attr(x,"label")) %>%
    do.call(rbind.data.frame, .) %>% 
    mutate(Question = names(data))
  names(qsts)[1] <- "Survey_Questions"
  
  return(qsts %>% select(Question, Survey_Questions))
}

#function to extract the attributes of non-numeric variables
survey_factors <- function(data) {
  
  lapply(data, function (x) attr(x,"labels"))
}

#function to convert the haven_labelled variables to factors
convert_label <- function(data) {
  
  dd_list <- lapply(data, function(x) {
    
    if(class(x)[1] == "haven_labelled") {
      
      #extract the levels and attributes from the variable
      levelss <- attr(x, "labels")
      labelss <- names(attr(x, "labels"))
      
      #form the factor
      y <- forcats::fct_explicit_na(factor(x, levels = levelss, 
                                           labels =  labelss), na_level = "Missing")
      
    } else {
      
      y <- x
      
    }
    
    return(y)
    
  })
  
  #convert the resulting list to a dataframe
  data.frame(dd_list, stringsAsFactors = FALSE)
  
}

#function to convert Vlaams Brabant to Vlaams-Brabant
vbrant <- function(x) {
  
  lx <- levels(x)
  lx[lx == "Vlaams Brabant"] <- "Vlaams-Brabant"
  return(lx)
  
}

### function to return a merged dataset of respondents and Belgium map
respondents_group <- function(belgium_mag_group, survey_data, 
                              by.x = "Prov_nl", by.y = "Province") {
  
  merged_data <- merge(belgium_mag_group, survey_data, 
                       by.x = by.x, by.y = by.y) #%>%
  #mutate(Long = c(4.40346, 3.95229, 5.33781, 5.56749, 5.81667, 4.86746, 
  #                3.71667, 4.70093, 4.60138, 3.22424, NA),
  #       Lat = c(	51.21989, 50.45413, 50.93106, 50.63373, 49.68333, 50.4669, 
  #                51.05, 50.87959, 50.71717, 51.20892, NA))
  return(merged_data)
  
}

# function to recode diploma
create_diploma <- function(x) {
  
  case_when(
    
    x == 1 ~ "Lager",
    x %in% 2:4 ~ "Middelbaar",
    x %in% 5:6 ~ "Hoger", 
    x == 7 ~ "PhD"
    
  )
  
}

#### Exploratory Analysis functions

## cross-tab between two categorical variables
crosstab_data <- function(data, x, y){
  x = get(x, data)
  y = get(y, data)
  data2 = data.frame(x=x,y=y)
  dat = data2 %>% group_by(x, y) %>% count() %>% as.data.frame()
  return(dat)
}

#side-by-side barcharts
sidebar_chart <- function(data, x, y, fill, ylab = "ylab", legendt = "legendt", survey_question) {
  data %>% ggplot(aes_string(x = x, y = y, fill = fill)) +
    geom_bar(position="dodge", stat = "identity") +
    theme_bw() +
    scale_y_continuous(labels = function(val) {trans = val / 1000; paste0(trans, "K")}) +
    labs(y = ylab, x = survey_question) +
    theme(axis.title.x = element_text(face = 'bold')) + 
    scale_fill_brewer(name = legendt, palette = "Set1")
  
}

## function to make boxplots
bar_box <- function(data, x, y, ylab="ylab") {
  
  data %>% ggplot(aes_string(x = x, y = y)) +
    geom_boxplot() +
    theme_minimal() +
    labs(y = ylab, x="")
}

#function to compute some summares
survey_summaries <- function(data, province, gender) { 
  
  #number of respondents
  N_respondents <- nrow(data)
  
  #number of respondents per province
  pdata_province <- as.data.frame(table(data[, province], useNA = 'ifany'))
  names(pdata_province) <- c("Province", "Number")
  
  #number of respondents per region
  Region <- case_when(
    
    data[, province] %in% c("Antwerpen", "Oost-Vlaanderen", "Vlaams-Brabant",
                            "Limburg", "West-Vlaanderen") ~ "Flanders",
    data[, province] %in% c("Henegouwen", "Luik", "Luxemburg",
                            "Namen", "Waals-Brabant") ~ "Wallonia",
    data[, province] == "Arrondissemment Brussel" ~ "Brussel",
    data[, province] == "Missing" ~ "Missing"
    
  )
  
  #number of respondents per region
  pdata_region <- as.data.frame(table(Region, useNA = 'ifany'))
  names(pdata_region) <- c("Region", "Number")
  
  #gender distribution
  pdata_gender <- as.data.frame(table(data[,gender], useNA = 'ifany'))
  names(pdata_gender) <- c("Gender", "Number")
  
  return(list(newdata = cbind(data, Region = Region), N_respondents = N_respondents, 
              pdata_province = pdata_province,
              pdata_region = pdata_region,
              pdata_gender = pdata_gender)
  )
}

#function for bar chart
bar_chart <- function(data, x, y) {
  
  data %>% ggplot(aes_string(x = x, y = y)) + 
    geom_bar(fill = "peru", position="stack", stat = "identity") + 
    theme_minimal() +
    scale_y_continuous(labels = function(val) {trans = val / 1000; paste0(trans, "K")}) + 
    labs(y = "Number of Respondents") +
    geom_text(aes_string(label = paste0(prettyNum(y, big.mark = ",") )), 
              color = "black", vjust =- 0.3) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  
}


###### Cluster Analysis function to compute the data needed for the clustering
cluster_matrix <- function(data, var_interest, other_var) {
  
  x <- get(var_interest, data)
  y <- get(other_var, data)
  mat <- as.matrix(prop.table(table(x, y, useNA = 'ifany'), margin = 2))
  return(mat)
}



##### reading in the data

data_dir <- list.dirs("Data/")

load(file = 'Data/surveys/preloaded.RData')