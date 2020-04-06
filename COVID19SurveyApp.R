### load libraries here
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
      labelss <- sort(names(attr(x, "labels")))
      
      #form the factor
      y <- factor(x, levels = levelss, 
                  labels =  labelss)
      
    } else {
      
      y <- x
      
    }
    
    return(y)
    
  })
  
  #convert the resulting list to a dataframe
  data.frame(dd_list)
  
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

##### reading in the data
#first_survey <- read_sav("Data/firstSurvey/2020_UA_Corona_study_wave 17-03-2020_first clean.sav")
#second_survey <- read_sav("Data/secondSurvey/2020_UA_Corona_golf2_data_no clean.sav")

### clean the factor columns
#first_survey2 <- convert_label(first_survey)
#second_survey2 <- convert_label(second_survey)

#extract the survey questions

# change Vlaams Brabant to Vlaams-Brabant
levels(first_survey2$province) <- vbrant(first_survey2$province)
levels(second_survey2$province) <- vbrant(second_survey2$province)

#survey data list
survey_list_orig <- list(first_survey, second_survey)
survey_list <- list(first_survey2, second_survey2)


#function to compute some summares
survey_summaries <- function(data, province, gender) { 
  
  #number of respondents
  N_respondents <- nrow(data)
  
  #number of respondents per province
  pdata_province <- as.data.frame(table(data[, province], useNA = 'always'))
  names(pdata_province) <- c("Province", "Number")
    
  #number of respondents per region
    Region <- case_when(
      
      data[, province] %in% c("Antwerpen", "Oost-Vlaanderen", "Vlaams-Brabant",
                              "Limburg", "West-Vlaanderen") ~ "Flanders",
            data[, province] %in% c("Henegouwen", "Luik", "Luxemburg",
                              "Namen", "Waals-Brabant") ~ "Wallonia",
      data[, province] == "Arrondissemment Brussel" ~ "Brussel",
      data[, province] == "<NA>" ~ "NA"
      
    )
    
  #number of respondents per region
    pdata_region <- as.data.frame(table(Region, useNA = 'always'))
    names(pdata_region) <- c("Region", "Number")
    
  #gender distribution
  pdata_gender <- as.data.frame(table(data[,gender], useNA = 'always'))
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

#obtain number of respondents in total, per region and per province
N_respondents <- numeric(length = length(survey_list))
region_plots <- surv_sums  <- vector("list", length = length(survey_list))

for(i in 1:2) {
  
  if(i == 1) {
    
    surv_sums[[i]] <- survey_summaries(survey_list[[i]], "province", gender = "Q3")
    #extract the number of respondents
    N_respondents[i] <- surv_sums[[i]]$N_respondents
    #make the barchart
    region_plots[[i]] <- bar_chart(surv_sums[[i]]$pdata_region, x = "Region", y = "Number")
    
  } else {
    
    surv_sums[[i]] <- survey_summaries(survey_list[[i]], "province", gender = "Q4")
    #extract the number of respondents
    N_respondents[i] <- surv_sums[[i]]$N_respondents
    #make the barchart
    region_plots[[i]] <- bar_chart(surv_sums[[i]]$pdata_region, x = "Region", y = "Number")
  }
  
}

### Belgium map
#belgium2 <- read_sf("shapeFiles/BE_SB_TF_PD_STATDIS_2014.shp", quiet = TRUE)

#compute area per province
#belgium_province  <- belgium2 %>% group_by(Prov_nl) %>% 
#  summarise(Province_area = sum(Shape_Area))

#convert it
#belgium_province_conv <- st_transform(belgium_province, '+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')

#obtain respondents per province and mere with data
province_data <- lapply(surv_sums, function(x) {
  
  
  respondents_group(belgium_province_conv, 
                    x$pdata_province, 
                    by.x = "Prov_nl", by.y = "Province")
  
})  

#make the leaflet plots

maps_plot <- function(plotdata) {
  
  ss <- seq(0, 500, 50)
  mm <- max(plotdata$Number)/1000
  ss_mm <- ss[which(mm <= ss)[1]]
  color_bins <- seq(0, ss_mm, by = 50)
  color_pal <- colorBin("YlOrRd", domain = plotdata$Prov_nl, 
                        bins = color_bins)
  
  leaflet(data = plotdata) %>% 
    addLegend("bottomright", pal = color_pal, values = ~province_data$Number/1000,
              title = "<small>Respondents per 1,000</small>")  %>% 
    
    addPolygons(
      data = plotdata,
      color = "#444444",
      weight = 2,
      #stroke = FALSE,
      smoothFactor = 0.5, 
      opacity = 1.0,
      fillOpacity = 0.5,
      fillColor = ~color_pal(plotdata$Number / 1000),
      highlightOptions = highlightOptions(color = "white", weight = 5,
                                          bringToFront = TRUE),
      label = sprintf("<strong>%s (%g respondents)</strong>", 
                      plotdata$Prov_nl, 
                      plotdata$Number) %>% lapply(htmltools::HTML)
    ) 
  
}

#leaflet_plots <- lapply(province_data, maps_plot)

#### Exploratory Analysis plots

## cross-tab between two categorical variables
crosstab_data <- function(data, x, y){
  x = get(x, data)
  y = get(y, data)
  data2 = data.frame(x=x,y=y)
  dat = data2 %>% group_by(x, y) %>% count() %>% as.data.frame()
  return(dat)
}

#dat = crosstab_data(first_survey2, "Q17", "age_class")

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
#sidebar_chart(data=dat, x = "x", y = "n", fill = "y" , ylab = "Number of Respondents", 
#              legendt = "Age group", 
#              survey_question = test_qsts$Survey_Questions[test_qsts$Question == "Q17"])


bar_box <- function(data, x, y, ylab="ylab") {
  
  data %>% ggplot(aes_string(x = x, y = y)) +
    geom_boxplot() +
    theme_minimal() +
    labs(y = ylab, x="")
}
#bar_box(data=firstsurvey, x = "Q17", y = "Q2", ylab = "Age")

#province_data %>% ggplot() + 
#  geom_sf(aes(fill = province_data$Number / 1000)) +
#   scale_fill_gradientn(colours = brewer.pal(4, "YlOrRd")) + 
#  geom_point(aes(x = Long, y = Lat, size = Number^(1/5)))

################ cluster analysis

data_cluster <- first_survey2 %>% 
  filter(age_class == "13-17 jaar") %>% 
  select(Q17, Q20, Q22_1, Q22_2, Q22_3) %>%
  mutate(
    Q17_num = case_when(
      Q17 == "Do not know" ~ 0, 
      Q17 == "No" ~ 1,
      Q17 == "Yes" ~ 2
      ), 
    Q20_num = case_when(
      Q20 == "Do not know" ~ 0, 
      Q20 == "No" ~ 1,
      Q20 == "Yes" ~ 2
    )
      
  )
  

#gower.dist <- daisy(data_cluster[, 3:4], metric = 'gower')
#c1 <- hclust(gower.dist, method = "complete")



#### The App UI
ui <- bootstrapPage(
          navbarPage(
  
          # App title ----
          title = "Corona Survey",
          theme = shinytheme("flatly"), 
          collapsible = TRUE,
          id = "navigation",
  
          #first panel showing distribution of survey respondents across belgium
          tabPanel("Survey Respondents", 
            tags$head(
                     tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
                   ),
            leafletOutput("mymap", width = "100%", height = 800),
           
            absolutePanel(id = "controls", class = "panel panel-default",
                         top = 80, left = 40, width = 300, fixed=TRUE,
                         draggable = TRUE, height = "auto",
                         # Input Survey Round
                         selectInput("surveyId", 
                                     label = "Survey Round", 
                                     choices = c("First" = 1, 
                                                 "Second" = 2, 
                                                 "Third" = 3), 
                                     selected = "First",
                                     multiple = FALSE),
                         h3(textOutput("total_reactive_respondents"), align = "right"),
                         plotOutput("region_reactive_plot", height="360px", width="100%"),
                         tags$i(h6("Survey carried out Tuesday to Friday every week. For more 
                                   , see:", tags$a(href = "https://surveys-covid19.sciensano.be/index.php/566999?lang=en", 
                                                   "https://surveys-covid19.sciensano.be/index.php/566999?lang=en"))),
                         h5(textOutput("date"), align = "right")
                      )
           
           ), 
  
    #Second panel for exploratory data analysis
    tabPanel("Exploratory Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("surveyId_EDA", 
                             label = "Survey Round", 
                             choices = c("First" = 1, 
                                         "Second" = 2, 
                                         "Third" = 3), 
                             selected = "First",
                             multiple = FALSE),
                 
                 pickerInput("question_select", "Survey Questions",   
                             choices = textOutput("questions"),
                             multiple = TRUE),
                 textInput("legendTitle", "Legend Title:")
                 ), 
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plots",
                              plotlyOutput("EDA_plot", width = "100%", height = "700px")),
                     tabPanel("Plot Data", 
                              verbatimTextOutput("EDA_data")
                              )
                   )
                 )
                 
                
             )
          ),
  
    #Third panel to check for data merging
    tabPanel("Merging Surveys"),
  
    #Third panel for exploratory data analysis
    tabPanel("Patterns and Clusters"),
  
    #fourth panel for exploratory data analysis
    tabPanel("Spatial Analysis"),
  
    tabPanel("Contact Analysis"),
  
    tabPanel("About the survey")
  

      
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ##### Survey Respondents tab
  #total number of respondents
  reactive_total_respondents <- reactive(N_respondents[as.numeric(input$surveyId)])
  
  #reactive_region_plot
  region_resps <- reactive(region_plots[[as.numeric(input$surveyId)]])
  
  #reactive plotdata for the province
  
  plotdata <- reactive({
    
    prov_data <- province_data[[as.numeric(input$surveyId)]]
    
  })
  
  # today's date
  output$date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  output$total_reactive_respondents <- renderText({
    paste0(prettyNum(reactive_total_respondents(), big.mark=","), " respondents")
  })
  
  output$date <- renderText({
    prettyNum(format(Sys.Date(),"%d/%m/%Y"))
  })
  
  output$region_reactive_plot <- renderPlot({
    
    region_resps()
    
    })
  
  output$mymap <- renderLeaflet({ 
    
    maps_plot(plotdata())
    
  })
  
  ########### EDA tab
  
  #to fill up the questions asked for each survey
  
  reactive_question <- reactive({
    survey_choice <- survey_list_orig[[as.numeric(input$surveyId_EDA)]]
    qsts <- survey_questions(survey_choice)
    qsts
  })
  observe({
    
    
    updatePickerInput(
      session, 
      "question_select", 
      label = "Survey Questions",
      choices = levels(reactive_question()$Survey_Questions),
      selected = levels(reactive_question()$Survey_Questions)[c(1, 17)]
      )
    
  })
  
  reactive_EDA_data <- reactive({
    
    #survey_choice <- survey_list_orig[[as.numeric(input$surveyId_EDA)]]
    #qsts <- survey_questions(survey_choice)
    sdata <- survey_list[[as.numeric(input$surveyId_EDA)]]
    
    if(length(input$question_select) == 2) {
      
      qsts_selected <- reactive_question()$Question[which(reactive_question()$Survey_Questions %in% input$question_select)]
      
    } else {
      
      message("Please select two and only two variables")
      
    }
    
    
    #filter variable name for the selected questions
    crosstab_data(sdata, x = qsts_selected[1], y = qsts_selected[2])
  })
  
  #plotting
  output$EDA_plot <- renderPlotly({
    
    #survey_choice <- survey_list_orig[[as.numeric(input$surveyId_EDA)]]
    #qsts <- survey_questions(survey_choice)
    if(length(input$question_select) == 2) {
      
      qsts_selected <- reactive_question()$Question[which(reactive_question()$Survey_Questions %in% input$question_select)]
      
    } else {
      
      message("Please select two and only two variables")
      
    }
    
    if(input$legendTitle != ""){
      
      leg <- input$legendTitle
      
    } else {
      
      leg <- "Legend"
      
    }
    
    ggplotly(sidebar_chart(data = reactive_EDA_data(), x = "x", 
                  y = "n", fill = "y" , 
                  ylab = "Number of Respondents", 
                  legendt = leg, 
                  survey_question = input$question_select[2]))
    
  })  
  
  
  #print out plot data
  output$EDA_data <- renderPrint({
      print(reactive_EDA_data())
    
    })
  
  
}



######## build the shiny app
shinyApp(ui = ui, server = server)
