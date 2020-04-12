require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinycssloaders)
require(shinyBS)
require(ggplot2)
require(sf)
require(tidyverse)
require(leaflet)
require(RColorBrewer)
require(classInt)

source('scripts/dictionary.R')
set_language('en')

ui_survey_response <- function() {
  fluidRow(
    column(width = 6,uiOutput('survey_response_map_box')),
    column(width = 6,uiOutput('survey_response_bar_box'))
  )
}

ui_symptoms <- function() {
  fluidRow(
    column(width = 6,uiOutput('symptom_map_box')),
    column(width = 6,uiOutput('mitigation_bar_box'))
  )
}

ui_psychology <- function() {
  fluidRow(
    column(width = 6,uiOutput('psychology_map_box')),
    column(width = 6,uiOutput('psychology_bar_box'))
  )
}

ui_header_menu <- function() {
  div(
    style = "position: absolute; right: 1.7em; top: -0.9em; z-index:1050; display:inline-block;",
    fluidRow(width = 160,
            column(8, selectInput('language', '',
                 c('Nederlands' = 'nl', 'English' = 'en'),
                width = 150)),
             column(2, 
                    div(style = 'position:relative; top: 1.4em;',
                        actionButton('about', '',
                          icon = icon("question", class = "opt"))))
    )
    )
}

ui_button_bar <- function() {
  fluidRow(
    width = NULL,
    column(
      width = 12,
        bsButton("patients", 
                 label = "\u00A0Participants", 
                 icon = icon("user"), 
                 style = "maintab"),
        bsButton("epidemiology", 
                 label = "\u00A0Epidemiology", 
                 icon = icon("viruses"), 
                 style = "maintab"),
        bsButton("mitigation", 
                 label = "\u00A0Mitigation", 
                 icon = icon("people-arrows"), 
                 style = "maintab"),
        bsButton("mental health", 
                 label = "\u00A0Mental health", 
                 icon = icon("brain"), 
                 style = "maintab")
    )
  )
}

ui <- dashboardPage(dashboardHeader(title = "Corona Survey"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      tags$head(
                        tags$link(
                          rel = "stylesheet", 
                          type = "text/css", 
                          href = "fontawesome/css/all.css"),
                        tags$link(
                          rel = "stylesheet", 
                          type = "text/css", 
                          href = "corona_survey_style.css")
                      ),
                      ui_header_menu(),
                      ui_button_bar(),
                      ui_survey_response(),
                      ui_symptoms(),
                      ui_psychology()))




server <- function(session, input, output) {
  
  about <- function() {
    showModal(modalDialog(
      easyClose = TRUE,
      title = 'Corona Survey',
      tabBox(
        title = "",
        width = NULL,
        id = "info_corona_survey",
        tabPanel("Nederlands", 
                 includeHTML('information/info_nl.html')
        ),
        tabPanel("fran\u00E7ais", 
                 includeHTML('information/info_en.html')
        ),
        tabPanel("English", 
                 includeHTML('information/info_fr.html')
        )
      ),
      footer = NULL
    ))
  }
  
  
  
  ggblue <- "#56B4E9";#gg_co
  ggorange <- gg_color_hue(2)[1];
  ggred <- scales::hue_pal()(1)
  ggpurple <- "#796BD6"
  gggreen <- "#A3D596"
  
  map_predict <- read_sf('data/shapefile_predict.shp', quiet = TRUE)
  
  map_province <- map_predict %>% 
    group_by(prov_nl, prov_fr) %>%
    summarise(province_area = sum(F_AREA))
  
  choose_province <- c('Belgium', map_province$prov_nl) 
  names(choose_province) <- translate(c('Belgium', map_province$prov_nl))
  
  load('data/aggregated_data.RData')
  
  render_everything <- function() {
    output$survey_response_map_box <- renderUI({
      div(style = 'position:relative;',
          tags$head(
            tags$style(HTML(".leaflet-container { background: #fff; }"))
          ),
          tabBox(
            title = "",
            height = 400,
            width = NULL,
            id = "map_survey_response",
            tabPanel(translate("April 7"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_4', height='300')),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_map_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     )),
            tabPanel(translate("March 31"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_3', height='300'))),
            tabPanel(translate("March 24"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_2', height='300'))),
            tabPanel(translate("March 17"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_1', height='300')))
          ))
    })
  }
  
  render_everything();
  
  observeEvent(input$language, {
    prev_language <- active_language
    set_language(input$language)
    if(prev_language != active_language) {
      render_everything()
    }
  })
  
  output$symptom_map_box <- renderUI({
    div(style = 'position:relative;',
        tags$head(
          tags$style(HTML(".leaflet-container { background: #fff; }"))
        ),
        tabBox(
          title = "",
          height = 400,
          width = NULL,
          id = "map_symptom",
          tabPanel("March 31", 
                   div(style = 'font-size:24px; color:#F8766D;', 'Probability of having symptoms'),
                   withSpinner(leafletOutput('symptom_map_3', height='300')),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_map_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   div(
                     style = "position: absolute; right: 0.5em; top: 2.5em;",
                     selectInput("select_province_3", "",
                                 choose_province,
                                 width = 140)
                   )),
          tabPanel("March 24", 
                   div(style = 'font-size:24px; color:#F8766D;', 'Probability of having symptoms'),
                   withSpinner(leafletOutput('symptom_map_2', height='300')),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_map_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   div(
                     style = "position: absolute; right: 0.5em; top: 2.5em;",
                     selectInput("select_province_2", "",
                                 choose_province,
                                 width = 140)
                   ))
        ))
  })
  
  
  
  output$survey_response_bar_box <- renderUI({
    div(style = 'position:relative;', 
        tabBox(
          title = "",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", 
          height = 400,
          width = NULL,
          tabPanel("response", 
                   icon = icon("chart-bar", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('survey_response_count', height = 300)),
          tabPanel("gender", 
                   icon = icon("venus-mars", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('survey_response_gender', height = 300)),
          tabPanel("age", 
                   icon = icon("birthday-cake", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('survey_response_age', height = 300))
        ))
  })
  
  output$mitigation_bar_box <- renderUI({
    div(style = 'position:relative;', 
        tabBox(
          title = "",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", 
          height = 400,
          width = NULL,
          tabPanel("work", 
                   icon = icon("building", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_homework', height = 300)),
          tabPanel("behaviour work", 
                   icon = icon("briefcase", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_work', height = 300)),
          tabPanel("behaviour public", 
                   icon = icon("tree", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_public', height = 300)),
          tabPanel("behaviour household", 
                   icon = icon("home", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_household', height = 300)),
          tabPanel("conversation", 
                   icon = icon("comment-dots", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_conversation', height = 300)),
          tabPanel("fysical contact", 
                   icon = icon("handshake", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('mitigation_fysical_contact', height = 300))
          
        ))
  })
  
  output$psychology_bar_box <- renderUI({
    div(style = 'position:relative;', 
        tabBox(
          title = "",
          id = "psychology_bar", 
          height = 400,
          width = NULL,
          tabPanel("sleep", 
                   icon = icon("bed", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('psychology_sleep', height = 300)),
          tabPanel("concentrate", 
                   icon = icon("lightbulb", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('psychology_concentrate', height = 300)),
          tabPanel("optimistic", 
                   icon = icon("smile-beam", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('psychology_optimistic', height = 300)),
          tabPanel("tension", 
                   icon = icon("bolt", class = "opt"),
                   div(
                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                     actionButton('question_bar_responses', '',
                                  icon = icon("question", class = "opt")
                     )
                   ),
                   plotOutput('psychology_tension', height = 300))
        ))
  })
  
  map_response_freq <- function(survey_number) {
    data <- aggregated %>%
      filter(survey == survey_number,
             variable == 'response_freq') %>%
      select(label, value)
    
    data <- map_province %>%
      merge(data,
            by.x = 'prov_nl',
            by.y = 'label')
    
    color_pal <- colorBin("YlOrRd", 
                          bins = 0:8/17)
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addPolygons(
        data = data,
        weight = 2,
        smoothFactor = 0.5, 
        opacity = 1.0,
        fillOpacity = 0.7,
        color = 'gray',
        fillColor = ~color_pal(data$value),
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            bringToFront = TRUE),
        label = paste0(round(data$value*10000)/100, '%'))
  }
  
  observeEvent(input$map_survey_response, {
    if(input$map_survey_response == translate('April 7')) {
      output$survey_response_4 <- renderLeaflet({ map_response_freq(4) })
    } else if(input$map_survey_response == translate('March 31')) {
      output$survey_response_3 <- renderLeaflet({ map_response_freq(3) })
    } else if(input$map_survey_response == translate('March 24')) {
      output$survey_response_2 <- renderLeaflet({ map_response_freq(2) })
    } else if(input$map_survey_response == translate('March 17')) {
      output$survey_response_1 <- renderLeaflet({ map_response_freq(1) })
    }
    
  })
  
  output$survey_response_count <- renderPlot({
    
    data <- aggregated %>% 
      filter(variable == 'response_count')
    
    ggplot(data) +
      theme_bw() +
      geom_bar(aes(x = factor(c('March 17', 'March 24', 'March 31', 'April 4'),
                              c('March 17', 'March 24', 'March 31', 'April 4')), 
                   y = value / 1000), fill = ggblue, stat = 'identity') +
      ylab('responses') + xlab('') +
      ggtitle('Number of responses per survey (x1000)')
    
  })
  
  output$survey_response_gender <- renderPlot({
    
    data <- aggregated %>% 
      filter(variable == 'response_count')
    
    ggplot(aggregated %>% filter(variable == 'gender')) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = c('March 17', 'March 24', 'March 31', 'April 7')) +
      ggtitle('Distribution of participants by gender')
    
  })
  
  output$survey_response_age <- renderPlot({
    
    data <- aggregated %>% 
      filter(variable == 'response_count')
    
    ggplot(aggregated %>% filter(variable == 'age')) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = c('March 17', 'March 24', 'March 31', 'April 7')) +
      ggtitle('Distribution of participants by age group')
    
  })
  
  var <- c(map_predict$symptom2, map_predict$symptom3)
  
  breaks_map_symptom <- c(round(min(var),4)-0.0001,
                          round(as.numeric(quantile(var, probs = seq(0, 1, 0.25))),4)[2:4],
                          round(max(var),4)+0.0001)
  
  active_map_symptom <- rep('', 4)
  
  map_symptom <- function(survey_number, column_name, province = 'Belgium') {
    
    active_map_symptom[survey_number] <<- province;
    
    map <- map_predict
    
    if(province != 'Belgium') {
      map <- map %>% 
        filter(prov_nl == province)
    }
    
    
    values <- map[[column_name]]
    
    nclr <- 5      
    plotclr <- brewer.pal(nclr,"YlOrRd")
    
    class <- classIntervals(values, nclr, style = "fixed", fixedBreaks = breaks_map_symptom)
    color_bins <- class$brks
    
    color_pal <- colorBin( 
      plotclr, domain = map$gemeente, 
      bins = color_bins
    )
    options = leafletOptions(zoomControl = FALSE)
    
    
    
    leaflet(data = map, options = leafletOptions(zoomControl = FALSE)) %>% 
      addLegend("bottomright", pal = color_pal, 
                values = ~ column_name, 
                title = "probability") %>%
      addPolygons(
        data = map,
        color = "gray",
        weight = 0.5,
        smoothFactor = 0.5, 
        opacity = 1.0,
        fillOpacity = 0.5,
        fillColor = ~color_pal(map[[column_name]]),
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            bringToFront = TRUE),
        label = paste0(map$gemeente,'\n',round(map[[column_name]]*10000)/100, '%'))
  }
  
  #output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3')})
  #output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2')})
  
  observeEvent(input$map_symptom,{
    
    if(input$map_symptom == 'March 31') {
      if(active_map_symptom[3] != input$select_province_3) {
        output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3)})
      }
    } else if(input$map_symptom == 'March 24') {
      if(active_map_symptom[2] != input$select_province_2) {
        output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2)})
      }
    }
  })
  
  observeEvent(input$select_province_3, {
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_3)
    
    if(active_map_symptom[3] != input$select_province_3) {
      output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3)})
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$select_province_2, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_2)
    
    if(active_map_symptom[2] != input$select_province_2) {
      output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2)})
    }
  }, ignoreInit = TRUE)
  
  
  output$mitigation_homework <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == "work condition")) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = "dodge2") +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Current working conditions')
    
  })
  
  output$mitigation_work <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_work',
                                 survey != 1)) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Change in behaviour at work')
    
  })
  
  output$mitigation_public <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_public',
                                 survey != 1)) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Change in behaviour at public places')
    
  })
  
  output$mitigation_household <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_household',
                                 survey != 1)) +
      theme_bw() +
      geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Change in behaviour at home')
    
  })
  
  output$mitigation_conversation<- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'number_people_talk',
                                 survey != 1)) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Number of people spoken today (not virtual)')
    
  })
  
  output$mitigation_fysical_contact<- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'last_fysical_contact')) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('last fysical contact (handshake or kiss)')
    
  })
  
  output$psychology_sleep <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'sleep')) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Sleep quality last week')
    
  })
  
  output$psychology_concentrate <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'concentrate')) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Concentration last week')
    
  })
  
  output$psychology_optimistic <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'optimistic')) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Optimism last week')
    
  })
  
  output$psychology_tension <- renderPlot({
    
    ggplot(aggregated %>% filter(variable == 'tension')) +
      theme_bw() +
      geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
      ylab('percentage') + xlab('') +
      scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
      ggtitle('Tension last week')
    
  })
  
  
}

shinyApp(ui = ui, server = server)

