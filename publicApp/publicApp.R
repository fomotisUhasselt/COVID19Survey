require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinycssloaders)
require(shinyBS)
require(shinyjs)
require(ggplot2)
require(sf)
require(tidyverse)
require(leaflet)
require(RColorBrewer)
require(classInt)

source('scripts/dictionary.R')
set_language('en')

ui_epidemiology <- function() {
  div(id = 'tab_epidemiology',
      fluidRow(column(width = 12, uiOutput('symptom_bar_box'))),
      fluidRow(column(width = 6, uiOutput('symptom_map_box')),
               column(width = 6, uiOutput('symptom_heatmap_box')))
  )
}

ui_participants <- function() {
  div(id = 'tab_participants',
      fluidRow(column(width = 6,uiOutput('survey_response_map_box')),
               column(width = 6,uiOutput('survey_response_bar_box'))),
      fluidRow(column(width = 6,uiOutput('mitigation_bar_box')),
               column(width = 6,uiOutput('psychology_bar_box')))
  )
}

ui_header_menu <- function() {
  div(
    style = "position: absolute; right: 1.7em; top: -0.9em; z-index:1050; display:inline-block; text-align:center;",
    fluidRow(width = '180px',
            column(5, selectInput('language', '',
                 c('Nederlands' = 'nl', 'English' = 'en'),
                 width = 160),
                 style = 'text-align:left;'),
             column(2, 
                    div(style = 'position:relative; top: 1.4em;',
                        actionButton('about', '',
                          icon = icon("question", class = "opt")))),
            column(4, actionButton('about', 
                                   'Achtergrond',
                                   style = "position: relative; top: 1.4em;"))
    )
    )
}

ui_button_bar <- function() {
  fluidRow(
    width = NULL,
    style = 'text-align:center;',
    column(
      width = 12,
        bsButton("participants", 
                 label = "\u00A0Participants", 
                 icon = icon("user"), 
                 style = "activetab"),
        bsButton("epidemiology", 
                 label = "\u00A0Epidemiology", 
                 icon = icon("viruses"), 
                 style = "maintab"),
        bsButton("mitigation", 
                 label = "\u00A0Social distancing", 
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
                      useShinyjs(),
                      ui_header_menu(),
                      ui_button_bar(),
                      ui_participants(),
                      ui_epidemiology()))




server <- function(session, input, output) {
  
  icon_observe <- function(){
    div(
      style = "position: absolute; right: 1em",
      HTML("<i style='font-size:20px;' class='fas fa-eye'></i>")
    )
  }
  
  icon_model <- function() {
    div(
      style = "position: absolute; right: 1em",
      HTML("<i style='font-size:20px;' class='fas fa-calculator'></i>")
    )
  }
  
  icon_model_map <- function() {
    div(
      style = "position: absolute; right: 1em; top: 7em;",
      HTML("<i style='font-size:20px;' class='fas fa-calculator'></i>")
    )
  }
  
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
  
  about()
  
  ggblue <- "#56B4E9";#gg_co
  ggorange <- gg_color_hue(2)[1];
  ggred <- scales::hue_pal()(1)
  ggpurple <- "#796BD6"
  gggreen <- "#A3D596"
  
  map_predict <- st_zm(read_sf('shapefiles/predict_pc_simplified/shapefile_predict_pc.shp', quiet = TRUE))
  
  map_province <- read_sf('shapefiles/predict_nis/shapefile_predict_nis.shp', quiet = TRUE) %>% 
    group_by(prov_nl) %>%
    summarise()
  
  load('data/aggregated_data.RData')
  
  hide('tab_epidemiology')
  
  set_active_style <- function(id) {
    code <- paste0("document.getElementById('", id, "').className = document.getElementById('",id,"').className.replace('maintab', 'activetab')");
    shinyjs::runjs(code)         
  }
  
  remove_active_style <- function(id) {
    code <- paste0("document.getElementById('", id, "').className = document.getElementById('",id,"').className.replace('activetab', 'maintab')");
    shinyjs::runjs(code)         
  }

  
  observeEvent(input$participants, {
    set_active_style('participants')
    remove_active_style('epidemiology')

    show('tab_participants')
    hide('tab_epidemiology')
    
  })
  
  observeEvent(input$epidemiology, {
    set_active_style('epidemiology')
    remove_active_style('participants')
    
    #updateButton(session, "participants", label='test', style = "maintab") 
    #updateButton(session, "epidemiology", style = "activetab") 
    
    hide('tab_participants')
    show('tab_epidemiology')
  })
  
  render_everything <- function() {
    
    choose_province <- c('Belgium', map_province$prov_nl) 
    names(choose_province) <- translate(c('Belgium', map_province$prov_nl))
    
    #updateActionButton(session, 'about', label = translate('About'))
    
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
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_4', height='300')),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_map_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     )),
            tabPanel(translate("March 31"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_3', height='300'))),
            tabPanel(translate("March 24"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_2', height='300'))),
            tabPanel(translate("March 17"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_1', height='300')))
          ))
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
            tabPanel(translate("April 7"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_4', height='300')),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_map_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_4", "",
                                   choose_province,
                                   width = 140)
                     ),
                     icon_model_map()),
            tabPanel(translate("March 31"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
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
                     ),
                     icon_model_map()),
            tabPanel(translate("March 24"), 
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
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
                     ),
                     icon_model_map())
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
            tabPanel(translate("response"), 
                     icon = icon("chart-bar", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('q_bar_response', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('survey_response_count', height = 300)),
            tabPanel(translate("gender"), 
                     icon = icon("venus-mars", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('q_bar_gender', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('survey_response_gender', height = 300)),
            tabPanel(translate("age"), 
                     icon = icon("birthday-cake", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('q_bar_age', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('survey_response_age', height = 300)),
            tabPanel(translate("education"), 
                     icon = icon("user-graduate", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('q_bar_education', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('survey_response_education', height = 300))
          ))
    })
    
    output$survey_response_gender <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'gender')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = translate(c('March 17', 'March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('distribution_gender'))
      
    })
    
    output$survey_response_count <- renderPlot({
      
      data <- aggregated %>% 
        filter(variable == 'response_count')
      
      ggplot(data) +
        theme_bw() +
        geom_bar(aes(x = factor(translate(c('March 17', 'March 24', 'March 31', 'April 7')),
                                translate(c('March 17', 'March 24', 'March 31', 'April 7'))), 
                     y = value / 1000), fill = ggblue, stat = 'identity') +
        ylab(translate('responses')) + xlab('') +
        ggtitle(translate('survey_response'))
      
    })
      
    output$survey_response_age <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'age')) +
        theme_bw() +
        geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = translate(c('March 17', 'March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('distribution_age'))
      
    })
    
    output$survey_response_education <- renderPlot({
      
      
      ggplot(aggregated %>% filter(variable == 'education')) +
        theme_bw() +
        geom_bar(aes(x = translate(c('March 17', 'March 24', 'March 31', 'April 7'))[survey], y = value, fill =  reorder(translate(label), order)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple)) +
        ggtitle('Distribution of participants by level of education')
      
    })
    
    output$mitigation_bar_box <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = "",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", 
            height = 400,
            width = NULL,
            tabPanel(translate("work"), 
                     icon = icon("building", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     icon_observe(),
                     plotOutput('mitigation_homework', height = 300)),
            tabPanel(translate("behaviour work"), 
                     icon = icon("briefcase", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('mitigation_work', height = 300)),
            tabPanel(translate("behaviour public"), 
                     icon = icon("tree", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('mitigation_public', height = 300)),
            tabPanel(translate("behaviour household"), 
                     icon = icon("home", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('mitigation_household', height = 300)),
            tabPanel(translate("childcare"), 
                     icon = icon("baby-carriage", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('mitigation_childcare', height = 300)),
            tabPanel(translate("conversation"), 
                     icon = icon("comment-dots", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('mitigation_conversation', height = 300)),
            tabPanel(translate("fysical contact"), 
                     icon = icon("handshake", class = "opt"),
                     icon_observe(),
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
            tabPanel(translate("sleep"), 
                     icon = icon("bed", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('psychology_sleep', height = 300)),
            tabPanel(translate("concentrate"), 
                     icon = icon("lightbulb", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('psychology_concentrate', height = 300)),
            tabPanel(translate("optimistic"), 
                     icon = icon("smile-beam", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('psychology_optimistic', height = 300)),
            tabPanel(translate("tension"), 
                     icon = icon("bolt", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('psychology_tension', height = 300))
          ))
    })
    
    output$symptom_bar_box <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = "",
            id = "symptom_bar", 
            height = 400,
            width = NULL,
            tabPanel(translate("symptom"), 
                     icon = icon("virus", class = "opt"),
                     icon_observe(),
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                       actionButton('question_bar_responses', '',
                                    icon = icon("question", class = "opt")
                       )
                     ),
                     plotOutput('symptom', height = 300))
          ))
    })
    
    output$mitigation_homework <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == "work condition")) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = "dodge2") +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Current working conditions'))
      
    })
    
    output$mitigation_work <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_work',
                                   survey != 1)) +
        theme_bw() +
        geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Change in behaviour at work'))
      
    })
    
    output$mitigation_public <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_public',
                                   survey != 1)) +
        theme_bw() +
        geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Change in behaviour at public places'))
      
    })
    
    output$mitigation_childcare <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'childcare',
                                   survey != 1)) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Change in behaviour at public places'))
      
    })
    
    output$mitigation_household <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_household',
                                   survey != 1)) +
        theme_bw() +
        geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Change in behaviour at home'))
      
    })
    
    output$mitigation_conversation <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'number_people_talk',
                                   survey != 1)) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Number of people spoken today (not virtual)'))
      
    })
    
    output$mitigation_fysical_contact<- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'last_fysical_contact')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
        ggtitle(translate('last fysical contact (handshake or kiss)'))
      
    })
    
    output$psychology_sleep <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'sleep')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Sleep quality last week'))
      
    })
    
    output$psychology_concentrate <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'concentrate')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Concentration last week'))
      
    })
    
    output$psychology_optimistic <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'optimistic')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Optimism last week'))
      
    })
    
    output$psychology_tension <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'tension')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Tension last week'))
      
    })
    
    output$symptom <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'symptom')) +
        theme_bw() +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = translate(c('March 24', 'March 31', 'April 7'))) +
        ggtitle(translate('Percantage of participants reporting COVID19 symptoms'))
      
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
        label = paste0(translate(data$prov_nl),'\n',round(data$value*10000)/100, '%'))
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
  
 
  
  
  
  
  var <- c(map_predict$symptom2, map_predict$symptom3, map_predict$symptom4)
  
  breaks_map_symptom <- c(round(min(var),4)-0.0001,
                          round(as.numeric(quantile(var, probs = seq(0, 1, 1/6))),4)[2:6],
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
      plotclr, domain = map$ZipCode, 
      bins = color_bins
    )
    options = leafletOptions(zoomControl = FALSE)
    
    
    
    leaflet(data = map, options = leafletOptions(zoomControl = FALSE)) %>% 
      addLegend("bottomright", pal = color_pal, 
                values = ~ column_name, 
                title = translate("percentage")) %>%
      addPolygons(
        color = "gray",
        weight = 0.5,
        smoothFactor = 0.5, 
        opacity = 1.0,
        fillOpacity = 0.5,
        fillColor = ~color_pal(map[[column_name]]),
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            bringToFront = TRUE),
        label = paste0(translate(map$naam_nl),'\n',round(map[[column_name]]*10000)/100, '%'))
  }
  
  
  observeEvent(input$map_symptom,{
    
    if(input$map_symptom == 'April 7') {
      if(active_map_symptom[4] != input$select_province_4) {
        output$symptom_map_4 <-renderLeaflet({ map_symptom(4, 'symptom4', input$select_province_4)})
      }
    } else if(input$map_symptom == 'March 31') {
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
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_3)
    
    if(active_map_symptom[3] != input$select_province_3) {
      output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3)})
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$select_province_2, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_2)
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_2)
    
    if(active_map_symptom[2] != input$select_province_2) {
      output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_province_2, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_4)
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_4)
    
    if(active_map_symptom[4] != input$select_province_4) {
      output$symptom_map_4 <-renderLeaflet({ map_symptom(4, 'symptom4', input$select_province_4)})
    }
  }, ignoreInit = TRUE)
 
  # c(input$q_bar_response, input$q_bar_age, input$q_bar_gender, input$q_bar_education)
  observeEvent(c(input$q_bar_response, input$q_bar_gender, input$q_bar_age, input$q_bar_education), {
    validate(need(input$q_bar_response > 0, ''))
    showModal(modalDialog(
      easyClose = TRUE,
      title = 'Deelnemers coronastudie',
      includeHTML('information/info_survey_nl.html'),
      footer = NULL
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$about, {
    about()
  })
   
}

shinyApp(ui = ui, server = server)

