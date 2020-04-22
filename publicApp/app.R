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
require(grDevices)
require(scales)
require(plotly)
require(fmsb)

source('scripts/dictionary.R')
set_language('nl')

ui_epidemiology <- function() {
  div(id = 'tab_epidemiology',
      fluidRow(column(width = 12, uiOutput('symptom_bar_box'))),
      fluidRow(column(width = 6, uiOutput('symptom_map_box')),
               column(width = 6, uiOutput('symptom_heatmap_box'))),
      fluidRow(column(width = 6, uiOutput('symptom_spider_box')))
  )
}

ui_epidemiology_mobile <- function() {
  div(id = 'tab_epidemiology',
      div(id = 'epidemiology', class = 'mobile-header', "Epidemiology"),
      fluidRow(column(width = 12, uiOutput('symptom_bar_box'))),
      fluidRow(column(width = 6, uiOutput('symptom_map_box')),
               column(width = 6, uiOutput('symptom_heatmap_box'))),
      fluidRow(column(width = 6, uiOutput('symptom_spider_box')))
  )
}

ui_participants <- function() {
  div(id = 'tab_participants',
      fluidRow(column(width = 6,uiOutput('survey_response_map_box')),
               column(width = 6,uiOutput('survey_response_bar_box')))
  )
}

ui_participants_mobile <- function() {
  div(id = 'tab_participants',
      div(id = 'participants', class = 'mobile-header', "Participants"),
      fluidRow(column(width = 6,uiOutput('survey_response_map_box')),
               column(width = 6,uiOutput('survey_response_bar_box')))
  )
}

ui_mitigation <- function() {
  div(id = 'tab_mitigation',
      fluidRow(column(width = 10,uiOutput('mitigation_bar_box')),
               column(width = 2,uiOutput('mitigation_options_box'))),
      fluidRow(column(width = 6,uiOutput('mitigation_fysical_contact_box')),
               column(width = 6,uiOutput('mitigation_contact_province_box'))))
}

ui_mitigation_mobile <- function() {
  div(id = 'tab_mitigation',
      div(id = 'mitigation', class = 'mobile-header', "Social distancing"),
      fluidRow(column(width = 12,uiOutput('mitigation_bar_box'))),
      fluidRow(column(width = 6,uiOutput('mitigation_fysical_contact_box')),
               column(width = 6,uiOutput('mitigation_contact_province_box'))))
}

ui_mentalhealth <- function() {
  div(id = 'tab_mentalhealth',
      fluidRow(column(width = 10,uiOutput('psychology_bar_box')),
               column(width = 2,uiOutput('psychology_options_box'))),
      fluidRow(column(width = 6,uiOutput('psychology_ghq_box')),
               column(width = 6,uiOutput('psychology_tension_box')))
  )
}

ui_mentalhealth_mobile <- function() {
  div(id = 'tab_mentalhealth',
      div(id = 'mentalhealth', class = 'mobile-header', "Mental health"),
      fluidRow(column(width = 12,uiOutput('psychology_bar_box'))),
      fluidRow(column(width = 6,uiOutput('psychology_ghq_box')),
               column(width = 6,uiOutput('psychology_tension_box')))
  )
}

ui_society <- function() {
  div(id = 'tab_society',
      fluidRow(column(width = 6,uiOutput('society_topics_box')),
               column(width = 6,uiOutput('society_financial_bar')))
  )
}

ui_society_mobile <- function() {
  div(id = 'tab_society',
      div(id = 'society', class = 'mobile-header', "Society"),
      fluidRow(column(width = 6,uiOutput('society_topics_box')),
               column(width = 6,uiOutput('society_financial_bar')))
  )
}

ui_header_menu <- function() {
  # div(
  #   style = "position: absolute; right: 1.7em; top: -0.9em; z-index:1050; display:inline-block; text-align:center;",
  #   fluidRow(width = '180px',
  #           column(9, selectInput('language', '',
  #                c('Nederlands' = 'nl', 'English' = 'en'),
  #                width = 160),
  #                style = 'text-align:left;'),
  #            column(3, 
  #                   div(style = 'position:relative; top: 1.4em;',
  #                       actionButton('about', '',
  #                         icon = icon("question", class = "opt"))))
  #   )
  #   )
  
  div(
    style = "position: absolute; right: 1.7em; top: -0.9em; z-index:1050; display:inline-block; text-align:center;",
    div(selectInput('language', '',
                c('Nederlands' = 'nl', 'English' = 'en'),
                width = 160),
                style = 'text-align:left; display:inline-block; float:left'),
             div(style = 'position:relative; top: 1.4em; float:left; margin-left:10px;',
                        actionButton('about', '',
                                     icon = icon("question", class = "opt"))
    )
  )
}

ui_header_logo <- function() {

  div(id='logos',
    style = "position: absolute; left: 300px; top: 0px; z-index:1050; display:inline-block; text-align:center;",
    img(src='ua.png', height='50px', style='margin:0px;'),
    img(src='uhasselt.png', height='50px', style='margin:0px'),
    img(src='kul.png', height='50px', style='margin:0px')
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
                 style = "maintab"),
        bsButton("epidemiology", 
                 label = "\u00A0Epidemiology", 
                 icon = icon("viruses"), 
                 style = "activetab"),
        bsButton("mitigation", 
                 label = "\u00A0Social distancing", 
                 icon = icon("people-arrows"), 
                 style = "maintab"),
        bsButton("mentalhealth", 
                 label = "\u00A0Mental health", 
                 icon = icon("brain"), 
                 style = "maintab"),
      bsButton("society", 
               label = "\u00A0Society", 
               icon = icon("globe"), 
               style = "maintab")
    )
  )
}

ui <- dashboardPage(dashboardHeader(title = "Coronastudie"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      tags$head(
                        tags$head(includeHTML('google_analytics.js')),
                        tags$link(
                          rel = "stylesheet", 
                          type = "text/css", 
                          href = "fontawesome/css/all.css"),
                        tags$link(
                          rel = "stylesheet", 
                          type = "text/css", 
                          href = "corona_survey_style.css"),
                        tags$script(HTML("
                      function openInfo(msg){
                          Shiny.onInputChange('openInfo', Math.random());   
                      }

                      Shiny.addCustomMessageHandler('go_mobile', 
                          function(message) {
                              window.location = 'https://corona-studie.shinyapps.io/corona-studie-mobile/';
                          });

                     ")),
                        tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '))
                        ),
                      useShinyjs(),
                      ui_header_menu(),
                      ui_header_logo(),
                      ui_button_bar(),
                      ui_participants(),
                      ui_epidemiology(),
                      ui_mitigation(),
                      ui_mentalhealth(),
                      ui_society()))


ui_mobile <- dashboardPage(dashboardHeader(title = "Coronastudie"),
                        dashboardSidebar(disable = TRUE),
                        dashboardBody(
                          tags$head(
                            tags$head(includeHTML('google_analytics.js')),
                            tags$link(
                              rel = "stylesheet", 
                              type = "text/css", 
                              href = "fontawesome/css/all.css"),
                            tags$link(
                              rel = "stylesheet", 
                              type = "text/css", 
                              href = "corona_survey_style.css"),
                            tags$script(HTML("
                                             function openInfo(msg){
                                             Shiny.onInputChange('openInfo', Math.random());   
                                             }
                                             
                                             Shiny.addCustomMessageHandler('go_mobile', 
                                             function(message) {
                                             window.location = 'http://www.google.com';
                                             });
                                             
                                             ")),
                            tags$head(tags$script('
                                                  var dimension = [0, 0];
                                                  $(document).on("shiny:connected", function(e) {
                                                  dimension[0] = window.innerWidth;
                                                  dimension[1] = window.innerHeight;
                                                  Shiny.onInputChange("dimension", dimension);
                                                  });
                                                  $(window).resize(function(e) {
                                                  dimension[0] = window.innerWidth;
                                                  dimension[1] = window.innerHeight;
                                                  Shiny.onInputChange("dimension", dimension);
                                                  });
                                                  '))
                        ),
                      useShinyjs(),
                      ui_header_menu(),
                      ui_participants_mobile(),
                      ui_epidemiology_mobile(),
                      ui_mitigation_mobile(),
                      ui_mentalhealth_mobile(),
                      ui_society_mobile()))

server <- function(session, input, output) {
  
  mobile <- FALSE
  startup <- TRUE

  if(mobile) {
    plot_style <- theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  } else {
    plot_style <- theme_bw() 
  }
  
  icon_observe <- function(tab = ''){
    div(
      style = "position: absolute; right: 1em; z-index:1000;",
      HTML(paste0("<i style='font-size:20px;' tag='",tab,"' class='fas fa-eye'></i>"))
    )
  }
  
  icon_model <- function(tab = '') {
    div(
      style = "position: absolute; right: 1em;  z-index:1000;",
      HTML(paste0("<i style='font-size:20px;' tag='",tab,"' class='fas fa-calculator'></i>"))
    )
  }
  
  icon_model_map <- function() {
    div(
      style = "position: absolute; right: 1em; top: 10em;",
      HTML("<i style='font-size:20px;' class='fas fa-calculator'></i>")
    )
  }
  
  btn_question <- function(id) {
    div(
      style = "position: absolute; left: 0.5em; bottom: 0.5em; z-index:1000;",
      actionButton(id, '',
                   icon = icon("question", class = "opt"),
                   style = 'background-color:#56B4E9; color:#fff;'
      )
    )
  }
  
  set_icon <- function(tag, icon = 'eye') {
    if(icon == 'eye') {
      code <- paste0("$('i[tag=\"", tag, "\"]').removeClass('fa-calcualtor').addClass('fa-eye')");
    } else {
      code <- paste0("$('i[tag=\"", tag, "\"]').removeClass('fa-eye').addClass('fa-calculator')");
    }
    
    shinyjs::runjs(code)  
    
  }
  
  about <- function() {
    
    language <- c('Nederlands', 'English', 'fran\u00E7ais')[match(colnames(dictionary)[active_language], c('nl', 'en', 'fr'))]
    
    showModal(modalDialog(
      easyClose = TRUE,
      title = translate('Corona Survey'),
      size = 'm',
      tabBox(
        title = "",
        width = NULL,
        id = "info_corona_survey",
        tabPanel("Nederlands", 
                 includeHTML('information/info_nl.html')
        ),
        tabPanel("English", 
                 includeHTML('information/info_en.html')
        ),
        selected = language
      ),
      footer = NULL
    ))
    
    
    # tabPanel("fran\u00E7ais", 
    #          includeHTML('information/info_en.html')
    # ),
    
    observeEvent(input$info_corona_survey, {
      language <- c('nl', 'en', 'fr')[match(input$info_corona_survey, c('Nederlands', 'English', 'fran\u00E7ais'))]
      updateSelectInput(session, 'language', selected = language)
    })
  }

  observe({
    input$openInfo

    if(!is.null(input$openInfo)) {
      showModal(modalDialog(
        easyClose = TRUE,
        title = translate('Corona Survey'),
        size = 'l',
        includeHTML(translate('information/credits.html')),
        footer = NULL
      ))
    }
    
  })
  
  set_active_style <- function(id) {
    code <- paste0("document.getElementById('", id, "').className = document.getElementById('",id,"').className.replace('maintab', 'activetab')");
    shinyjs::runjs(code)         
  }
  
  remove_active_style <- function(id) {
    code <- paste0("document.getElementById('", id, "').className = document.getElementById('",id,"').className.replace('activetab', 'maintab')");
    shinyjs::runjs(code)         
  }

  set_content_by_id <- function(id, value) {
    code <- paste0("document.getElementById('", id, "').innerHTML = '", value, "';");
    shinyjs::runjs(code)    
  }
  
  observeEvent(input$participants, {
    set_active_style('participants')
    remove_active_style('epidemiology')
    remove_active_style('mitigation')
    remove_active_style('mentalhealth')
    remove_active_style('society')
    
    shinyjs::show('tab_participants')
    hide('tab_epidemiology')
    hide('tab_mitigation')
    hide('tab_mentalhealth')
    hide('tab_society')
    
  })
  
  observeEvent(input$epidemiology, {
    set_active_style('epidemiology')
    remove_active_style('participants')
    remove_active_style('mitigation')
    remove_active_style('mentalhealth')
    remove_active_style('society')

    hide('tab_participants')
    hide('tab_mitigation')
    hide('tab_mentalhealth')
    hide('tab_society')
    shinyjs::show('tab_epidemiology')
  })
  
  observeEvent(input$mitigation, {
    set_active_style('mitigation')
    remove_active_style('participants')
    remove_active_style('epidemiology')
    remove_active_style('mentalhealth')
    remove_active_style('society')
    
    hide('tab_participants')
    hide('tab_epidemiology')
    hide('tab_mentalhealth')
    hide('tab_society')
    shinyjs::show('tab_mitigation')
  })
  
  observeEvent(input$mentalhealth, {
    set_active_style('mentalhealth')
    remove_active_style('participants')
    remove_active_style('epidemiology')
    remove_active_style('mitigation')
    remove_active_style('society')
    
    hide('tab_participants')
    hide('tab_epidemiology')
    hide('tab_mitigation')
    hide('tab_society')
    shinyjs::show('tab_mentalhealth')
  })
  
  observeEvent(input$society, {
    set_active_style('society')
    remove_active_style('participants')
    remove_active_style('epidemiology')
    remove_active_style('mitigation')
    remove_active_style('mentalhealth')
    
    hide('tab_participants')
    hide('tab_epidemiology')
    hide('tab_mitigation')
    hide('tab_mentalhealth')
    shinyjs::show('tab_society')
  })
  
  map_symptom <- function(survey_number, column_name, province = 'Belgium', legend = 'Time dependent') {
    
    active_map_symptom[survey_number] <<- paste0(province,legend);
    
    map <- map_predict
    
    if(province != 'Belgium') {
      map <- map %>% 
        filter(prov_nl == province)
    }
    
    values <- map[[column_name]]
    
    if(legend == 'Fixed') {
      breaks <- breaks_map_symptom
      
      title <- translate('percentage')
      labels <- paste0(breaks[1:6]*100,'-',breaks[2:7]*100,'')
    } else {
      breaks <- c(round(min(values),4)-0.0001,
                  round(as.numeric(quantile(values, probs = seq(0, 1, 1/6))),4)[2:6],
                  round(max(values),4)+0.0001)
      
      title <- translate("Prevalence")
      labels <- c(translate('Low'), rep('', 4), translate('High'))
    }
    
    nclr <- 6      
    plotclr <- brewer.pal(nclr,"YlOrRd")
    
    class <- classIntervals(values, nclr, style = "fixed", fixedBreaks = breaks)
    color_bins <- class$brks
    
    color_pal <- colorBin( 
      plotclr, domain = map$gemeente, 
      bins = color_bins
    )
    
    leaflet(data = map, options = leafletOptions(zoomControl = FALSE)) %>% 
      addLegend("bottomright", 
                colors= plotclr,
                title = title,
                labels = labels) %>%
      addPolygons(
        color = "gray",
        weight = 0.5,
        smoothFactor = 0.5, 
        opacity = 1.0,
        fillOpacity = 0.5,
        fillColor = ~color_pal(map[[column_name]]),
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            bringToFront = TRUE),
        label = paste0(translate(map$gemeente),'\n',round(map[[column_name]]*10000)/100, '%'))
    
    
    
  }
  
  
  
  render_everything <- function() {
    
    choose_province <- c('Belgium', map_province$prov_nl) 
    names(choose_province) <- translate(c('Belgium', map_province$prov_nl))
    
    choose_legend <- c('Time dependent', 'Fixed')
    names(choose_legend) <- translate(c('Legend: Time dependent', 'Legend: Fixed'))
    
    dates <- rev(translate(c('April 21', 'April 14', 'April 7', 'March 31', 'March 24', 'March 17')))
    colors <- c(ggblue, ggorange, gggreen, ggpurple, '#f5ce42', '#34ebbd')
    #updateActionButton(session, 'about', label = translate('About'))
    
    if(mobile) {
      set_content_by_id('participants', translate('Participants'))
      set_content_by_id('epidemiology', translate('Epidemiology'))
      set_content_by_id('mitigation', translate('Social distancing'))
      set_content_by_id('mentalhealth', translate('Mental health'))
      set_content_by_id('society', translate('Society'))
    } else {
      updateButton(session, 'participants', label = paste0('&nbsp;&nbsp;',translate('Participants')))
      updateButton(session, 'epidemiology', label = paste0('&nbsp;&nbsp;',translate('Epidemiology')))
      updateButton(session, 'mitigation', label = paste0('&nbsp;&nbsp;',translate('Social distancing')))
      updateButton(session, 'mentalhealth', label = paste0('&nbsp;&nbsp;',translate('Mental health')))
      updateButton(session, 'society', label = paste0('&nbsp;&nbsp;',translate('Society')))
    }
    
    shinyjs::runjs(paste0("document.getElementsByClassName('logo')[0].innerHTML = '", translate('Corona Survey'), "'"))    
    
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
            tabPanel(translate("April 21"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_6', height='300')),
                     btn_question('question_map_response6')),
            tabPanel(translate("April 14"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_5', height='300')),
                     btn_question('question_map_response1')),
            tabPanel(translate("April 7"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_4', height='300')),
                     btn_question('question_map_response2')),
            tabPanel(translate("March 31"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_3', height='300')),
                     btn_question('question_map_response3')),
            tabPanel(translate("March 24"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_2', height='300')),
                     btn_question('question_map_response4')),
            tabPanel(translate("March 17"), 
                     icon_observe(),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Distribution participants by province')),
                     withSpinner(leafletOutput('survey_response_1', height='300')),
                     btn_question('question_map_response5'))
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
            tabPanel(translate("April 21"),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_6', height='300')),
                     btn_question('question_map_symptom6'),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_6", "",
                                   choose_province,
                                   width = 200)
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5.5em;",
                       selectInput("select_legend_6", "",
                                   choose_legend,
                                   width = 200)
                     ),
                     icon_model_map()),
            tabPanel(translate("April 14"),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_5', height='300')),
                     btn_question('question_map_symptom5'),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_5", "",
                                   choose_province,
                                   width = 200)
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5.5em;",
                       selectInput("select_legend_5", "",
                                   choose_legend,
                                   width = 200)
                     ),
                     icon_model_map()),
            tabPanel(translate("April 7"),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_4', height='300')),
                     btn_question('question_map_symptom4'),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_4", "",
                                   choose_province,
                                   width = 200)
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5.5em;",
                       selectInput("select_legend_4", "",
                                   choose_legend,
                                   width = 200)
                     ),
                     icon_model_map()),
            tabPanel(translate("March 31"),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_3', height='300')),
                     btn_question('question_map_symptom3'),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_3", "",
                                   choose_province,
                                   width = 200)
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5.5em;",
                       selectInput("select_legend_3", "",
                                   choose_legend,
                                   width = 200)
                     ),
                     icon_model_map()),
            tabPanel(translate("March 24"),
                     div(style = 'font-size:24px; color:#F8766D;', translate('Probability of having symptoms')),
                     withSpinner(leafletOutput('symptom_map_2', height='300')),
                     btn_question('question_map_symptom2'),
                     div(
                       style = "position: absolute; right: 0.5em; top: 2.5em;",
                       selectInput("select_province_2", "",
                                   choose_province,
                                   width = 200)
                     ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5.5em;",
                       selectInput("select_legend_2", "",
                                   choose_legend,
                                   width = 200)
                     ),
                     icon_model_map())
          ))
    })
    
    output$symptom_bar_box <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = "",
            id = "tabset1", 
            height = 400,
            width = NULL,
            tabPanel(translate("symptoms"), 
                     icon = icon('virus'),
                     icon_observe(),
                     btn_question('question_symptom_all'),
                     withSpinner(plotOutput('symptom', height = 300))),
            tabPanel(translate("fast_fever"), 
                     icon_observe(),
                     btn_question('question_symptom1'),
                     withSpinner(plotOutput('examine_fast_fever', height = 300))),
            tabPanel(translate("high_fever"), 
                     icon_observe(),
                     btn_question('question_symptom2'),
                     withSpinner(plotOutput('examine_high_fever', height = 300))),
            tabPanel(translate("sore_throat"), 
                     icon_observe(),
                     btn_question('question_symptom3'),
                     withSpinner(plotOutput('examine_sore_throat', height = 300))),
            tabPanel(translate("short_breath"), 
                     icon_observe(),
                     btn_question('question_symptom4'),
                     withSpinner(plotOutput('examine_short_breath', height = 300))),
            tabPanel(translate("dry_cough"), 
                     icon_observe(),
                     btn_question('question_symptom5'),
                     withSpinner(plotOutput('examine_dry_cough', height = 300))),
            tabPanel(translate("deep_cough"), 
                     icon_observe(),
                     btn_question('question_symptom6'),
                     withSpinner(plotOutput('examine_deep_cough', height = 300))),
            tabPanel(translate("chest_pain"), 
                     icon_observe(),
                     btn_question('question_symptom7'),
                     withSpinner(plotOutput('examine_chest_pain', height = 300))),
            tabPanel(translate("runny_nose"), 
                     icon_observe(),
                     btn_question('question_symptom8'),
                     withSpinner(plotOutput('examine_runny_nose', height = 300))),
            tabPanel(translate("muscle_ache"), 
                     icon_observe(),
                     btn_question('question_symptom9'),
                     withSpinner(plotOutput('examine_muscle_ache', height = 300))),
            tabPanel(translate("symptom_fatiguee"), 
                     icon_observe(),
                     btn_question('question_symptom10'),
                     withSpinner(plotOutput('examine_fatiguee', height = 300))),
            tabPanel(translate("symptom_shivery"), 
                     icon_observe(),
                     btn_question('question_symptom11'),
                     withSpinner(plotOutput('examine_shivery', height = 300))),
            tabPanel(translate("symptom_nausea"), 
                     icon_observe(),
                     btn_question('question_symptom12'),
                     withSpinner(plotOutput('examine_nausea', height = 300))),
            tabPanel(translate("sore_eyes"), 
                     icon_observe(),
                     btn_question('question_symptom13'),
                     withSpinner(plotOutput('examine_sore_eyes', height = 300)))
          ))
    })
    
    output$symptom_heatmap_box <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = "",
            id = "symptom_heatmap", 
            height = 400,
            width = NULL,
            tabPanel(translate("April 21"), 
                     icon_observe(),
                     btn_question('question_symptom_heatmap6'),
                     withSpinner(plotOutput('heatmap_symptom_6', height = 300))),
            tabPanel(translate("April 14"), 
                     icon_observe(),
                     btn_question('question_symptom_heatmap5'),
                     withSpinner(plotOutput('heatmap_symptom_5', height = 300))),
            tabPanel(translate("April 7"), 
                     icon_observe(),
                     btn_question('question_symptom_heatmap4'),
                     withSpinner(plotOutput('heatmap_symptom_4', height = 300))),
            tabPanel(translate("March 31"), 
                     icon_observe(),
                     btn_question('question_symptom_heatmap3'),
                     withSpinner(plotOutput('heatmap_symptom_3', height = 300))),
            tabPanel(translate("March 24"), 
                     icon_observe(),
                     btn_question('question_symptom_heatmap2'),
                     withSpinner(plotOutput('heatmap_symptom_2', height = 300)))
          ))
    })
  
    
    output$symptom_spider_box <- renderUI({
      div(style = 'position:relative;',
          box(
          title = "",
          height = 400,
          width = NULL,
          icon_observe(),
          btn_question('question_symptom_spider'),
           div(
             style = 'position: relative; top:-30px',
             withSpinner(plotOutput('symptom_spider', height = 320))
           )
          ))
    })

    output$symptom_spider <- renderPlot({

      prevalence <- aggregated %>%
        filter(weight == 'uniform_weight',
               variable == 'symptom') %>%
        pivot_wider(id_cols = survey, names_from = label, values_from = value)

      prevalence_matrix <- as.matrix(prevalence)[, 2:14]
      names <- translate(colnames(prevalence_matrix))
      
      data <- data.frame(rbind(rep(1.5,13) , rep(0,13) , prevalence_matrix[1:5,] / matrix(rep(prevalence_matrix[1,], 5), 5, byrow = TRUE)))
      
      colnames(data) <- names;
      rownames(data) <- translate(c('1', '2', 'March 24', 'March 31', 'April 7', 'April 14', 'April 21'))
      
      par(mar = c(0, 2, 3, 2))
      
      radarchart(data,
                 axistype = 0,
                 pty = 32,
                 seg = 3,
                 plty = 1,
                 plwd = 1.5,
                 pcol = c('black', colors[3:6]),
                 pfcol = c(rgb(1, 1, 1, alpha = 0), rgb(t(col2rgb(colors[3:6])/255), alpha = 0.5)),
                 centerzero = TRUE,
                 vlcex=0.8,
                 title = translate('Evolution of symptoms'))

      legend(x=1.2, y=1.3, legend = rownames(data[-c(1:3),]), bty = "n", pch=20 , col=c(rgb(t(col2rgb(colors[3:6])/255), alpha = 0.5)) , text.col = "black", cex=1, pt.cex=3)
    })
    
    plot_examine <- function(symptom) {
      ggplot(aggregated %>% filter(variable == paste0('symptom_', symptom),
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors[2:6], label = dates[2:6]) +
        ggtitle(paste0(translate('Diagnosis received by patients with '), translate(symptom), translate(' after docter examination')))
    }
    
    output$examine_fast_fever <- renderPlot({plot_examine('fast_fever')})
    output$examine_high_fever <- renderPlot({plot_examine('high_fever')})
    output$examine_sore_throat <- renderPlot({plot_examine('sore_throat')})
    output$examine_short_breath <- renderPlot({plot_examine('short_breath')})
    output$examine_dry_cough <- renderPlot({plot_examine('dry_cough')})
    output$examine_deep_cough <- renderPlot({plot_examine('deep_cough')})
    output$examine_chest_pain <- renderPlot({plot_examine('chest_pain')})
    output$examine_runny_nose <- renderPlot({plot_examine('runny_nose')})
    output$examine_muscle_ache <- renderPlot({plot_examine('muscle_ache')})
    output$examine_fatiguee <- renderPlot({plot_examine('symptom_fatiguee')})
    output$examine_shivery <- renderPlot({plot_examine('symptom_shivery')})
    output$examine_nausea <- renderPlot({plot_examine('symptom_nausea')})
    output$examine_sore_eyes <- renderPlot({plot_examine('sore_eyes')})
    
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
                     btn_question('q_bar_response'),
                     withSpinner(plotOutput('survey_response_count', height = 300))),
            tabPanel(translate("gender"), 
                     icon = icon("venus-mars", class = "opt"),
                     icon_observe(),
                     btn_question('q_bar_gender'),
                     withSpinner(plotOutput('survey_response_gender', height = 300))),
            tabPanel(translate("age"), 
                     icon = icon("birthday-cake", class = "opt"),
                     icon_observe(),
                     btn_question('q_bar_age'),
                     withSpinner(plotOutput('survey_response_age', height = 300))),
            tabPanel(translate("education"), 
                     icon = icon("user-graduate", class = "opt"),
                     icon_observe(),
                     btn_question('q_bar_education'),
                     withSpinner(plotOutput('survey_response_education', height = 300))),
            tabPanel(translate("family size"), 
                     icon = icon("users", class = "opt"),
                     icon_observe(),
                     btn_question('q_bar_family_size'),
                     withSpinner(plotOutput('survey_response_family_size', height = 300)))
          ))
    })
    
    output$survey_response_gender <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'gender',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors, label = dates) +
        ggtitle(translate('distribution_gender'))
      
    })
    
    output$survey_response_count <- renderPlot({
      
      data <- aggregated %>% 
        filter(variable == 'response_count',
               weight == 'uniform_weight')
      
      ggplot(data) +
        plot_style +
        geom_bar(aes(x = factor(dates, dates), 
                     y = value / 1000), fill = ggblue, stat = 'identity') +
        ylab(translate('responses')) + xlab('') +
        ggtitle(translate('survey_response'))
      
    })
      
    output$survey_response_age <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'age',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors, label = dates) +
        ggtitle(translate('distribution_age'))
      
    })
    
    output$survey_response_education <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'education',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = as.factor(survey) ), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors, label = dates) +
        ggtitle(translate('Distribution of participants by level of education'))
      
    })
    
    output$survey_response_family_size <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'members',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = as.factor(survey) ), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors, labels = dates) +
        ggtitle(translate('Distribution of participants by number of other family members'))
      
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
                     icon_model('mitigation'),
                     btn_question('question_mitigation_work'),
                     withSpinner(plotOutput('mitigation_homework', height = 300))),
            tabPanel(translate("behaviour work"), 
                     icon = icon("briefcase", class = "opt"),
                     icon_model('mitigation'),
                     btn_question('question_mitigation_bework'),
                     withSpinner(plotOutput('mitigation_work', height = 300))),
            tabPanel(translate("behaviour public"), 
                     icon = icon("tree", class = "opt"),
                     icon_model('mitigation'),
                     btn_question('question_mitigation_bepublic'),
                     withSpinner(plotOutput('mitigation_public', height = 300))),
            tabPanel(translate("behaviour household"), 
                     icon = icon("home", class = "opt"),
                     icon_model('mitigation'),
                     btn_question('question_mitigation_behome'),
                     withSpinner(plotOutput('mitigation_household', height = 300))),
            tabPanel(translate("childcare"), 
                     icon = icon("baby-carriage", class = "opt"),
                     icon_model('mitigation'),
                     btn_question('question_mitigation_childcare'),
                     withSpinner(plotOutput('mitigation_childcare', height = 300))),
            tabPanel(translate("conversation"), 
                     icon = icon("comment-dots", class = "opt"),
                     icon_model('mitigation'),
                     btn_question('question_mitigation_conversation'),
                     withSpinner(plotOutput('mitigation_conversation', height = 300))))
      )
    })
    
    output$mitigation_fysical_contact_box <- renderUI({
      div(style = 'position:relative;',
          box(
            height = 400,
            width = NULL,
            title = '',
            solidHeader = TRUE,
            icon_model(),
            btn_question('question_mitigation_fysical'),
            div(
              style = 'position: relative; top:-30px',
              withSpinner(plotOutput('mitigation_fysical_contact', height = 350))
            )
          )
      )
    })
    
    output$mitigation_contact_province_box <- renderUI({
      div(style = 'position:relative;',
          box(
            height = 400,
            width = NULL,
            title = '',
            solidHeader = TRUE,
            icon_model('mitigation'),
            btn_question('question_mitigation_contact_province'),
            div(
              style = 'position: relative; top:-30px',
              withSpinner(plotOutput('mitigation_contact_province', height = 350))
            )
          )
      )
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
                     icon_model('mental'),
                     btn_question('question_psychology_sleep'),
                     withSpinner(plotOutput('psychology_sleep', height = 300))),
            tabPanel(translate("concentrate"), 
                     icon = icon("lightbulb", class = "opt"),
                     icon_model('mental'),
                     btn_question('question_psychology_concentrate'),
                     withSpinner(plotOutput('psychology_concentrate', height = 300))),
            tabPanel(translate("optimistic"), 
                     icon = icon("smile-beam", class = "opt"),
                     icon_model('mental'),
                     btn_question('question_psychology_optimistic'),
                     withSpinner(plotOutput('psychology_optimistic', height = 300))),
            tabPanel(translate("tension"), 
                     icon = icon("bolt", class = "opt"),
                     icon_model('mental'),
                     btn_question('question_psychology_tension'),
                     withSpinner(plotOutput('psychology_tension', height = 300)))
          ))
    })
    
    survey_options <- 6:2
    names(survey_options) <- dates[6:2]
    
    grouping_options <- c('Survey', 'Age', 'Gender', 'Housemember', 'Covid_status', 'Contact_status', 'Financial')
    names(grouping_options) <- translate(c('Survey', 'Age category', 'Gender', 'Family size', 'COVID-19 diagnosis', 'Fysical contact', 'Financial situation'))
    
    grouping_options <- c('Survey', 'Age', 'Gender')
    names(grouping_options) <- translate(c('Survey', 'Age category', 'Gender'))
    
    
    method_options <- c('Model predictions', 'Direct statistics')
    names(method_options) <- translate(c('Model predictions', 'Direct statistics'))
    
    filter_var <- c('---', 'Age', 'Gender', 'Housemember', 'Covid_status', 'Contact_status', 'Financial')
    names(filter_var) <- c('---',translate( c('Age category', 'Gender', 'Family size', 'COVID-19 diagnosis', 'Fysical contact', 'Financial situation')))
    
    filter_var <- c('---', 'Age', 'Gender')
    names(filter_var) <- c('---',translate( c('Age category', 'Gender')))
    
    
    filter_var_age <- c('All', '0-17 year', '18-65 year', '65+ year')
    names(filter_var_age) <- translate(c('All', '0-17 year', '18-65 year', '65+ year'))
    
    filter_var_gender <- c('All', 'Man', 'Vrouw')
    names(filter_var_gender) <- translate(c('All', 'Male', 'Female'))
    
    filter_var_member <- c('All', 'alone', 'two persons', '3-5 persons', '5+ persons')
    names(filter_var_member) <- translate(c('All', 'alone', 'two persons', '3-5 persons', '5+ persons'))
    
    filter_var_covid_status <- c('All', 'diagnosis_covid', 'diagnosis_potential_covid', 'diagnosis_other', 'symptoms_no_diagnosis', 'no symptoms')
    names(filter_var_covid_status) <- translate(c('All', 'diagnosis_covid', 'diagnosis_potential_covid', 'diagnosis_other', 'symptoms_no_diagnosis', 'no symptoms'))
    
    filter_var_financial <- c('All', 'Zeer gemakkelijk', 'Gemakkelijk', 'Eerder gemakkelijk', 'Eerder moeilijk', 'Moeilijk')
    names(filter_var_financial) <- translate(c('All', 'Zeer gemakkelijk', 'Gemakkelijk', 'Eerder gemakkelijk', 'Eerder moeilijk', 'Moeilijk'))
    
    filter_var_contact <- c('All', 'no fysical contact', 'fysical contact')
    names(filter_var_contact) <- translate(c('All', 'no fysical contact', 'fysical contact'))
    
    filter_var_groups <- list(Age = filter_var_age,
                              Gender = filter_var_gender,
                              Housemember = filter_var_member,
                              Covid_status = filter_var_covid_status,
                              Contact_status = filter_var_contact,
                              Financial = filter_var_financial)
    
    filter_var_groups <- list(Age = filter_var_age,
                              Gender = filter_var_gender)
    
    no_choice <- c('')
    
    output$psychology_options_box <- renderUI({
      box(
        id = "psychology_options",
        title = translate('Plot options'),
        height = 400,
        width = NULL,
        solidHeader=TRUE,
        status = "warning",
        selectizeInput('psychology_option_approach', label = paste0(translate('Results'),':'),
                       choices = method_options),
        selectizeInput('psychology_option_group', label = paste0(translate('Grouping'),':'),
                       choices = grouping_options),
        selectizeInput('psychology_option_filter_var', label = paste0(translate('Filter'),':'),
                       choices = filter_var),
        div(style='position:relative; top:-25px;', disabled(selectizeInput('psychology_option_filter_var_group', label='', choices = no_choice))),
        div(id='psychology_option_survey_box', style='display:none', selectizeInput('psychology_option_survey', label = paste0(translate('Survey'),':'),
                       choices = survey_options))
      )
    })
    
    observeEvent(input$psychology_option_filter_var, {
      if(input$psychology_option_filter_var == '---') {
        updateSelectInput(session, 
                          'psychology_option_filter_var_group',
                          choices = no_choice)
        shinyjs::disable('psychology_option_filter_var_group')
      } else {
        updateSelectInput(session, 
                          'psychology_option_filter_var_group',
                          choices = filter_var_groups[[input$psychology_option_filter_var]])
        shinyjs::enable('psychology_option_filter_var_group')
      }
    })
    
    observeEvent(input$psychology_option_group, {
      if(input$psychology_option_group == 'Survey') {
        shinyjs::hide('psychology_option_survey_box')
        shinyjs::show('psychology_option_filter_var_group')
        shinyjs::show('psychology_option_filter_var')
      } else {
        shinyjs::show('psychology_option_survey_box')
        shinyjs::hide('psychology_option_filter_var_group')
        shinyjs::hide('psychology_option_filter_var')
      }
    })
    
    observeEvent(input$psychology_option_approach, {
      if(input$psychology_option_approach == 'Direct statistics') {
        set_icon('mental', 'eye')
      } else {
        set_icon('mental', 'calculator')
      }
    })
    
    output$mitigation_options_box <- renderUI({
      box(
        id = "mitigation_options",
        title = translate('Plot options'),
        height = 400,
        width = NULL,
        solidHeader=TRUE,
        status = "warning",
        selectizeInput('mitigation_option_approach', label = paste0(translate('Results'),':'),
                       choices = method_options),
        selectizeInput('mitigation_option_group', label = paste0(translate('Grouping'),':'),
                       choices = grouping_options),
        selectizeInput('mitigation_option_filter_var', label = paste0(translate('Filter'),':'),
                       choices = filter_var),
        div(style='position:relative; top:-25px;', disabled(selectizeInput('mitigation_option_filter_var_group', label='', choices = no_choice))),
        div(id='mitigation_option_survey_box', style='display:none', selectizeInput('mitigation_option_survey', label = paste0(translate('Survey'),':'),
                                                                                    choices = survey_options))
      )
    })
    
    observeEvent(input$mitigation_option_filter_var, {
      if(input$mitigation_option_filter_var == '---') {
        updateSelectInput(session, 
                          'mitigation_option_filter_var_group',
                          choices = no_choice)
        shinyjs::disable('mitigation_option_filter_var_group')
      } else {
        updateSelectInput(session, 
                          'mitigation_option_filter_var_group',
                          choices = filter_var_groups[[input$mitigation_option_filter_var]])
        shinyjs::enable('mitigation_option_filter_var_group')
      }
    })
    
    observeEvent(input$mitigation_option_group, {
      if(input$mitigation_option_group == 'Survey') {
        shinyjs::hide('mitigation_option_survey_box')
        shinyjs::show('mitigation_option_filter_var_group')
        shinyjs::show('mitigation_option_filter_var')
      } else {
        shinyjs::show('mitigation_option_survey_box')
        shinyjs::hide('mitigation_option_filter_var_group')
        shinyjs::hide('mitigation_option_filter_var')
      }
    })
    
    observeEvent(input$mitigation_option_approach, {
      if(input$mitigation_option_approach == 'Direct statistics') {
        set_icon('mitigation', 'eye')
      } else {
        set_icon('mitigation', 'calculator')
      }
    })
    
    #shinyjs::hide('psychology_option_survey')
    
    output$psychology_tension_box <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = "",
            id = "psychology_tension", 
            height = 400,
            width = NULL,
            tabPanel(translate("April 21"), 
                     icon_observe(),
                     btn_question('question_tension_heatmap6'),
                     withSpinner(plotOutput('heatmap_tension_6', height = 300))),
            tabPanel(translate("April 14"), 
                     icon_observe(),
                     btn_question('question_tension_heatmap5'),
                     withSpinner(plotOutput('heatmap_tension_5', height = 300))),
            tabPanel(translate("April 7"), 
                     icon_observe(),
                     btn_question('question_tension_heatmap4'),
                     withSpinner(plotOutput('heatmap_tension_4', height = 300))),
            tabPanel(translate("March 31"), 
                     icon_observe(),
                     btn_question('question_tension_heatmap3'),
                     withSpinner(plotOutput('heatmap_tension_3', height = 300))),
            tabPanel(translate("March 24"), 
                     icon_observe(),
                     btn_question('question_tension_heatmap2'),
                     withSpinner(plotOutput('heatmap_tension_2', height = 300)))
          ))
    })
    
    output$psychology_ghq_box <- renderUI({
      div(style = 'position:relative;',
          box(
            height = 400,
            width = NULL,
            title = '',
            solidHeader = TRUE,
            icon_model('mental'),
            btn_question('question_ghq'),
            div(
              style = 'position: relative; top:-30px',
              withSpinner(plotOutput('psychology_ghq', height = 350))
            )
          )
      )
    })
    
    if(mobile) {
      output$mitigation_conversation <- renderPlot({ plot_psychology('number_people_talk', 'Number of people spoken today (not virtual)') })
      output$mitigation_household <- renderPlot({ plot_psychology('personal_change_behaviour_household', 'Change in behaviour at home') })
      output$mitigation_childcare <- renderPlot({ plot_psychology('childcare', 'How have you arrange childcare today?') })
      output$mitigation_public <- renderPlot({ plot_psychology('personal_change_behaviour_public', 'Change in behaviour at public places') })
      output$mitigation_work <- renderPlot({ plot_psychology('personal_change_behaviour_work', 'Change in behaviour at work') })
      output$mitigation_homework <- renderPlot({ plot_psychology('work condition', 'Current working conditions') })
      
      output$mitigation_contact_province <- renderPlot({ plot_psychology('avg_contact', 'avg contacts by province') +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))})
    } else {
      output$mitigation_conversation <- renderPlot({ plot_psychology('number_people_talk', 'Number of people spoken today (not virtual)', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      output$mitigation_household <- renderPlot({ plot_psychology('personal_change_behaviour_household', 'Change in behaviour at home', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      output$mitigation_childcare <- renderPlot({ plot_psychology('childcare', 'How have you arrange childcare today?', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      output$mitigation_public <- renderPlot({ plot_psychology('personal_change_behaviour_public', 'Change in behaviour at public places', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      output$mitigation_work <- renderPlot({ plot_psychology('personal_change_behaviour_work', 'Change in behaviour at work', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      output$mitigation_homework <- renderPlot({ plot_psychology('work condition', 'Current working conditions', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) })
      
      output$mitigation_contact_province <- renderPlot({ plot_psychology('avg_contact', 'avg contacts by province', input$mitigation_option_approach, input$mitigation_option_group,input$mitigation_option_filter_var, input$mitigation_option_filter_var_group, input$mitigation_option_survey) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))})
    }
    
    
    
    
    
    
    output$mitigation_fysical_contact<- renderPlot({
      set.seed(1)
      simul <- rbind(
        data.frame(category = '0-17 year',
                   date = rep(fysical_contact_probabilities %>% 
                         filter(category == '0-17 year') %>% pull(date),
                       fysical_contact_probabilities %>% 
                         filter(category == '0-17 year') %>% pull(prob) * 2000)),
        data.frame(category = '18-65 year',
                   date = rep(fysical_contact_probabilities %>% 
                         filter(category == '18-65 year') %>% pull(date),
                       fysical_contact_probabilities %>% 
                         filter(category == '18-65 year') %>% pull(prob) * 2000)),
        data.frame(category = '65+ year',
                   date = rep(fysical_contact_probabilities %>% 
                         filter(category == '65+ year') %>% pull(date),
                       fysical_contact_probabilities %>% 
                         filter(category == '65+ year') %>% pull(prob) * 2000)))
        
      ggplot(simul) +
        plot_style +
        geom_line(aes(date, color = category), adjust = 3, na.rm = TRUE, stat = 'density') +
        geom_vline(aes(xintercept = as.Date('2020-03-11'), linetype = 'dashed')) +
        geom_vline(aes(xintercept = as.Date('2020-03-13'), linetype = 'dashed')) +
        geom_vline(aes(xintercept = as.Date('2020-03-17'), linetype = 'dashed')) +
        scale_x_date(labels = date_format("%d/%m")) +
        xlab('date') +
        scale_linetype_manual('', values='dashed', label = translate('Measures')) +
        scale_colour_manual('', values = c(ggblue, gggreen, ggorange), label = translate(c('0-17 year', '18-65 year', '65+ year'))) +
        ggtitle(translate('last fysical contact (handshake or kiss) outside household'))
      
    })
    
    plot_psychology <- function(variable, 
                                title,
                                method = 'Model predictions',
                                grouping = 'Survey',
                                filter_var = '---', 
                                filter_group = 'All',
                                filter_enquete = 'All') {
      
      if(method == 'Direct statistics') {
        weighting <- 'uniform_weight'
      } else {
        weighting <- 'weight2'
      }
      
      var <- variable
      
      if(grouping == 'Survey') {
        if(filter_var != '---' & filter_group != 'All'){
          var <- paste0(var, '_', filter_group)
        }
        
        df <- aggregated %>% filter(variable %in% var,
                                    weight == weighting,
                                    survey != 1)
        
        ggplot(df) +
          plot_style +
          geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
          ylab(translate('percentage')) + xlab('') +
          scale_fill_manual('', values = colors[unique(df$survey)], label = dates[unique(df$survey)]) +
          ggtitle(translate(title))
      } else {
        lvl <- tail(filter_var_groups[[grouping]], -1)
        
        var <- paste0(var, '_', lvl)
        len <- str_length(variable)
        data <- aggregated %>%
          filter(survey == filter_enquete,
                 weight == weighting,
                 variable %in% var) %>%
          mutate(category = substr(variable, len+2, str_length(variable)))
        
        data$category <- factor(translate(data$category), translate(lvl))
        
        ggplot(data) +
          plot_style +
          geom_bar(aes(x = reorder(translate(label), order), y = value, fill = category), stat = 'identity', position = 'dodge2') +
          ylab(translate('percentage')) + xlab('') +
          scale_fill_manual('', values = colors) +
          ggtitle(translate(title))

      }
    }
    
    # plot_psychology <- function(variable, 
    #                             title,
    #                             method = 'Model predictions',
    #                             grouping = 'Survey',
    #                             group_age = 'All', 
    #                             group_gender = 'All',
    #                             group_enquete = 'All') {
    #   
    #   
    #   
    #   var <- variable
    #   if(group_age != 'All' & grouping != 'Age') {
    #     var <- paste0(var, '_', group_age)
    #   }
    #   
    #   if(grouping == 'Age') {
    #     var <- paste0(var, '_', c('0-17 year', '18-65 year', '65+ year'))
    #   }
    #   
    #   if(group_gender != 'All' & grouping != 'Gender') {
    #     var <- paste0(var, '_', group_gender)
    #   }
    #   
    #   if(grouping == 'Gender') {
    #     var <- paste0(var, '_', c('Man', 'Vrouw'))
    #   }
    # 
    #   if(method == 'Direct statistics') {
    #     weighting <- 'uniform_weight'
    #   } else {
    #     weighting <- 'weight2'
    #   }
    #   
    #   data <- aggregated %>% filter(variable %in% var,
    #                                 weight == weighting,
    #                                 survey != 1)
    #   
    #   if(grouping != 'Survey') {
    #     data <- data %>%
    #       filter(survey == group_enquete)
    #   }
    #   
    #   if(grouping == 'Age') {
    #     category <- case_when(str_detect(data$variable, '0-17 year') ~ '0-17 year',
    #               str_detect(data$variable, '18-65 year') ~ '18-65 year',
    #               str_detect(data$variable, fixed('65+ year')) ~ '65+ year')
    #     
    #     ggplot(data) +
    #       plot_style +
    #       geom_bar(aes(x = reorder(translate(label), order), y = value, fill = translate(category)), stat = 'identity', position = 'dodge2') +
    #       ylab(translate('percentage')) + xlab('') +
    #       scale_fill_manual('', values = c(ggblue, ggorange, gggreen)) +
    #       ggtitle(translate(title))
    #   } else if(grouping == 'Gender') {
    #     category <- case_when(str_detect(data$variable, 'Man') ~ 'Male',
    #                           str_detect(data$variable, 'Vrouw') ~ 'Female')
    #     
    #     ggplot(data) +
    #       plot_style +
    #       geom_bar(aes(x = reorder(translate(label), order), y = value, fill = translate(category)), stat = 'identity', position = 'dodge2') +
    #       ylab(translate('percentage')) + xlab('') +
    #       scale_fill_manual('', values = c(ggblue, ggorange)) +
    #       ggtitle(translate(title))
    #   } else if(grouping == 'Survey') {
    #     ggplot(data) +
    #       plot_style +
    #       geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
    #       ylab(translate('percentage')) + xlab('') +
    #       scale_fill_manual('', values = colors[2:6], label = dates[2:6]) +
    #       ggtitle(translate(title))
    #   }
    # }
    
    if(mobile) {
      output$psychology_sleep <- renderPlot({ plot_psychology('sleep', 'Sleep quality last week') })
      output$psychology_concentrate <- renderPlot({ plot_psychology('concentrate', 'Concentration last week') })
      output$psychology_optimistic <- renderPlot({ plot_psychology('optimistic', 'Optimism last week') })
      output$psychology_tension <- renderPlot({ plot_psychology('tension_family', 'Tension last week') })
      
      output$psychology_ghq <- renderPlot({ plot_psychology('ghq', 'ghq_title') +
          xlab('GHQ12') +
          annotate("text", x = 8, y = 0.27, label = translate('mean precovid'), size = 5) })
      
    } else {
      output$psychology_sleep <- renderPlot({ plot_psychology('sleep', 'Sleep quality last week', input$psychology_option_approach, input$psychology_option_group,input$psychology_option_filter_var, input$psychology_option_filter_var_group, input$psychology_option_survey) })
      output$psychology_concentrate <- renderPlot({ plot_psychology('concentrate', 'Concentration last week', input$psychology_option_approach, input$psychology_option_group,input$psychology_option_filter_var, input$psychology_option_filter_var_group, input$psychology_option_survey) })
      output$psychology_optimistic <- renderPlot({ plot_psychology('optimistic', 'Optimism last week', input$psychology_option_approach, input$psychology_option_group,input$psychology_option_filter_var, input$psychology_option_filter_var_group, input$psychology_option_survey) })
      output$psychology_tension <- renderPlot({ plot_psychology('tension_family', 'Tension last week', input$psychology_option_approach, input$psychology_option_group,input$psychology_option_filter_var, input$psychology_option_filter_var_group, input$psychology_option_survey) })
      
      output$psychology_ghq <- renderPlot({ plot_psychology('ghq', 'ghq_title', input$psychology_option_approach, input$psychology_option_group,input$psychology_option_filter_var, input$psychology_option_filter_var_group, input$psychology_option_survey) +
          xlab('GHQ12') +
          annotate("text", x = 8, y = 0.27, label = translate('mean precovid'), size = 5) })
      
    }
    
    output$symptom <- renderPlot({
      
      ggplot(aggregated %>% filter(variable == 'symptom',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors[2:6], label = dates[2:6]) +
        ggtitle(translate('Percantage of participants reporting COVID19 symptoms'))
      
    })
    
    plot_heatmap_tension <- function(survey_number) {
      data <- aggregated %>% 
        filter(str_detect(variable, 'hm_tension_'),
               weight == 'uniform_weight',
               survey == survey_number) 
      
      data$variable <- factor(data$variable, 
                              c('hm_tension_child', 'hm_tension_adult', 'hm_tension_70plus'),
                              translate(c('0-17 year', '18-70 year', '70+ year')))
      
      data$label <- factor(data$label,
                           c('0-17 year', '18-70 year', '70+ year'),
                           translate(c('0-17 year', '18-70 year', '70+ year')))
      
      data$percentage = round(data$value * 100, 2)
      ggplot(data) +
        plot_style +
        geom_tile(aes(x = variable, y = label, fill = percentage)) +
        geom_text(aes(x = variable, y = label, label = paste0(percentage, '%')), color = 'black') + 
        ylab(translate('Percentage people of age')) + xlab(translate('reporting increased tension with people of age')) +
        scale_fill_gradient(translate('percentage'), low = 'white', high = 'red', limits = c(0, 50)) +
        ggtitle(translate('Increased tension by age group'))
    }
    
    output$heatmap_tension_6 <- renderPlot({ plot_heatmap_tension(6) })
    output$heatmap_tension_5 <- renderPlot({ plot_heatmap_tension(5) })
    output$heatmap_tension_4 <- renderPlot({ plot_heatmap_tension(4) })
    output$heatmap_tension_3 <- renderPlot({ plot_heatmap_tension(3) })
    output$heatmap_tension_2 <- renderPlot({ plot_heatmap_tension(2) })
    
    plot_heatmap_symptom <- function(survey_number) {
      data <- aggregated %>% 
        filter(str_detect(variable, 'symptom_cor_'),
               weight == 'uniform_weight',
               survey == survey_number) 
      
      data$variable <- translate(substr(data$variable, 13, str_length(data$variable)))

      data$label <- reorder(factor(translate(data$label)), data$order)
      data$variable <- factor(data$variable, levels(data$label), levels(data$label))
      
      
      data$percentage = round(data$value * 100, 2)
      ggplot(data) +
        plot_style +
        geom_tile(aes(x = reorder(label, order), y = reorder(variable, order), fill = percentage)) + 
        ylab(translate('Percentage reporting symptom')) + xlab(translate('also reporting symptom')) +
        scale_fill_gradient(translate('percentage'), low = 'white', high = 'red', limits = c(0, 95), na.value = '#CCCCCC') +
        ggtitle(translate('Relation between reported symptoms')) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }

    output$society_topics_box <- renderUI({
      div(style = 'position:relative;',
          box(
            height = 400,
            width = NULL,
            title = '',
            solidHeader = TRUE,
            icon_observe(),
            btn_question('question_topics'),
            div(
              style = 'position: relative; top:-30px',
              withSpinner(plotOutput('society_topics', height = 350))
            )
          )
      )
    })
    
    output$society_topics <- renderPlot({ 
      ggplot(aggregated %>% filter(variable == 'message_topic',
                                   weight == 'uniform_weight')) +
        plot_style +
        geom_bar(aes(x = reorder(translate(label), order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors[2:6], label = dates[2:6]) +
        ggtitle(translate('About what do participants write in the comments?'))
    })
    
    output$society_financial_bar <- renderUI({
      div(style = 'position:relative;', 
          tabBox(
            title = translate("Financial situation participants"),
            id = "tabset1", 
            height = 400,
            width = NULL,
            tabPanel(translate("family size"), 
                     icon = icon('users'),
                     icon_observe(),
                     btn_question('question_financial_members'),
                     withSpinner(plotOutput('financial_members', height = 300))),
            tabPanel(translate("age"), 
                     icon = icon('birthday-cake'),
                     icon_observe(),
                     btn_question('question_financial_age'),
                     withSpinner(plotOutput('financial_age', height = 300))),
            tabPanel(translate("employment evolution"), 
                     icon = icon('building'),
                     icon_observe(),
                     btn_question('question_financial_employment_change'),
                     withSpinner(plotOutput('financial_employment_change', height = 300)))
          )
      )
    })
    
    plot_financial <- function(var) {
      data <- aggregated %>% 
        filter(str_detect(variable, paste0('financial_', var)),
               weight == 'uniform_weight',
               survey == 5)
      
      category <- case_when(str_detect(data$variable, 'Zeer gemakkelijk') ~ 'Very easy',
                            str_detect(data$variable, 'Gemakkelijk') ~ 'Easy',
                            str_detect(data$variable, 'Eerder moeilijk') ~ 'Rather difficult',
                            str_detect(data$variable, 'Moeilijk') ~ 'Difficult',
                            str_detect(data$variable, 'Eerder gemakkelijk') ~ 'Rather easy')
      category <- factor(translate(category), 
                         translate(c('Very easy', 'Easy', 'Rather easy', 'Rather difficult', 'Difficult')))
      
      ggplot(data) +
        plot_style +
        geom_bar(aes(x = category, y = value, fill = reorder(translate(label), order)), stat = 'identity', position = 'dodge2') +
        ylab(translate('percentage')) + xlab('') +
        scale_fill_manual('', values = colors[1:5])
    }
    
    output$financial_members <- renderPlot({ plot_financial('members') +
        ggtitle(translate('Financial situation by household size'))})
      
    output$financial_age <- renderPlot({ plot_financial('age_category') +
        ggtitle(translate('Financial situation by age group'))})
    
    output$financial_employment_change <- renderPlot({ plot_financial('employment_change') +
        ggtitle(translate('Financial situation by employment evolution'))})
    
    output$heatmap_symptom_6 <- renderPlot({ plot_heatmap_symptom(6) })
    output$heatmap_symptom_5 <- renderPlot({ plot_heatmap_symptom(5) })
    output$heatmap_symptom_4 <- renderPlot({ plot_heatmap_symptom(4) })
    output$heatmap_symptom_3 <- renderPlot({ plot_heatmap_symptom(3) })
    output$heatmap_symptom_2 <- renderPlot({ plot_heatmap_symptom(2) })
    
    output$symptom_map_5 <-renderLeaflet({ map_symptom(5, 'symptom5', input$select_province_5, input$select_legend_5)})
    #output$symptom_map_5 <-renderLeaflet({ map_symptom(5, 'symptom5', input$select_province_5, input$select_legend_5)})
    
    
    
  }
  
  #render_everything()

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
                          bins = c(-20, (-7:0))/1.5)
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addPolygons(
        data = data,
        weight = 2,
        smoothFactor = 0.5, 
        opacity = 1.0,
        fillOpacity = 0.7,
        color = 'gray',
        fillColor = ~color_pal(log(data$value)),
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            bringToFront = TRUE),
        label = paste0(translate(data$prov_nl),'\n',round(data$value*10000)/100, '%'))
  }
  
  observeEvent(input$map_survey_response, {
    if(input$map_survey_response == translate('April 21')) {
      output$survey_response_6 <- renderLeaflet({ map_response_freq(6) })
    }else if(input$map_survey_response == translate('April 14')) {
      output$survey_response_5 <- renderLeaflet({ map_response_freq(5) })
    }else if(input$map_survey_response == translate('April 7')) {
      output$survey_response_4 <- renderLeaflet({ map_response_freq(4) })
    } else if(input$map_survey_response == translate('March 31')) {
      output$survey_response_3 <- renderLeaflet({ map_response_freq(3) })
    } else if(input$map_survey_response == translate('March 24')) {
      output$survey_response_2 <- renderLeaflet({ map_response_freq(2) })
    } else if(input$map_survey_response == translate('March 17')) {
      output$survey_response_1 <- renderLeaflet({ map_response_freq(1) })
    }
    
  })

  
  observeEvent(input$map_symptom,{

    if(input$map_symptom == 'April 21') {
      if(active_map_symptom[6] != paste0(input$select_province_6, input$select_legend_6)) {
        output$symptom_map_6 <- renderLeaflet({ map_symptom(6, 'symptom6', input$select_province_6, input$select_legend_6)})
      }
    } else if(input$map_symptom == 'April 14') {
      if(active_map_symptom[5] != paste0(input$select_province_5, input$select_legend_5)) {
        output$symptom_map_5 <- renderLeaflet({ map_symptom(5, 'symptom5', input$select_province_5, input$select_legend_5)})
      }
    } else if(input$map_symptom == 'April 7') {
      if(active_map_symptom[4] != paste0(input$select_province_4, input$select_legend_4)) {
        output$symptom_map_4 <-renderLeaflet({ map_symptom(4, 'symptom4', input$select_province_4, input$select_legend_4)})
      }
    } else if(input$map_symptom == 'March 31') {
      if(active_map_symptom[3] != paste0(input$select_province_3, input$select_legend_3)) {
        output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3, input$select_legend_3)})
      }
    } else if(input$map_symptom == 'March 24') {
      if(active_map_symptom[2] != paste0(input$select_province_2, input$select_legend_2)) {
        output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2, input$select_legend_2)})
      }
    }
  })


  observeEvent(input$select_province_3, {
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_3)
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_3)
    updateSelectizeInput(session, "select_province_5", selected = input$select_province_3)
    updateSelectizeInput(session, "select_province_6", selected = input$select_province_3)

    if(active_map_symptom[3] != paste0(input$select_province_3, input$select_legend_3)) {
      output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_province_2, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_2)
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_2)
    updateSelectizeInput(session, "select_province_5", selected = input$select_province_2)
    updateSelectizeInput(session, "select_province_6", selected = input$select_province_2)

    if(active_map_symptom[2] != paste0(input$select_province_2, input$select_legend_2)) {
      output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_province_4, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_4)
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_4)
    updateSelectizeInput(session, "select_province_5", selected = input$select_province_4)
    updateSelectizeInput(session, "select_province_6", selected = input$select_province_4)

    if(active_map_symptom[4] != paste0(input$select_province_4, input$select_legend_4)) {
      output$symptom_map_4 <-renderLeaflet({ map_symptom(4, 'symptom4', input$select_province_4)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_province_5, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_5)
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_5)
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_5)
    updateSelectizeInput(session, "select_province_6", selected = input$select_province_5)

    if(active_map_symptom[5] != paste0(input$select_province_5, input$select_legend_5)) {
      output$symptom_map_5 <-renderLeaflet({ map_symptom(5, 'symptom5', input$select_province_5)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_province_6, {
    updateSelectizeInput(session, "select_province_3", selected = input$select_province_6)
    updateSelectizeInput(session, "select_province_2", selected = input$select_province_6)
    updateSelectizeInput(session, "select_province_4", selected = input$select_province_6)
    updateSelectizeInput(session, "select_province_5", selected = input$select_province_6)

    if(active_map_symptom[6] != paste0(input$select_province_6, input$select_legend_6)) {
      output$symptom_map_6 <-renderLeaflet({ map_symptom(6, 'symptom6', input$select_province_6)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_legend_2, {
    updateSelectizeInput(session, "select_legend_4", selected = input$select_legend_2)
    updateSelectizeInput(session, "select_legend_3", selected = input$select_legend_2)
    updateSelectizeInput(session, "select_legend_5", selected = input$select_legend_2)
    updateSelectizeInput(session, "select_legend_6", selected = input$select_legend_2)

    if(active_map_symptom[2] != paste0(input$select_province_2, input$select_legend_2)) {
      output$symptom_map_2 <-renderLeaflet({ map_symptom(2, 'symptom2', input$select_province_2, input$select_legend_2)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_legend_3, {
    updateSelectizeInput(session, "select_legend_4", selected = input$select_legend_3)
    updateSelectizeInput(session, "select_legend_2", selected = input$select_legend_3)
    updateSelectizeInput(session, "select_legend_5", selected = input$select_legend_3)
    updateSelectizeInput(session, "select_legend_6", selected = input$select_legend_3)

    if(active_map_symptom[3] != paste0(input$select_province_3, input$select_legend_3)) {
      output$symptom_map_3 <-renderLeaflet({ map_symptom(3, 'symptom3', input$select_province_3, input$select_legend_3)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_legend_4, {
    updateSelectizeInput(session, "select_legend_3", selected = input$select_legend_4)
    updateSelectizeInput(session, "select_legend_2", selected = input$select_legend_4)
    updateSelectizeInput(session, "select_legend_5", selected = input$select_legend_4)
    updateSelectizeInput(session, "select_legend_6", selected = input$select_legend_4)

    if(active_map_symptom[4] != paste0(input$select_province_4, input$select_legend_4)) {
      output$symptom_map_4 <-renderLeaflet({ map_symptom(4, 'symptom4', input$select_province_4, input$select_legend_4)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_legend_5, {
    updateSelectizeInput(session, "select_legend_3", selected = input$select_legend_5)
    updateSelectizeInput(session, "select_legend_2", selected = input$select_legend_5)
    updateSelectizeInput(session, "select_legend_4", selected = input$select_legend_5)
    updateSelectizeInput(session, "select_legend_6", selected = input$select_legend_5)

    if(active_map_symptom[5] != paste0(input$select_province_5, input$select_legend_5)) {
      output$symptom_map_5 <-renderLeaflet({ map_symptom(5, 'symptom5', input$select_province_5, input$select_legend_5)})
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_legend_6, {
    updateSelectizeInput(session, "select_legend_3", selected = input$select_legend_6)
    updateSelectizeInput(session, "select_legend_2", selected = input$select_legend_6)
    updateSelectizeInput(session, "select_legend_4", selected = input$select_legend_6)
    updateSelectizeInput(session, "select_legend_5", selected = input$select_legend_6)

    if(active_map_symptom[6] != paste0(input$select_province_6, input$select_legend_6)) {
      output$symptom_map_6 <-renderLeaflet({ map_symptom(6, 'symptom6', input$select_province_6, input$select_legend_6)})
    }
  }, ignoreInit = TRUE)
 
  # c(input$q_bar_response, input$q_bar_age, input$q_bar_gender, input$q_bar_education)
  
  question <- function(title, page) {
    showModal(modalDialog(
      easyClose = TRUE,
      title = translate(title),
      includeHTML(paste0('information/', translate(page))),
      footer = NULL
    ))
  }
  
  observeEvent(c(input$q_bar_response, input$q_bar_gender, input$q_bar_age, input$q_bar_education), {
    active <- any(map_lgl(c(input$q_bar_response, input$q_bar_gender, input$q_bar_age, input$q_bar_education), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Participants coronastudy', 'info_survey.html')
  }, ignoreInit = TRUE)
 
  observeEvent(c(input$question_map_symptom5, input$question_map_symptom4, input$question_map_symptom3, input$question_map_symptom2), {
    active <- any(map_lgl(c(input$question_map_symptom5, input$question_map_symptom4, input$question_map_symptom3, input$question_map_symptom2), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Map symptoms', 'info_map_symptoms.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_map_response6, input$question_map_response5, input$question_map_response4, input$question_map_response3, input$question_map_response2, input$question_map_response1), {
    active <- any(map_lgl(c(input$question_map_response6, input$question_map_response5, input$question_map_response4, input$question_map_response3, input$question_map_response2, input$question_map_response1), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Map participants', 'info_map_participants.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_symptom_all, input$question_symptom1, input$question_symptom2, input$question_symptom3, input$question_symptom4,
                 input$question_symptom5, input$question_symptom6, input$question_symptom7, input$question_symptom8,
                 input$question_symptom9, input$question_symptom10, input$question_symptom11, input$question_symptom12,
                 input$question_symptom13), {
    active <- any(map_lgl(c(input$question_symptom_all, input$question_symptom1, input$question_symptom2, input$question_symptom3, input$question_symptom4,
                            input$question_symptom5, input$question_symptom6, input$question_symptom7, input$question_symptom8,
                            input$question_symptom9, input$question_symptom10, input$question_symptom11, input$question_symptom12,
                            input$question_symptom13), function(x){x > 0}))
    validate(need(active, ''))
    
    question('symptoms', 'info_symptom_bar.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_symptom_heatmap6, input$question_symptom_heatmap5, input$question_symptom_heatmap4, input$question_symptom_heatmap3, input$question_symptom_heatmap2), {
    active <- any(map_lgl(c(input$question_symptom_heatmap6, input$question_symptom_heatmap5, input$question_symptom_heatmap4, input$question_symptom_heatmap3, input$question_symptom_heatmap2), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Symptom clusters', 'info_symptom_heatmap.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_mitigation_work, input$question_mitigation_bework, input$question_mitigation_behome, input$question_mitigation_bepublic,
                 input$question_mitigation_conversation, input$question_mitigation_childcare), {
                
    active <- any(map_lgl(c(input$question_mitigation_work, input$question_mitigation_bework, input$question_mitigation_behome, input$question_mitigation_bepublic,
        input$question_mitigation_conversation, input$question_mitigation_childcare), function(x){x > 0}))
                   
    validate(need(active, ''))
    
    question('Social distancing', 'info_mitigation.html')
  }, ignoreInit = TRUE)
  
  observeEvent(input$question_mitigation_fysical, {
    question('Last fysical contact', 'info_last_fysical_contact.html')
  })
  
  observeEvent(input$question_mitigation_contact_province, {
    question('Average number of contacts by province', 'info_avg_contact.html')
  })
  
  observeEvent(c(input$question_psychology_optimistic, input$question_psychology_sleep, input$question_psychology_tension, input$question_psychology_concentrate), {
    active <- any(map_lgl(c(input$question_psychology_optimistic, input$question_psychology_sleep, input$question_psychology_tension, input$question_psychology_concentrate), function(x){x > 0}))
    
    validate(need(active, ''))
   
   question('Mental health', 'info_psychology.html')
 }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_tension_heatmap6, input$question_tension_heatmap5, input$question_tension_heatmap4, input$question_tension_heatmap3, input$question_tension_heatmap2), {
    active <- any(map_lgl(c(input$question_tension_heatmap6, input$question_tension_heatmap5, input$question_tension_heatmap4, input$question_tension_heatmap3, input$question_tension_heatmap2), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Intergenerational tensions', 'info_tension_heatmap.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_ghq), {
    validate(need(input$question_ghq > 0, ''))
    
    question('ghq', 'info_ghq.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_symptom_spider), {
    validate(need(input$question_symptom_spider > 0, ''))
    
    question('radarchart', 'info_radarchart.html')
  }, ignoreInit = TRUE)
  
  
  
  observeEvent(c(input$question_financial_members, input$question_financial_age, input$financial_employment_change), {
    active <- any(map_lgl(c(input$question_financial_members, input$question_financial_age, input$financial_employment_change), function(x){x > 0}))
    
    validate(need(active, ''))
    
    question('Financial situation participants', 'info_financial_situation.html')
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$question_topics), {
    validate(need(input$question_topics > 0, ''))
    
    question('comments participants', 'info_topics.html')
  }, ignoreInit = TRUE)
  
  
  # val <- c('symptom_bar', 'symptom_heatmap', 'mitigation', 'psychology', 'ghq', 'tension_heatmap')
  # for(i in 1:length(val)) {
  #   file.create(paste0('information/info_', val[i], '_nl.html'))
  #   file.create(paste0('information/info_', val[i], '_en.html'))
  # }
   
  # val <- c('avg_contact', 'last_fysical_contact', 'topics', 'financial_situation')
  # for(i in 1:length(val)) {
  #   file.create(paste0('information/info_', val[i], '_nl.html'))
  #   file.create(paste0('information/info_', val[i], '_en.html'))
  # }
  
  observeEvent(input$about, {
    validate(need(input$about != 0, ''))
    
    about()
  })
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  ggblue <- "#56B4E9";#gg_co
  ggorange <- gg_color_hue(2)[1];
  ggred <- scales::hue_pal()(1)
  ggpurple <- "#796BD6"
  gggreen <- "#A3D596"
  
  setup <- function() {
    about()
    
    map_predict <<- read_sf('shapefiles/predict_nis/shapefile_predict_nis.shp', quiet = TRUE)
    
    map_province <<- map_predict %>%
      group_by(prov_nl) %>%
      summarise()
    
    load('data/aggregated_data.RData')
    load('data/fysical_contact_probabilities.RData')
    
    aggregated <<- aggregated
    fysical_contact_probabilities <<- fysical_contact_probabilities
    
    if(!mobile) {
      hide('tab_participants')
      hide('tab_mitigation')
      hide('tab_mentalhealth')
      hide('tab_society')
    }
    
    var <- c(map_predict$symptom2, map_predict$symptom3, map_predict$symptom4, map_predict$symptom5, map_predict$symptom6)
    
    breaks_map_symptom <<- c(round(min(var),4)-0.0001,
                            round(as.numeric(quantile(var, probs = seq(0, 1, 1/6))),4)[2:6],
                            round(max(var),4)+0.0001)
    
    active_map_symptom <<- rep('', 6)
  }
  
  observe({
    input$dimension
    
    if(length(input$dimension) == 2) {
      
      if(startup) {
        if(input$dimension[1] < 1100) {
          
          if(!mobile) {
            session$sendCustomMessage("go_mobile", "go_mobile")
          } else {
            setup()
            render_everything()
          }
          
          
          # showModal(modalDialog(
          #   easyClose = TRUE,
          #   title = translate('Corona Survey'),
          #   size = 'm',
          #   includeHTML('information/warning_small_screen.html'),
          #   footer = NULL
          # ))
          
        } else {
          setup()
          render_everything()
        }
      } 
      
      startup <<- FALSE
      
      # if(input$dimension[1] < 1100) {
      #   large_screen <<- FALSE
      #   shinyjs::hide('logos')
      #   updateButton(session, 'participants', label = '')
      #   updateButton(session, 'epidemiology', label = '')
      #   updateButton(session, 'mitigation', label = '')
      #   updateButton(session, 'mentalhealth', label = '')
      #   updateButton(session, 'society', label = '')
      # } else {
      #   large_screen <<- TRUE
      #   shinyjs::show('logos')
      #   updateButton(session, 'participants', label = paste0('&nbsp;&nbsp;',translate('Participants')))
      #   updateButton(session, 'epidemiology', label = paste0('&nbsp;&nbsp;',translate('Epidemiology')))
      #   updateButton(session, 'mitigation', label = paste0('&nbsp;&nbsp;',translate('Social distancing')))
      #   updateButton(session, 'mentalhealth', label = paste0('&nbsp;&nbsp;',translate('Mental health')))
      #   updateButton(session, 'society', label = paste0('&nbsp;&nbsp;',translate('Society')))
      # }
    }
  })
  
  #setup()
}
# rsconnect::deployApp(appTitle='publicapp_mobile', account = 'jonas-crevecoeur', server='shinyapps.io')
# rsconnect::deployApp(appTitle='publicapp', account = 'jonas-crevecoeur', server='shinyapps.io')
# rsconnect::deployApp(appTitle='corona-studie', account = 'corona-studie', server='shinyapps.io')
# rsconnect::deployApp(appTitle='corona-studie-mobile', account = 'corona-studie', server='shinyapps.io')
shinyApp(ui = ui, server = server)

#preview_mobile(appPath, device = "iphoneX")
