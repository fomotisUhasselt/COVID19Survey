### load libraries here
source("RScripts/DataProcessing.R")
source("RScripts/mergeSurvey.R")
source("RScripts/EDA.R")
source("RScripts/spatial.R")
source("RScripts/multivariate.R")



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
           
            absolutePanel(
              id = "controls", class = "panel panel-default",
                         top = 80, left = 40, width = 300, fixed=TRUE,
                         draggable = TRUE, height = "auto",
                         # Input Survey Round
                         selectInput("surveyId", 
                                     label = "Survey Round", 
                                     choices = c("First" = 1, 
                                                 "Second" = 2, 
                                                 "Third" = 3,
                                                 "Fourth" = 4), 
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
                                         "Third" = 3, 
                                         "Fourth" = 4), 
                             selected = "First",
                             multiple = FALSE),
                 
                 selectInput("question_select", 
                             label = "Survey Questions: Variable One",   
                             choices = textOutput("questions"),
                             multiple = FALSE),
                 
                 selectInput("question_select", 
                             label = "Survey Questions: Variable Two",   
                             choices = textOutput("questions"),
                             multiple = FALSE),
                 
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
    tabPanel("Merging Surveys",
             
             sidebarLayout(
               sidebarPanel(
                 
                 pickerInput("survey_select", 
                             label = "Select survey rounds to merge:", 
                             choices = c("First" = 1, 
                                         "Second" = 2, 
                                         "Third" = 3, 
                                         "Fourth" = 4), 
                             selected = c("Second" = 2, "Third" = 3),
                             multiple = TRUE)
               ),
               
               mainPanel(
                 
                  DT::dataTableOutput("merged_survey"),
                  downloadButton("downloadMergedSurvey", "Download as CSV")
                 
               )
             )
             
             
    ),
  
    #Third panel for pattern and clustering
    tabPanel("Patterns and Clusters"),
  
    #fourth panel for spatial data analysis
    tabPanel(
      "Spatial Analysis",
      tags$head(
        tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
      ),
      sidebarLayout(
        sidebarPanel(
          
          selectInput("surveyID_spatial", 
                      label = "Survey Round", 
                      choices = c("First" = 1, 
                                  "Second" = 2, 
                                  "Third" = 3, 
                                  "Fourth" = 4), 
                      selected = "First",
                      multiple = FALSE),
          
          selectInput("provinceID", 
                      label = "Province", 
                      choices = unique(belgium_gemeente$Prov_nl), 
                      multiple = FALSE),
          selectInput("regionID", 
                      label = "Region", 
                      choices = unique(belgium_gemeente$Reg_nl), 
                      multiple = FALSE)
          
        ),
        mainPanel(
          
          leafletOutput("spcarmap", width = "100%", height = 800)
          
        )
      ),
      
    ),
  
    tabPanel("Contact Data Analysis"),
  
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
      
      qsts_selected <- reactive_question()$Question[which(reactive_question()$Survey_Questions %in% 
                                                            input$question_select)]
      
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
  
  ################## The Data merging tab
  
  reactive_survey_select <- reactive({
    
      poss <- as.numeric(unlist(input$survey_select))
      
      merge_surveys(survey_list_orig[poss])
   })
  
  output$merged_survey <- DT::renderDataTable({
    
    DT::datatable(reactive_survey_select())
    
    }, extensions = "Buttons", options = list(dom = 'Bfrtip', ordering = FALSE, searching = TRUE, 
                                              scrollY="300px",scrollCollapse = TRUE,
                                              paging = FALSE, scrollX = TRUE,
                                              buttons = list("csv","excel"))
    )
   
  output$downloadMergedSurvey <- downloadHandler(
    filename = function() {
      paste("Merged", "Survey.csv", sep = "_")
    },
    content = function(file) {
      write.csv(reactive_survey_select(), file)
    }
  )
  
  
  ################## The spatial Analysis tab
  
  reactive_sppred <- reactive({
    
      if(as.numeric(input$surveyID_spatial) == 1) {
      
        gr <- 2
      
      } else {
      
        gr <- as.numeric(input$surveyID_spatial)
      
      }
    
      pred.symp[[gr]]
    
    
    })
  
  output$spcarmap <- renderLeaflet({ 
    
    spatial_car_maps(pred = reactive_sppred(), nclr = 8, map.shp = map.shp)
    
  })
  
  
  
  
}



######## build the shiny app
shinyApp(ui = ui, server = server)
