library(shiny)
library(dplyr)
library(ggplot2)
library(shinydisconnect)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(tmap)
library(sf)

library(shinyjs)

shinyjs <- "shinyjs.closeWindow = function() { window.close(); }"

# Excel 

p=read.csv('police.csv')
pd=read.csv('vict.csv')

#shapefile

nz_police_district=st_read("nz-police-district-boundaries.shp",stringsAsFactors = FALSE)

levels(factor(pd$ANZSOC.Subdivision))
names(pd)

# Title 
name <- list(
  title = "Crime In New Zealand")

# setting  and cleaning the data 
f <- file.path('C:','Users','User','Desktop','shiny_r_final','vict.csv')
if (file.exists(f)) {
  pd <- read.csv(f, stringsAsFactors = FALSE)
  pd<-pd[!(pd$Age=="Not Applicable" | 
             pd$Age=="Not Specified"),]  }

application <- list(
  
  ui <- fluidPage(theme = shinytheme("slate"),themeSelector(),tags$head(tags$style(HTML(# changing the table text to grey so it can match with the different theme selector types e.g. intergrate with gray and white
    "
                  .dataTables_length label, 
                  .dataTables_filter label,
                  .dataTables_info {
                      color: grey!important;
                      }

                  .paginate_button {
                      background: grey!important;
                  }

                  thead {
                      color: grey;
                      }

                  "))),
    shinydisconnect::disconnectMessage2(),
    
    titlePanel("Crime In New Zealand"),
    sidebarLayout(
      sidebarPanel(
        br(),
        sliderInput("datePicker", # adding a date slider 
                    label="Observation Period:", 
                    min=2014,
                    max=2020,
                    value = c(2014,2020),
                    sep = ""
        ),
        uiOutput("OutputSelecttype"),
        checkboxInput("clean_district", "Subset by District", FALSE),
        conditionalPanel(
          condition = "outcome.clean_district",
          uiOutput("Outputdistrict"),
          span(tags$a("Open Crime Map",
                      href = "https://arcg.is/1zizrC0")) # creating my story map

        ),
        
      ),
      mainPanel(
        h3(textOutput("Textsummary")),
        downloadButton("install", "Download"),# arranging the download
        br(),
        plotOutput("plot"),
        br(),
        tmapOutput("map"),
        hr(),
        span("Data source:", # creating a hyperlink
             tags$a("Open Police Data",
                    href = "https://www.police.govt.nz/about-us/publications-statistics/data-and-statistics/policedatanz/victimisation-time-and-place")),
        br(), br(),
        tags$br(),
        DT::dataTableOutput("dates"),
        actionButton("exit", "EXIT", class = "btn-warning")# creating a put buttom
      )
    )
  ),
  
  server <- function(input, output, session) {
    
   
    output$Outputdistrict <- renderUI({
      selectInput("DistrictInput", "Police District",# creating a selection input
                  sort(unique(pd$Police.District)),
                  selected = "Auckland City")# creating a selection input
    })
    
    output$OutputSelecttype <- renderUI({
      selectInput("crimeInput", "Crime Categories",# creating a selection input
                  sort(unique(pd$ANZSOC.Subdivision)),
                  multiple = TRUE,
                  selected = c("Assault", "Robbery")) # creating a selection input
    })
     
    output$table <- DT::renderDT({
      pd
    })
    
    output$Textsummary <- renderText({
      numOptions <- nrow(dates())
      if (is.null(numOptions)) {
        numOptions <- 0
      }
      paste0(numOptions," Entries") # linking the Entries subset data
    })
    
    dates <- reactive({
      dates <- pd
      
      if (is.null(input$DistrictInput)) {
        return(NULL)
      }
      
      dates <- dplyr::filter(dates, ANZSOC.Subdivision %in% input$crimeInput) # cleaning out the date data set
      if (input$clean_district) {
        dates <- dplyr::filter(dates, Police.District == input$DistrictInput)
      }
      dates <- dplyr::filter(dates, Year >= input$datePicker[1],
                             Year <= input$datePicker[2])
      if(nrow(dates) == 0) {
        return(NULL)
      }
      dates
    })
    
    output$plot <- renderPlot({
      
      if (is.null(dates())) {
        return(NULL)
      }
      
      ggplot(dates(), aes(y=Victimisations, x = ANZSOC.Subdivision, fill= Age)) + # creating a boxplot
        geom_bar(stat="identity", position=position_dodge()) + theme(plot.background = element_rect(fill = "gray"),
                                                                     legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
    }, bg="transparent",execOnResize = TRUE)
    
      output$map=renderTmap({ # creating a map from tmap and sf library
        tm_shape(nz_police_district)+
        tm_borders() +
        tm_layout(main.title = "Study area", legend.outside = TRUE)+
        tm_dots("DISTRICT_N",size = 0.1)+
        tm_basemap("OpenStreetMap.Mapnik")+
        tmap_mode("view")
      
    })
    
    
    output$dates <- DT::renderDataTable({  #linking dates with the table
      dates()
    })
    
    output$install <- downloadHandler( # creating a download file name
      f  = function() {
        "Police_outcome.csv"
      },
      content = function(c1) {
        write.csv(dates(), c1)
      }
    )
    observeEvent(input$exit,  { stopApp("networkapp") ; js$closeWindow()})
  }  
)

networkapp <- list( # creating a window
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = shinyjs, functions = c("closeWindow")),
    fluidRow(
      column(1),
      mainPanel(h2("Find Crimes Across New Zealand"), 
                actionButton("application", "Select",class = "btn-danger"), 
      ))
  ),
  server = function(input, output, session) {
    observeEvent(input$application, {stopApp("application") ; js$closeWindow() })
  }
)

network <- "networkapp"
while (TRUE) 
{
  m <- switch(
    network,
    networkapp = shinyApp(networkapp$ui, networkapp$server),
    application = shinyApp(application$ui, application$server),
  )

    break
  }
   network <- print(m)
  message("New network ", network)

shinyApp(ui = ui, server = server)

