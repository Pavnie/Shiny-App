
# Load necessary libraries
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
library(feather)
library(units)
library(RColorBrewer)

# JavaScript code for closing the Shiny app window
shinyjs_code <- "shinyjs.closeWindow = function() { window.close(); }"

# Load the datasets
police <- readRDS('police.rds')
pd <- readRDS('vict.rds')

# Load the updated shapefile and remove unnecessary columns
nz_police_district <- st_read("nz-police-district-boundaries-updated.shp", stringsAsFactors = FALSE) %>%
  rename(`Regional Council` = DISTRICT_N) %>%
  mutate(area = st_area(geometry) %>% set_units(km^2) %>% as.numeric())

# Assign colors to regions
region_colors <- RColorBrewer::brewer.pal(n = length(unique(nz_police_district$`Regional Council`)), name = "Set3")
names(region_colors) <- unique(nz_police_district$`Regional Council`)

# Clean the pd data
f <- file.path('C:', 'Users', 'Pawaneet Kaur', 'R', 'Desktop', 'Crime Shiny Project', 'vict.rds')
if (file.exists(f)) {
  pd <- readRDS(f) %>% filter(!Age %in% c("Not Applicable", "Not Specified"))
}

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  themeSelector(),
  tags$head(tags$style(HTML("
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
  useShinyjs(),
  extendShinyjs(text = shinyjs_code, functions = c("closeWindow")),
  
  titlePanel("Crime In New Zealand"),
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("datePicker", label = "Observation Period:", min = 2014, max = 2020, value = c(2018, 2020), sep = ""),
      uiOutput("OutputSelecttype"),
      checkboxInput("clean_district", "Subset by District", TRUE),
      uiOutput("Outputdistrict"),
      span(tags$a("Open Crime Map", href = "https://arcg.is/1zizrC0"))
    ),
    mainPanel(
      h3(textOutput("Textsummary")),
      downloadButton("install", "Download"),
      br(),
      plotlyOutput("plotly_plot"),
      br(),
      tmapOutput("map"),
      hr(),
      span("Data source:", tags$a("Open Police Data", href = "https://www.police.govt.nz/about-us/publications-statistics/data-and-statistics/policedatanz/victimisation-time-and-place")),
      br(), br(),
      tags$br(),
      DT::dataTableOutput("dates"),
      actionButton("exit", "EXIT", class = "btn-warning")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Output for district selection UI
  output$Outputdistrict <- renderUI({
    selectInput("DistrictInput", "Police District", sort(unique(pd$Police.District)), selected = "Auckland City")
  })
  
  # Output for crime category selection UI
  output$OutputSelecttype <- renderUI({
    selectInput("crimeInput", "Crime Categories", sort(unique(pd$ANZSOC.Subdivision)), multiple = TRUE, selected = c("Assault", "Robbery"))
  })
  
  # Output for data table
  output$table <- DT::renderDT({ pd })
  
  # Text summary output
  output$Textsummary <- renderText({
    numOptions <- nrow(dates())
    if (is.null(numOptions)) numOptions <- 0
    paste0(numOptions, " Entries")
  })
  
  # Reactive dates filtered by user input
  dates <- reactive({
    dates <- pd
    if (is.null(input$DistrictInput)) return(NULL)
    dates <- filter(dates, ANZSOC.Subdivision %in% input$crimeInput)
    if (input$clean_district) dates <- filter(dates, Police.District == input$DistrictInput)
    dates <- filter(dates, Year >= input$datePicker[1], Year <= input$datePicker[2])
    if (nrow(dates) == 0) return(NULL)
    dates
  })
  
  # Plotly plot output
  output$plotly_plot <- renderPlotly({
    if (is.null(dates())) return(NULL)
    p <- ggplot(dates(), aes(y = Victimisations, x = ANZSOC.Subdivision, fill = Age)) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      theme(plot.background = element_rect(fill = "gray"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"),
            plot.title = element_text(margin = margin(b = 20), size = 14))
    ggplotly(p) %>% layout(title = "Victimisations by Crime Category and Age", margin = list(t = 50))
  })
  
  # Tmap output
  output$map <- renderTmap({
    tm_shape(nz_police_district) +
      tm_borders() +
      tm_layout(main.title = "Study area", legend.outside = TRUE, legend.title.size = 1.5, legend.text.size = 1.2) +
      tm_dots(col = "Regional Council", size = 0.1, palette = region_colors, popup.vars = c("Regional Council" = "Regional Council", "Area (km²)" = "area")) +
      tm_basemap("OpenStreetMap.Mapnik") +
      tmap_mode("view")
  })
  
  # Data table output
  output$dates <- DT::renderDataTable({ dates() })
  
  # Download handler for CSV
  output$install <- downloadHandler(
    filename = function() { "Police_outcome.csv" },
    content = function(file) { write.csv(dates(), file) }
  )
  
  # Close window action
  observeEvent(input$exit, { stopApp("networkapp"); js$closeWindow() })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
