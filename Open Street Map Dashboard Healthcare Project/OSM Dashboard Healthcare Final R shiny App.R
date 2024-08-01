library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(httr)
library(osmdata)
library(sf)
library(purrr)
library(DT)
library(htmlwidgets)
library(htmltools)

options(jsonlite.vec_identical = TRUE)  # Suppress jsonlite warning

# Default user location in Wellington
default_user_latitude <- -41.2865
default_user_longitude <- 174.7762

# Function to get address from latitude and longitude
get_address <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", lat, "&lon=", lon, "&addressdetails=1")
  response <- GET(url)
  content <- content(response, "parsed")
  if (!is.null(content$address)) {
    address <- paste(content$address$road, content$address$city, content$address$state, content$address$country, sep = ", ")
    return(address)
  } else {
    return("Address not found")
  }
}

# Function to fetch OSM data for a given key and value
fetch_osm_data <- function(key, value, bbox) {
  tryCatch({
    opq(bbox) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf() %>%
      .$osm_points %>%
      st_as_sf() %>%
      select(osm_id, name, geometry) %>%
      filter(!is.na(name))  # Remove NA names
  }, error = function(e) {
    print(paste("Error fetching data:", e$message))
    return(NULL)
  })
}

# Bounding box for Wellington area
bbox <- c(174.738, -41.329, 174.836, -41.245)

# Fetch healthcare data
healthcare_data <- list(
  Hospital = fetch_osm_data("amenity", "hospital", bbox),
  Clinic = fetch_osm_data("amenity", "clinic", bbox),
  Doctor = fetch_osm_data("amenity", "doctors", bbox),
  Pharmacy = fetch_osm_data("amenity", "pharmacy", bbox),
  Dentist = fetch_osm_data("amenity", "dentist", bbox)
)

# Combine all healthcare data into one data frame
all_healthcare_data <- bind_rows(
  lapply(names(healthcare_data), function(type) {
    if (!is.null(healthcare_data[[type]])) {
      healthcare_data[[type]] %>% mutate(type = type)
    }
  })
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Healthcare Information Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      selectInput("travel_mode", "Travel Mode:", choices = c("Walk", "Bike", "Car")),
      checkboxInput("enable_distance_filter", "Enable Distance Filter", value = TRUE),
      sliderInput("distance_range", "Distance (km):", min = 0, max = 100, value = c(0, 3), step = 1),
      checkboxInput("enable_time_filter", "Enable Time Filter", value = TRUE),
      sliderInput("time_range", "Time (minutes):", min = 0, max = 120, value = c(0, 15), step = 1),
      checkboxInput("buffer_filter", "Enable Buffer Filtering", value = TRUE),
      sliderInput("buffer_distance", "Buffer Distance (km):", min = 0, max = 20, value = 3, step = 1),
      checkboxGroupInput("healthcare_type", "Select Healthcare Type:", 
                         choices = unique(all_healthcare_data$type), 
                         selected = "Hospital")
    )
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 300px) !important;}"), # Adjust map height
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Healthcare Map", status = "info", solidHeader = TRUE, width = 12,
                    leafletOutput("map", width = "100%", height = "calc(100vh - 300px)"),
                    textOutput("no_data_message")),
                box(title = "Information Table", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("info_table"),
                    downloadButton("download_table", "Download Table as CSV"))
              )
      )
    ),
    tags$script(
      HTML("
        Shiny.addCustomMessageHandler('update_map', function(data) {
          var map = HTMLWidgets.find('.leaflet');
          var latLng = new L.LatLng(data.lat, data.lng);
          map.setView(latLng, 12);
        });
      ")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Speed factors for different travel modes (in km/h)
  speed_factors <- list(
    Walk = 5,
    Bike = 15,
    Car = 50
  )
  
  # Reactive value to store user location
  user_location <- reactiveValues(lat = default_user_latitude, lng = default_user_longitude, address = get_address(default_user_latitude, default_user_longitude))
  
  # Function to calculate travel distance and time using OSRM
  calculate_travel_info <- function(origin_lat, origin_lng, dest_lat, dest_lng, travel_mode) {
    profile <- switch(travel_mode,
                      "Walk" = "foot",
                      "Bike" = "bike",
                      "Car" = "driving")
    
    url <- paste0("http://router.project-osrm.org/route/v1/", profile, "/",
                  origin_lng, ",", origin_lat, ";", dest_lng, ",", dest_lat, "?overview=false")
    
    print(paste("Travel Mode:", travel_mode))  # Debug statement to check the travel mode
    print(paste("Request URL:", url))  # Debug statement to check the URL
    
    response <- tryCatch({
      send_request <- GET(url)
      stop_for_status(send_request)
      content(send_request, "parsed")
    }, error = function(e) {
      print(paste("Error:", e$message))  # Debug statement for error handling
      NULL
    })
    
    if (!is.null(response)) {
      route_info <- response$routes[[1]]
      distance <- route_info$distance / 1000  # in km
      # Use the speed factor to adjust the duration
      duration <- distance / speed_factors[[travel_mode]] * 60  # in minutes
      print(paste("Mode:", travel_mode, "Distance (km):", round(distance, 2), "Duration (min):", round(duration, 2)))  # Log the parsed distance and duration
      return(list(distance = distance, duration = duration))
    } else {
      return(NULL)
    }
  }
  
  # Reactive expression to filter healthcare amenities
  filtered_healthcare <- reactive({
    req(input$healthcare_type)
    
    all_healthcare_data <- bind_rows(
      lapply(names(healthcare_data), function(type) {
        if (!is.null(healthcare_data[[type]])) {
          healthcare_data[[type]] %>% mutate(type = type)
        }
      })
    )
    
    all_healthcare_data <- all_healthcare_data %>%
      filter(type %in% input$healthcare_type) %>%
      mutate(latitude = st_coordinates(geometry)[, 2], longitude = st_coordinates(geometry)[, 1])
    
    dest_data <- all_healthcare_data %>%
      mutate(travel_info = map2(latitude, longitude, ~{
        info <- calculate_travel_info(user_location$lat, user_location$lng, .x, .y, input$travel_mode)
        info
      })) %>%
      filter(!map_lgl(travel_info, is.null))
    
    if (input$enable_distance_filter) {
      dest_data <- dest_data %>%
        filter(map_lgl(travel_info, ~.x$distance >= input$distance_range[1] & .x$distance <= input$distance_range[2]))
    }
    
    if (input$enable_time_filter) {
      dest_data <- dest_data %>%
        filter(map_lgl(travel_info, ~.x$duration >= input$time_range[1] & .x$duration <= input$time_range[2]))
    }
    
    if (input$buffer_filter) {
      buffer_distance <- input$buffer_distance
      dest_data <- dest_data %>%
        filter(map_lgl(travel_info, ~.x$distance <= buffer_distance))
    }
    
    dest_data
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = user_location$lng, lat = user_location$lat, zoom = 12) %>%
      addAwesomeMarkers(lng = user_location$lng, lat = user_location$lat,
                        label = "Your Location", popup = user_location$address, icon = makeAwesomeIcon(icon = "user", markerColor = "orange", iconColor = "black"), 
                        options = markerOptions(draggable = TRUE)) %>%
      addSearchOSM(options = searchOptions(position = "topleft", 
                                           autoType = FALSE, 
                                           minLength = 2)) %>%
      onRender("
        function(el, x) {
          var map = this;
          map.on('click', function(e) {
            var lat = e.latlng.lat;
            var lng = e.latlng.lng;
            Shiny.onInputChange('map_click', {lat: lat, lng: lng});
          });
        }
      ")
    
    if (input$buffer_filter) {
      map <- map %>%
        addCircles(lng = user_location$lng, lat = user_location$lat, 
                   radius = input$buffer_distance * 1000, 
                   color = "blue", 
                   fillColor = "blue", 
                   fillOpacity = 0.2)
    }
    
    if (nrow(filtered_healthcare()) == 0) {
      output$no_data_message <- renderText({"No healthcare amenities found within the filtered distance."})
    } else {
      output$no_data_message <- renderText({""})
      
      healthcare_with_travel_info <- filtered_healthcare() %>%
        mutate(
          distance = map_dbl(travel_info, "distance"),
          duration = map_dbl(travel_info, "duration")
        )
      
      map <- map %>%
        addMarkers(data = healthcare_with_travel_info, 
                   ~st_coordinates(geometry)[,1], 
                   ~st_coordinates(geometry)[,2], 
                   label = ~name, 
                   popup = ~paste0("<b>", name, "</b><br>Type: ", type, "<br>Distance: ", round(distance, 2), " km<br>Time: ", round(duration, 2), " min"),
                   clusterOptions = markerClusterOptions())
    }
    
    map
  })
  
  # Observe changes in user location marker
  observeEvent(input$map_marker_dragend, {
    user_location$lat <- input$map_marker_dragend$lat
    user_location$lng <- input$map_marker_dragend$lng
    user_location$address <- get_address(input$map_marker_dragend$lat, input$map_marker_dragend$lng)
    
    leafletProxy("map") %>% 
      clearPopups() %>% 
      addPopups(lng = user_location$lng, lat = user_location$lat, popup = user_location$address)
  }, ignoreInit = TRUE)
  
  # Add popup for clicked location
  observeEvent(input$map_click, {
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    address <- get_address(lat, lng)
    
    leafletProxy("map") %>% 
      addPopups(lng = lng, lat = lat, popup = address)
  }, ignoreInit = TRUE)
  
  # Render information table
  output$info_table <- renderDT({
    travel_data <- filtered_healthcare() %>%
      transmute(
        `Place Name` = name,
        `Healthcare Type` = type,
        `Distance (km)` = map_dbl(travel_info, "distance") %>% round(2),
        `Time (minutes)` = map_dbl(travel_info, "duration") %>% round(2)
      ) %>%
      head(3)
    
    datatable(travel_data, options = list(pageLength = 3, dom = 't'))
  })
  
  # Download handler for the information table
  output$download_table <- downloadHandler(
    filename = function() {
      paste("healthcare_info-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      travel_data <- filtered_healthcare() %>%
        transmute(
          `Place Name` = name,
          `Healthcare Type` = type,
          `Distance (km)` = map_dbl(travel_info, "distance") %>% round(2),
          `Time (minutes)` = map_dbl(travel_info, "duration") %>% round(2)
        )
      
      write.csv(travel_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

