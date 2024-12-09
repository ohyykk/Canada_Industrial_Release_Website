# load libraries
library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(readr)
library(here)
library(rmapshaper)

# load data
province_data <- readRDS("province_data.rds")
provinces_with_data <- readRDS("provinces_with_data.rds")
aggregated_data <- readRDS("aggregated_data.rds")
scatter_data_sf <- readRDS("scatter_data_sf.rds")

# Convert total_quantity from tons to ktons
provinces_with_data <- provinces_with_data %>% 
  mutate(total_quantity_ktons = total_quantity / 1000)

# Calculate the average total emission across all provinces (in ktons)
average_ktons <- mean(provinces_with_data$total_quantity_ktons, na.rm = TRUE)

# Province name lookup
province_names <- c(
  "AB" = "Alberta",
  "BC" = "British Columbia",
  "MB" = "Manitoba",
  "NB" = "New Brunswick",
  "NL" = "Newfoundland and Labrador",
  "NS" = "Nova Scotia",
  "NT" = "Northwest Territories",
  "NU" = "Nunavut",
  "ON" = "Ontario",
  "PE" = "Prince Edward Island",
  "QC" = "Quebec",
  "SK" = "Saskatchewan",
  "YT" = "Yukon"
)

# Define bounding box for Canada
canada_bounds <- list(
  lng1 = -141,  # Westernmost longitude
  lat1 = 41,    # Southernmost latitude
  lng2 = -52,   # Easternmost longitude
  lat2 = 83     # Northernmost latitude
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        background: url('5.jpg') no-repeat center center fixed;
        background-size: cover;
        position: relative;
      }

      body::before {
        content: '';
        position: fixed;
        top: 0;
        right: 0;
        bottom: 0;
        left: 0;
        background: rgba(255, 255, 255, 0.65);
        z-index: 0;
      }

      .container-fluid {
        position: relative;
        z-index: 1;
      }

      #loading-message {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 1.2rem;
        color: #4a5568;
        background: rgba(255, 255, 255, 0.9);
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        z-index: 1000;
      }
    "))
  ),
  
  tags$div(id = "loading-message", "Please wait while the map is loading..."),
  
  tags$script("
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('hideLoading', function(message) {
        $('#loading-message').fadeOut();
      });
    });
  "),
  
  fluidRow(
    # First column: Map
    column(
      width = 7, offset = 1,
      style = "padding: 20px;",
      leafletOutput("map", height = "90vh")
    ),
    # Second column: Info boxes
    column(
      width = 4,
      # Add a scrollable container
      div(
        style = "max-height:90vh; overflow-y:auto; padding: 20px;",
        
        # Province info box
        tags$div(
          id = "province-info-box",
          style = "padding: 10px; border: 1px solid #ccc; background: #f9f9f9; margin-bottom: 20px;",
          tags$p("Province: ", tags$span(uiOutput("province_name", inline = TRUE))),
          tags$p("Total Emission (ktons): ", tags$span(uiOutput("province_emission", inline = TRUE))),
          tags$p("Difference from Average (ktons): ", tags$span(uiOutput("province_diff", inline = TRUE)))
        ),
        
        # Scatter info box
        tags$div(
          id = "scatter-info-box",
          style = "padding: 10px; border: 1px solid #ccc; background: #f9f9f9;",
          tags$p("Facility: ", tags$span(uiOutput("scatter_facility", inline = TRUE))),
          tags$p("Substance Info: ", tags$span(uiOutput("scatter_substance", inline = TRUE)))
        )
      )
    )
  ),
  # Help button in the top-left corner
  absolutePanel(
    top = 10, left = 10, style = "z-index:500;",
    actionButton("help_button", label = NULL, icon = icon("question-circle"), 
                 style = "background-color: #fff; border: 1px solid #ccc; border-radius: 50%; width:40px; height:40px;")
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive values to store info for province and scatter points
  selected_province <- reactiveValues(name = "No province selected", emission = "N/A", diff = "N/A")
  selected_scatter <- reactiveValues(facility = "No facility selected", substance = "N/A")
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 3)) %>%  # Set minimum zoom level
      addTiles() %>%
      setView(lng = -95, lat = 60, zoom = 3) %>%  # Default view
      setMaxBounds(
        lng1 = canada_bounds$lng1,
        lat1 = canada_bounds$lat1,
        lng2 = canada_bounds$lng2,
        lat2 = canada_bounds$lat2
      ) %>%
      addPolygons(
        data = provinces_with_data,
        fillColor = ~colorNumeric("Greys", total_quantity)(total_quantity),
        color = "white", weight = 1, fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
        layerId = ~PREABBR  # Assign unique IDs for provinces
      ) %>%
      addLayersControl(
        overlayGroups = c("scatter"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        pal = colorNumeric("Greys", domain = provinces_with_data$total_quantity),
        values = provinces_with_data$total_quantity,
        position = "bottomright",
        title = "Total Emissions (ktons)"
      )
    
    # Hide loading message after map is created
    session$sendCustomMessage("hideLoading", TRUE)
    
    map
  })
  
  # Update the province info box dynamically
  output$province_name <- renderUI({ HTML(selected_province$name) })
  output$province_emission <- renderUI({HTML(selected_province$emission)})
  output$province_diff <- renderUI({ HTML(selected_province$diff) })
  
  # Update the scatter info box dynamically
  output$scatter_facility <- renderUI({ HTML(selected_scatter$facility) })
  output$scatter_substance <- renderUI({ HTML(selected_scatter$substance) })
  
  # Handle click event on province polygons
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    province_clicked <- click$id  # Province ID (PREABBR)
    total_emission <- provinces_with_data %>% filter(PREABBR == province_clicked) %>% pull(total_quantity_ktons)
    diff_from_average <- total_emission - average_ktons
    
    # Determine color based on difference from average
    color <- if (diff_from_average < 0) {
      "green"
    } else if (diff_from_average > 0) {
      "red"
    } else {
      "black"
    }
    
    # Get full province name
    full_name <- province_names[province_clicked]
    
    # Update selected province details
    selected_province$name <- full_name
    selected_province$emission <- paste(round(total_emission, 3), "ktons")
    selected_province$diff <- paste0("<span style='color:", color, "'>", round(diff_from_average, 3), " ktons</span>")
    
    
    
    # Update scatter point details
    selected_scatter$facility <- "Select a facility"
    selected_scatter$substance <- "N/A"
    
    # Filter scatter data and show scatter points
    filtered_data <- aggregated_data %>% filter(province == province_clicked)
    scatter_data <- st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
    
    leafletProxy("map") %>%
      clearGroup("scatter") %>%
      addCircleMarkers(
        data = scatter_data,
        radius = ~log(total_quantity + 1) * 3,
        color = "red", stroke = FALSE, fillOpacity = 0.6,
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 18),
        layerId = ~facility,  # Set layer ID to facility for scatter info
        group = "scatter"
      ) %>%
      showGroup("scatter") %>%
      fitBounds(click$bbox[1], click$bbox[2], click$bbox[3], click$bbox[4])
  })
  
  # Handle click event on scatter points
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    facility_clicked <- click$id
    scatter_info <- aggregated_data %>% filter(facility == facility_clicked)
    
    # Update scatter point details
    selected_scatter$facility <- scatter_info$facility[1]
    selected_scatter$substance <- scatter_info$substances_info[1]
  })
  
  # Handle "Help" button click
  observeEvent(input$help_button, {
    showModal(modalDialog(
      title = "How to Use This App",
      tags$p("1. Click on a province on the map to view its total emissions in ktons and how it compares to the average."),
      tags$p("2. Once a province is selected, facility markers will appear. Click on a facility marker to view more details about its substance emissions."),
      tags$p("3. Use the map controls to zoom and pan around."),
      tags$p("4. The legend on the bottom right shows the scale for the total emissions (in ktons)."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# Run the Shiny app
shinyApp(ui, server)
