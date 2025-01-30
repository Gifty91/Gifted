library(tidyverse)
library(janitor)
library(leaflet)
library(sf)

# Load the chicago data
# data acquired from: https://www.kaggle.com/datasets/thedevastator/public-health-indicators-in-chicago
dat <- read_csv("./data/chicago.csv")
dat <- janitor::clean_names(dat)

# Load the geo data of chicago
# Geo data obtained from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
map <- read_csv("./data/map.csv")
map <- janitor::clean_names(map)

# Standardize community names for both data (title case)
dat <- dat |>
  rename(community_name = community_area_name) |>
  mutate(community_name = str_to_title(community_name))

map <- map |>
  rename(community_name = community) |>
  mutate(community_name = str_to_title(community_name))

# Convert coordinates to sf format
map <- st_as_sf(map, wkt = "the_geom", crs = 4326) 

map_sf <- map |> left_join(dat, by = "community_name")

ui <- fluidPage(
  # div(
  #   style = "text-align: center;",
  #   titlePanel("Chicago Health Indicators")
  # ),
  titlePanel("Chicago Health Indicators"),
  
  # Sidebar on top
  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; padding-right: 50px;",
        
        selectInput(inputId = "indicator", 
                    label = "Select Health Indicator:",
                    choices = c("birth_rate", "preterm_births", "low_birth_weight",
                                "general_fertility_rate", 
                                "prenatal_care_beginning_in_first_trimester",
                                "teen_birth_rate"),
                    
                    selected = "birth_rate"
        )
      )
    )
  ),
  # Map in the center
  fluidRow(
    column(
      width = 12,
      textOutput("text"),
      leafletOutput("map", height = "80vh"),  # Ensures the map takes most of the vertical space
      
    )
  )
)

server <- function(input, output) {
  
  pal <- reactive({
    colorNumeric(palette = "YlOrRd", domain = map_sf[[input$indicator]])
  })
  
  output$map <- renderLeaflet({
    
    leaflet(data = map_sf) %>%
      addTiles() %>%  # Add a base map
      addPolygons(
        fillColor = ~pal()(map_sf[[input$indicator]]), 
        color = "white",                 # Border color
        weight = 1,                      # Border width
        opacity = 1,                     # Border opacity
        fillOpacity = 0.7,               # Fill opacity
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste(community_name, ": ", map_sf[[input$indicator]]),
        labelOptions = labelOptions(
          style = list("font-size" = "15px"),
          noHide = F,
          direction = "auto"
        )
      )
  })
  
  output$text <- renderText({
    paste("Selected indicator:", input$indicator)
  })
}

shinyApp(ui = ui, server = server)
