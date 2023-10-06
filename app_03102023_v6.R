#libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(plyr)
library(dplyr)
library(sf)
library(tigris)
library(leaflet)
library(htmltools)
library(DT)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(rgdal)
library(sp)
library(maptools)
library(RColorBrewer)

# Define UI for application

setwd("C:/Users/orena/OneDrive/01_ORENAIKE/02_CAREER_AND_DEVELOPMENTS/SkillsIT/R_Skills/R_shiny/Alina_Incident_Dashboard/Incident_Dashboard")
ssd_mt_admin_point_exp_N <-st_read("./ssd_mt_admin_point_exp_Nv3.gpkg")
SSD_Admin <- st_read("./SSD_Admin.gpkg")

ssd_mt_admin_point_exp_N_count <- ssd_mt_admin_point_exp_N |>
  st_join(SSD_Admin) |> 
  st_drop_geometry() |> 
  count(ADM2_PCODE, name = "Reported Cases")

SSD_Admin <- SSD_Admin |>  left_join(ssd_mt_admin_point_exp_N_count, by = "ADM2_PCODE")

# SSD_Admin |>  if_else(is.na(`Reported Cases`), "No Reported Case", `Reported Cases`)

SSD_Admin %>%
  mutate(`New Column` = if_else(is.na(`Reported Cases`), "No Reported Case", as.character(`Reported Cases`)))


# Get Bounding box
library(rnaturalearth)

# Get the shapefile for Nigeria
SSD_shapefile <- ne_countries(country = "South Sudan", returnclass = "sf")

# Extract bounding coordinates
bbox <- st_bbox(SSD_shapefile)


# Create a custom color palette with 7 distinct colors
custom_colors <- brewer.pal(7, "Set3")

# Get individual coordinates
min_long <- bbox$xmin
max_long <- bbox$xmax
min_lat <- bbox$ymin
max_lat <- bbox$ymax


drowningTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Conflicts") %>%
  count(Reports) %>%
  pull(n)

floodTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Floods") %>%
  count(Reports) %>%
  pull(n)

fireTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Fires") %>%
  count(Reports) %>%
  pull(n)

lightningTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Fires") %>%
  count(Reports) %>%
  pull(n)

slopeTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Drought") %>%
  count(Reports) %>%
  pull(n)

trafficTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Traffic Accidents") %>%
  count(Reports) %>%
  pull(n)

stormTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Insecurity") %>%
  count(Reports) %>%
  pull(n)

infrastructureTotal <- ssd_mt_admin_point_exp_N %>%
  filter(Reports == "Infrastructure Hazards") %>%
  count(Reports) %>%
  pull(n)

mostRecentDate <- today() - 30



ui <- bootstrapPage(
  
  includeCSS("www//style.css"),
  includeCSS("www//grid.css"),
  includeCSS("www//normalize.css"),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300;0,400;0,600;1,300&display=swap');
    "))
  ),
  
  
  tags$body(
    tags$header(
      tags$nav(
        tags$div(class="row",
                 
                 tags$img(src = "NPM_Logo_white.png", class="logo"),
                 tags$ul(class="main-nav",
                         tags$li(tags$a(href="#maps","Report Map and Plots")),
                         # tags$li(tags$a(href="#time-series","Time Series")),
                         tags$li(tags$a(href="#about", "About")),
                 ),
        ),
        
      )
    ),
    tags$div(class="main-text-box",
             tags$h1("Incident Monitoring DashBoad"),
             tags$h2("This dashboard provides an interactive overview of incidents locally.")),

    
    tags$section(class = "section-small",
                 tags$div(class="row",
                          # tags$h4("This dashboard provides an interactive overview of incidents in the Rohingya refugee camps."),
                          tags$h4(HTML('<span style="color: #91522a;">Incidents reports by category are displayed below</span>')),
                          tags$div(class = "line-grey"),
                          tags$div(class="row-narrow-center",
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "affected-population.svg", class = "icon"),
                                            tags$h6(paste(drowningTotal, " Insecurity"), class="incident-type"),
                                            
                                   ),
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Flood.svg", class = "icon"),
                                            tags$h6(paste(floodTotal, " Floods"), class="incident-type"),
                                            
                                   ),
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Fire.svg", class = "icon"),
                                            tags$h6(paste(fireTotal, " Fires"), class="incident-type"),
                                            
                                   ),
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Storm.svg", class = "icon"),
                                            tags$h6(paste(lightningTotal, " Lightning incidents"), class="incident-type"),
                                            
                                   ),   
                                   
                                   
                          ),
                          
                          tags$div(class="row-narrow-center",
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "forced-recruitment.svg", class = "icon"),
                                            tags$h6(paste(slopeTotal, " Conflicts"), class="incident-type"),
                                            
                                   ),
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Car.svg", class = "icon"),
                                            tags$h6(paste(trafficTotal, " Traffic Accidents"), class="incident-type"),
                                            
                                   ),
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Heavy-rain.svg", class = "icon"),
                                            tags$h6(paste(stormTotal, " Wind/Rain/Storms"), class="incident-type"),
                                            
                                            
                                   ),
                                   
                                   
                                   tags$div(class="col span-1-of-4 box",
                                            tags$img(src = "Infrastructure.svg", class = "icon"),
                                            tags$h6(paste(infrastructureTotal, "Infrastructure Hazards"), class="incident-type"),
                                            
                                   )
                                   
                                   
                                   
                                   
                          )),
                 tags$div(class = "line-grey"),
                 # tags$h5(paste("In the last seven days from ", format(mostRecentDate, "%d/%m/%Y"), " there have been ", weekNumber, "incidents.")),
                 # tags$div(class = "weekPlot", plotlyOutput("weekAllPlot", height = "400px"))
    ),
    
    
    
    
    # chart to sit here
    tags$section(
      
      
      tags$div(class = "h3-box", 
               # tags$h2("Interactive Map", id="maps"),
               tags$h1("Interactive Map", id="maps", style = "font-weight: bold; color: #502813;"),
               tags$h5(" Interact with the map below to filter the incidents visible on the map. 'Report web' project aims to create an interactive platform for reporting and tracking the location and needs of displaced people in real-time. This system will empower individuals and communities to report various issues related to social insecurity, environmental insecurity, and infrastructural problems, helping authorities and organizations respond effectively to these challenges. ", class="h5-section1")
               
      ),
      
      tags$div(class = "tabs",
               tabsetPanel(type = "tabs",
                           tabPanel("Interactive Map", 
                                    tags$div(class = "map",
                                             leafletOutput("mymap", height = "100%", width = "100%"),
                                             tags$small("Click on the map polygon see more information."),
                                    )),
                           
                           
                           #plot to sit here
                           tabPanel("Interactive Plot",
                                    tags$div(class = "map",
                                             plotlyOutput("filterPlot", height = "500px"))
                                    
                           ))),
      # tags$div(class = "number-affected",
      #          tags$h5("Based on the selection, the following number of individuals and households were affected:"),
      #          tags$h4(textOutput("individuals")),
      # ),
      
      tags$div(class = "line-grey"),               
    )),
  # tags$section(
  # 
  #   tags$div(class = "h3-box",
  #            tags$h3("Time Series of Incidents", id = "time-series"),
  # tags$h5("The below plot shows a time series of total reported incidents across all camps by month.", class="h5-section1"),
  #            tags$div(class = "monthPlot", plotlyOutput("annualPlot", height = "400px"))),
  #   tags$div(class = "line-grey"),
  # 
  # ),
  
  tags$section(
    
    tags$div(class = "section3-image",
             tags$h3("About Report Web", class = "section3-text", id = "about")),
    tags$div(class = "h3-box-about",
             
             tags$div(class = "tabs-2",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Overview",
                             
                             tags$p(class="h5-section3",
                                    "This Report web presents an overview of reported incidents in a location, 'Report IT' project aims to create an interactive platform for reporting and tracking the location and needs of displaced people in real-time. This system will empower individuals and communities to report various issues related to social insecurity, environmental insecurity, and infrastructural problems, helping authorities and organizations respond effectively to these challenges."),
                             
                             tags$div(class = "line-grey"),
                                  ),
                             
                             tabPanel("Methodology",
                                      
                                      
                                      
                                      # tags$h4("Methodology", class= "h4-section3"),
                                      
                                      tags$p("Data used in this project does not reflet the true event", class="h5-section3"),
                                    
                                      tags$ul(class = "ul-section3",   
                                              tags$li(tags$b("Insecurity: "), "Security-related issues causing harm or danger."),
                                              tags$li(tags$b("Floods: "), "The overflowing of water of the normal confines of a stream or other body of water."),
                                              tags$li(tags$b("Fires: "), "A fire from any source that causes injury or damage."),
                                              tags$li(tags$b("Traffic Accidents: "), "Accidents involving vehicles resulting in injury, death, or damage."),
                                              tags$li(tags$b("Conflicts: "), "Disputes or hostilities resulting in harm."),
                                              tags$li(tags$b("Infrastructure Hazards: "), "Injury or death due to unsafe infrastructure."),
                                              tags$li(tags$b("Drought: "), "Prolonged periods of dry weather causing water shortage."))
                                      ,
                                    
     
                                
                                tags$div(class = "line-grey"),
                                
                             ),
                             # -------------
                             tabPanel("Atribusion",
                                      tags$ul(
                                        tags$li("Aline Kirkland Previous work on Incidence dashboard for NPM"),
                                        tags$li("Unsplash Images: https://unsplash.com/"),
                                        tags$li("ALX Software Engineering: https://www.alxafrica.com/software-engineering-plus/")
                                      ),
                                tags$div(class = "line-grey"),
                               ),
                             
                             # ---------------
                             
                             
                             ),
                      
             ))),
  
  tags$footer(
    tags$div(class = "footer",
             tags$caption(paste0("This dashboard is developedd to demostrate the use of R Shiny for incidence report The dashboard was last updated on ", today(), "."), class = "footer-div")),
    
  ),
  tags$style(HTML("
    .blue-popup .leaflet-popup-content-wrapper {
      background-color: #502813;
      color: white;
    }
  ")),
  
)




##################### SERVER #########################################
# Define server logic ----
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Esri_Basemap") %>%
      addProviderTiles("CartoDB.Positron", group = "Carto_Basemap") %>%
      addPolygons(data = SSD_Admin,
                  label = ~ADM3_NAME_,
                  popup = ~paste(`Reported Cases`,'<span style="font-size: 15px;"><b>', "Cases in ",ADM2_EN_18, '</b></span>',
                                 "<hr>",
                                 "County: ", ADM2_EN_18,
                                 "<br>", "Payam : ", ADM3_NAME_,
                                 "<br>", "PCODE: ", `Reported Cases`),
                  color = "grey",
                  weight = 2,
                  fillColor = "#E6EFFB",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  dashArray = "3",
                  fillOpacity = .2,
                  highlightOptions = highlightOptions(color = "#0033A0", weight = 5,
                                                      bringToFront = FALSE),
                  group = "Payam",
                  popupOptions = popupOptions(
                    closeButton = TRUE,
                    className = "blue-popup"
                  )
      ) |>
      addCircleMarkers(data = ssd_mt_admin_point_exp_N,
                       color = ~custom_colors[as.factor(ssd_mt_admin_point_exp_N$Reports)],
                       fillColor = ~custom_colors[as.factor(ssd_mt_admin_point_exp_N$Reports)],
                       radius = 7,
                       stroke = TRUE,
                       fillOpacity = 0.8,
                       popup = ~paste("Reports |", Reports ),
                       group = "marker") |>
      addLegend(position = "bottomleft",
                colors = custom_colors,
                labels = unique(ssd_mt_admin_point_exp_N$Reports),
                opacity = 1,
                title = "Reports Categories") |>
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") |>
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) |>
      addLayersControl(baseGroups = c("Carto_Basemap", "Esri_Basemap"),
                       overlayGroups = c("marker", "Payam"),
                       option = layersControlOptions(collapsed = FALSE)
      ) |>
      fitBounds(lng1 = 29, lat1 = 4.852, lng2 = 32, lat2 = 11, options = list(zoom_level = 3))
  })
  
  observeEvent(input$mymap_dblclick, {
    click <- input$mymap_dblclick
    if (!is.null(click)) {
      leafletProxy("mymap") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 15)
    }
  })
}



# Run the app ----
shinyApp(ui = ui, server = server)