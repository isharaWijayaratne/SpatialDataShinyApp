## app.R ##

## Libraries
library(shiny)
library(shinydashboard)
library(ceylon)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(dplyr)
library(plotly)
library(readxl)


## Data set - Spacial Data

## Associated Data

data(sf_sl_3) #From Ceylon package
Survey_Sheet <- read_excel("Survey_Sheet.xlsx")
Survey_Sheet <- data.frame(Survey_Sheet)
Survey_Sheet_sf <- st_as_sf(Survey_Sheet, coords = c("Latitude","Longitude"))
st_crs(Survey_Sheet_sf) <- 4326 # we can use EPSG as numeric here

##FOR STATIC MAP
#Create a folder named "client7spatialdata" and within it another folder named "srilanka"
st_write(Survey_Sheet_sf, "client7spatialdata/srilanka", driver = "ESRI Shapefile",append=FALSE)
client7_sf <- st_read("client7spatialdata/srilanka") #str(client7_sf)
client7_sf <- st_transform(client7_sf, crs = st_crs(sf_sl_3))

##FOR DYNAMIC MAP
# Transform the Divisional secretariat boundaries to WGS 84
philly_WGS84 <- st_transform(sf_sl_3, 4326)
# Transform the client7_sf points to WGS 84
client7_sf_wgs84 <- st_transform(client7_sf, 4326)

#Changing Column names of the datasets
colnames(client7_sf)[colnames(client7_sf) == "Dss_ncd"] <- "Disease_Incidence"
colnames(client7_sf)[colnames(client7_sf) == "Rc_Vrty"] <- "Rice_Variety"
colnames(client7_sf)[colnames(client7_sf) == "Sol_Typ"] <- "Soil_Type"
colnames(client7_sf)[colnames(client7_sf) == "Stg_f_m"] <- "Stage_of_Maturity"

colnames(client7_sf_wgs84)[colnames(client7_sf_wgs84) == "Dss_ncd"] <- "Disease_Incidence"
colnames(client7_sf_wgs84)[colnames(client7_sf_wgs84) == "Rc_Vrty"] <- "Rice_Variety"
colnames(client7_sf_wgs84)[colnames(client7_sf_wgs84) == "Sol_Typ"] <- "Soil_Type"
colnames(client7_sf_wgs84)[colnames(client7_sf_wgs84) == "Stg_f_m"] <- "Stage_of_Maturity"

#Colour Pallette 
color_vector1 <- setNames(rainbow(9), unique(client7_sf$Rice_Variety))
color_vector2 <- setNames(rainbow(6), unique(client7_sf$Stage_of_Maturity))
color_vector3 <- setNames(rainbow(6), unique(client7_sf$Soil_Type))

color_palette1 <- c("#286C3B", "#00FF33","#00CCFF","#FFFF00", "#FF00FF","#652926",
                    "#FF9900", "#FF3300", "#005DFF")

color_palette2 <- c( "#00FF33","#00CCFF","#652926", "#FF3300", "#005DFF")

#color_palette3 <- c("#286C3B", "#00FF33","#00CCFF","#FFFF00", "#FF00FF","#652926",
#                    "#FF9900", "#FF3300", "#005DFF", "black")

color_palette3 <- c("#286C3B", "#00FF33","#00CCFF","#FFFF00", "#FF00FF","#652926")

########################################################################################


# Creating the color palette
#set.seed(34) # Set random seed
#color_palette <- distinctColorPalette(34)
#color_palette



# User Interface
ui <- dashboardPage(skin="green",
                    dashboardHeader(title="Spatial Mapping" ),
                    dashboardSidebar(
                      sidebarMenu(id = "sidebarid",
                                  
                                  
                                  menuItem("Dynamic Maps", tabName = "dynamic_graph",
                                           icon = icon(name = "globe", lib="glyphicon")),
                                  
                                  menuItem("Static Maps", tabName = "static_graph",
                                           icon = icon(name = "map-marker", lib="glyphicon")),
                                  
                                  
                                  menuItem("About", tabName = "About",
                                           icon = icon(name = "search", lib="glyphicon"))
                                  
                      )),
                    dashboardBody(tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Calibri", Times, "Calibri", serif;
        font-weight: bold;
        font-size: 28px;
      }
    '))),
                                  tabItems(
                                    
                                    # Dynamic Graph tab
                                    tabItem(tabName = "dynamic_graph",
                                            fluidRow(
                                              box(
                                                width = 3,
                                                height = 675,
                                                column(12, 
                                                       helpText(HTML("<strong> Click to View Locations Visited </strong>")),
                                                       actionButton("loc", "Location")), 
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Rice Variety</strong>")),
                                                       actionButton("dyn_graph1", "Rice Variety")),
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Stage of Maturity</strong>")),
                                                       actionButton("dyn_graph2", "Stage of Maturity")),   
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Soil Type</strong>")),
                                                       actionButton("dyn_graph3", "Soil Type"))
                                                
                                              ),
                                              
                                              # Leaflet map
                                              box(leafletOutput("dyn_map", height = 600), width = 9, height = 675)
                                            )
                                    ),
                                    
                                    
                                    #Static Graph Tab
                                    tabItem(tabName = "static_graph",
                                            fluidRow(
                                              box(
                                                width = 3,
                                                height = 675,
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Locations Visited </strong>")),
                                                       actionButton("remove", "Location")),  
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Rice Variety</strong>")),
                                                       actionButton("stat_graph1", "Rice Variety")),
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Stage of Maturity</strong>")),
                                                       actionButton("stat_graph2", "Stage of Maturity")),   
                                                
                                                column(12, 
                                                       helpText(HTML("<strong>Click to View Incidence by Soil Type</strong>")),
                                                       actionButton("stat_graph3", "Soil Type"))
                                                
                                              ),
                                              
                                              
                                              # 1st visualization
                                              box(plotlyOutput("static_map", height = 600), width = 9, height = 675)
                                            )
                                    ),
                                    
                                    
                                    ##About tab
                                    tabItem(tabName = "About",
                                            tags$head(
                                              tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 15px;
                margin-top:10px;
                font-size:20px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
                                            
                                            h4(strong("Created by")),
                                            p("- Ishara Wijayaratne"),
                                            p("Under the supervision of ",
                                              tags$a(href = "https://thiyanga.netlify.app/", "Dr. Thiyanga S. Talagala.")),
                                            h4(strong("Dynamic Maps")),
                                            p("These graphs have been constructed using the ",
                                              tags$a(href = "https://github.com/thiyangt/ceylon", "Ceylon"), " and Leaflet packages."),
                                            h4(strong("Static Maps")),
                                            p("These graphs have been constructed using the ",
                                              tags$a(href = "https://github.com/thiyangt/ceylon", "Ceylon"), " and Plotly packages."),                                            
                                    )
                                  )))




# Server Function
server <- function(input, output, session) {
  
  ## Dynamic Map Tab
  
  # Calculate the radius based on the "Disease_Incidence" variable
  max_radius <- 25  # Set the maximum radius size
  min_radius <- 1   # Set the minimum radius size
  
  # Normalize the "Disease_Incidence" variable to the range of radius values
  normalized_radius <- scales::rescale(client7_sf_wgs84$Disease_Incidence, to = c(min_radius, max_radius))
  
  
  # Initialize an empty leaflet map
  dyn_map <- leaflet() %>%
    addPolygons(data = philly_WGS84, fillOpacity = 0.2, smoothFactor = 0.1, weight = 1, color = "red") %>%
    addTiles()
  
  # Render the initial map
  output$dyn_map <- renderLeaflet({
    dyn_map
  })
  
  # Visited locations Dynamic Graph
  observeEvent(input$loc, {
    leafletProxy("dyn_map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = client7_sf_wgs84, color = "blue", radius = 1, fillOpacity = 0.7)
  })
  
  # Rice Variety Dynamic Graph 
  observeEvent(input$dyn_graph1, {   
    leafletProxy("dyn_map") %>%
      clearMarkers() %>%
      clearControls() %>%  # Clear existing legend
      addCircleMarkers(data = client7_sf_wgs84, 
                       color = ~color_palette1[match(Rice_Variety, unique(client7_sf_wgs84$Rice_Variety))],  
                       radius = normalized_radius,  # Use the calculated normalized radius values 
                       fillOpacity = 0.3,
                       popup = ~as.character(Rice_Variety)) %>% # Add popup labels with the Rc_vrty value  
      addLegend(
        "topright",  # You can adjust the position of the legend
        title = "Rice Varieties",
        colors = color_palette1,
        labels = unique(client7_sf_wgs84$Rice_Variety),
        opacity = 1
      )
  })
  
  # Stage of Maturity Dynamic Graph 
  observeEvent(input$dyn_graph2, {
    leafletProxy("dyn_map") %>%
      clearMarkers() %>%
      clearControls() %>%  # Clear existing legend
      addCircleMarkers(data = client7_sf_wgs84, 
                       color = ~color_palette2[match(Stage_of_Maturity, unique(client7_sf_wgs84$Stage_of_Maturity))],  
                       radius = normalized_radius,  # Use the calculated normalized radius values 
                       fillOpacity = 0.3,
                       popup = ~as.character(Stage_of_Maturity)) %>% # Add popup labels with the Rc_vrty value  
      addLegend(
        "topright",  # You can adjust the position of the legend
        title = "Stage of Maturity",
        colors = color_palette2,
        labels = unique(client7_sf_wgs84$Stage_of_Maturity),
        opacity = 1
      )
  })
  
  # Soil Type Dynamic Graph 
  observeEvent(input$dyn_graph3, {   
    leafletProxy("dyn_map") %>%
      clearMarkers() %>%
      clearControls() %>%  # Clear existing legend
      addCircleMarkers(data = client7_sf_wgs84, 
                       color = ~color_palette3[match(Soil_Type, unique(client7_sf_wgs84$Soil_Type))],   
                       radius = normalized_radius,  # Use the calculated normalized radius values 
                       fillOpacity = 0.3,
                       popup = ~as.character(Soil_Type)) %>% # Add popup labels with the Rc_vrty value  
      addLegend(
        "topright",  # You can adjust the position of the legend
        title = "Soil Type",
        colors = color_palette3,
        labels = unique(client7_sf_wgs84$Soil_Type),
        opacity = 1
      )
  })
  
  ## Static Map Tab
  
  output$static_map <- renderPlotly({
    adjusted_plot <- ggplot() +
      geom_sf(data = sf_sl_3, fill = "lightgray")
    
    if (input$stat_graph1) {
      adjusted_plot <- adjusted_plot +
        geom_sf(data = client7_sf, aes(color = Rice_Variety, size = Disease_Incidence, alpha = 0.5)) +
        scale_color_manual(values = color_vector1) +
        scale_size_continuous(range = c(1, 20)) +
        labs(size = "Disease_Incidence") +
        theme_minimal()
    } else if (input$stat_graph2) {
      adjusted_plot <- adjusted_plot +
        geom_sf(data = client7_sf, aes(color = Stage_of_Maturity, size = Disease_Incidence, alpha = 0.5)) +
        scale_color_manual(values = color_vector2) +
        scale_size_continuous(range = c(1, 20)) +
        labs(size = "Disease_Incidence") +
        theme_minimal()
    } else if (input$stat_graph3) {
      adjusted_plot <- adjusted_plot +
        geom_sf(data = client7_sf, aes(color = Soil_Type, size = Disease_Incidence, alpha = 0.5)) +
        scale_color_manual(values = color_vector3) +
        scale_size_continuous(range = c(1, 20)) +
        labs(size = "Disease_Incidence") +
        theme_minimal()
    } else if (input$remove) {
      adjusted_plot <- adjusted_plot +
        geom_sf(data = client7_sf, color = "red", size = 1) +
        theme_minimal()
    }
    
    ggplotly(adjusted_plot)
  })
  
}

shinyApp(ui, server)
