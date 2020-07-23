library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)

source('utils.R')

if (!file.exists('cb_2018_us_county_5m.zip')){
  download.file('https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip',
                'cb_2018_us_county_5m.zip')
  unzip('cb_2018_us_county_5m.zip',exdir='cb_2018_us_county_5m')
}

shapes <- readOGR("cb_2018_us_county_5m","cb_2018_us_county_5m")


#### UI ####
header <- dashboardHeader(
  title = "Leaflet - Center Diverging Colors",
  titleWidth = 300
)

body <- dashboardBody(
  fluidRow(
    column(width=2,
           selectInput("select_state", label='Select State:',
                       choices = c('Louisiana'=22,
                                   'Maryland'=24),
                       selected= 'New York')
    ),
    column(width=2,
           selectInput("select_format", label='Select Ratio Format:',
                       choices = c('Raw Ratio'=1,
                                   'Percent'=100),
                       selected= 'Raw Ratio')
    )
  ),
  fluidRow(
    tabBox(
      width = 12, side='left', height = 720,
      id = "tabset_map", 
      tabPanel('Default Palette', tabName='Default Palette',
               leafletOutput("map_default",height=650)),
      tabPanel("Custom Palette", tabName='Custom Palette',
               leafletOutput("map_custom",height=650)),
      tabPanel("Advanced Custom Palette", tabName='Advanced Custom Palette',
               leafletOutput("map_advanced",height=650)),
      selected = 'Default Palette'
    )
  )
)

ui <- dashboardPage(
  title = "Leaflet - Center Diverging Colors",
  header,
  dashboardSidebar(disable = TRUE),
  body
)

#### Server ####
server <- function(input, output, session) {
  
  output$map_default <- renderLeaflet({
    poly_state <- shapes[shapes@data$STATEFP == input$select_state,]
    
    poly_state@data <- poly_state@data %>%
      mutate(WATER_LAND_RATIO = as.numeric(as.character(AWATER)) / as.numeric(as.character(ALAND)) * as.numeric(input$select_format),
             content = paste0(NAME,': ',WATER_LAND_RATIO)) 
    
    poly_state %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = poly_state,
                  weight=1, opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('RdBu',WATER_LAND_RATIO)(WATER_LAND_RATIO),
                  label = ~content) %>%
      addLegend(
        "topright",
        pal = colorBin('RdBu', poly_state@data$WATER_LAND_RATIO),
        values = poly_state@data$WATER_LAND_RATIO,
        opacity =  0.9
      )
    
  })
  
  output$map_custom <- renderLeaflet({
    
    poly_state <- shapes[shapes@data$STATEFP == input$select_state,]
    
    poly_state@data <- poly_state@data %>%
      mutate(WATER_LAND_RATIO = as.numeric(as.character(AWATER)) / as.numeric(as.character(ALAND)) * as.numeric(input$select_format),
             content = paste0(NAME,': ',WATER_LAND_RATIO)) 
    
    poly_state@data$WATER_LAND_RATIO <- poly_state@data$WATER_LAND_RATIO - as.numeric(input$select_format)
    
    minVal <- min(poly_state@data$WATER_LAND_RATIO)
    maxVal <- max(poly_state@data$WATER_LAND_RATIO)
    domain <- c(minVal,maxVal)
    
    colorPal <- c(colorRampPalette(colors = c("#b2182b", "#f7e9ea"), space = "Lab")(abs(minVal)),
                  colorRampPalette(colors = c("#e6f2ff", "#2166ac"), space = "Lab")(maxVal))
    
    poly_state %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = poly_state,
                  weight=1,opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~get('colorBin')(colorPal, 
                                               domain)(WATER_LAND_RATIO),
                  label = ~content) %>%
      addLegend(
        "topright",
        pal = colorBin(colorPal, domain = domain+as.numeric(input$select_format)),
        values = domain,
        opacity =  0.9
      )
  })
  
  output$map_advanced <- renderLeaflet({
    poly_state <- shapes[shapes@data$STATEFP == input$select_state,]
    
    poly_state@data <- poly_state@data %>%
      mutate(WATER_LAND_RATIO = as.numeric(as.character(AWATER)) / as.numeric(as.character(ALAND)) * as.numeric(input$select_format),
             content = paste0(NAME,': ',WATER_LAND_RATIO)) 
    
    poly_state@data$WATER_LAND_RATIO <- poly_state@data$WATER_LAND_RATIO - as.numeric(input$select_format)
    
    
    minVal <- min(poly_state@data$WATER_LAND_RATIO)
    maxVal <- max(poly_state@data$WATER_LAND_RATIO)
    domain <- c(minVal,maxVal)
    
    center <- as.numeric(input$select_format)
    interval <- ifelse((maxVal - minVal)>10,10,
                       ifelse((maxVal - minVal)>5,1,0.2))
    
    color_bucket <- calculateBucket(minVal,maxVal,
                                    interval=interval,interval_options = seq(interval,500,interval),center=center,floor_at= -1 * as.numeric(input$select_format))
    df_pal <- inferColor(color_bucket, 
                         color_below = "#b2182b", 
                         color_above = "#2166ac", 
                         interval=interval,
                         center=center)
    
    
    poly_state@data <- poly_state@data %>%
      mutate(WATER_LAND_RATIO_Color_Group = cut(WATER_LAND_RATIO, breaks = color_bucket$breaks, labels = color_bucket$breaks_label)) %>%
      mutate(WATER_LAND_RATIO_Color_Group = as.character(WATER_LAND_RATIO_Color_Group)) %>%
      left_join(df_pal %>%
                  rename(WATER_LAND_RATIO_Color_Group = Color_Label,
                         WATER_LAND_RATIO_Color_Value = Color_Value))
    
    poly_state %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = poly_state,
                  weight=1,opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~WATER_LAND_RATIO_Color_Value,
                  label = ~content) %>%
      addLegend(
        "topright",
        colors = df_pal$Color_Value,
        labels = df_pal$Color_Label,
        opacity =  0.9 # use the same opacity as fillOpacity in addPolygons or you will scratch your head wondering why colors mismatch
      ) 
    
  })
  
}

#### Run App ####
shinyApp(ui = ui, server = server)
