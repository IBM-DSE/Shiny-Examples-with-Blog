library(dplyr)
library(readxl)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(htmltools)

download.file('https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip',
              'cb_2018_us_county_5m.zip')
unzip('cb_2018_us_county_5m.zip',exdir='cb_2018_us_county_5m')

download.file('https://www2.census.gov/programs-surveys/popest/geographies/2019/all-geocodes-v2019.xlsx',
              'all-geocodes-v2019.xlsx')

shapes <- rgdal::readOGR("cb_2018_us_county_5m","cb_2018_us_county_5m")

df_geo <- read_excel('all-geocodes-v2019.xlsx',skip=4)  %>% # the table starts from row 5
  filter(`Summary Level`=='040') %>%
  select(`State Code (FIPS)`, `Area Name (including legal/statistical area description)`)

colnames(df_geo) <- c('STATEFP','STATENAME')

shapes@data <- shapes@data %>% 
  left_join(df_geo) %>%
  mutate(ALAND = as.numeric(as.character(ALAND)),
         AWATER = as.numeric(as.character(AWATER)),
         ALAND_RANK = round(percent_rank(ALAND),3),
         AWATER_RANK = round(percent_rank(AWATER),3),
         WATER_LAND_RATIO = AWATER / ALAND,
         WATER_LAND_DIFF = AWATER - ALAND,
         content = paste0('<b>',NAME,', ',STATENAME,'</b>',
                          '<br>Area of Land: ', ALAND, ' (Percentile ',ALAND_RANK,')',
                          '<br>Area of Water: ', AWATER, ' (Percentile ',AWATER_RANK,')',
                          '<br>Water-to-Land Ratio: ', WATER_LAND_RATIO,
                          '<br>Difference: ', WATER_LAND_DIFF),
         NAME = as.character(NAME))

shapes <- shapes[!is.na(shapes@data$STATENAME),] # remove shapes that are not in a state (e.g., Guam)
shapes <- shapes[shapes@data$STATENAME != 'Alaska',] # remove to center the focus
shapes <- shapes[shapes@data$STATENAME != 'Hawaii',] # remove to center the focus

layer_input <- c('Area of Land'='ALAND',
                 'Area of Water'='AWATER',
                 'Area of Land, Percentile'='ALAND_RANK',
                 'Area of Water, Percentile'='AWATER_RANK',
                 'Water-to-Land Ratio'='WATER_LAND_RATIO',
                 'Difference between Water and Land'='WATER_LAND_DIFF')
group_display <- 'Area of Land'


#### UI ####
header <- dashboardHeader(
  title = "Leaflet - Asynchronous Loading",
  titleWidth = 350
)

body <- dashboardBody(
  fluidRow(
    tabBox(
      width = 12, side='left', height = 620, 
      id = "tabset_map",
      tabPanel('Initialization', tabName='Initialization',
               h3('Shiny Application is initialized.'),
               h3('Now you can move to the map tabs and compare the loading time.')
               ),
      tabPanel('Default Loading', tabName='Default Loading',
               leafletOutput("map_default",height=595)),
      tabPanel('Single Layer Loading', tabName='Single Layer Loading',
                leafletOutput("map_single_layer",height=595)),
      tabPanel("Asynchronous Loading", tabName='Asynchronous Loading',
               leafletOutput("map_async",height=595)),
      selected = 'Initialization'
    )
  )
)

ui <- dashboardPage(
  title = "Leaflet - Asynchronous Loading",
  header,
  dashboardSidebar(disable = TRUE),
  body
)

#### Server ####
server <- function(input, output, session) {
  
  # #### initialize reactive values ####
  rvs <- reactiveValues(to_load=0,
                        map=NULL)
  
  #### output ####
  ## output: leaflet map
  output$map_default <- renderLeaflet({
    map <- shapes %>%
      leaflet() %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png')
    
    for (group_name in names(layer_input)){
      map <- map %>%
        addPolygons(data = shapes,
                    group = group_name,
                    weight=1, opacity = 1.0,color = 'white',
                    fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~colorBin('OrRd',get(layer_input[group_name]))(get(layer_input[group_name])),
                    label = lapply(shapes$content,HTML))
    }
  
    map %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = names(layer_input),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      showGroup(group_display)
    
  })
  
  output$map_single_layer <- renderLeaflet({
    shapes %>%
      leaflet() %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addPolygons(data = shapes,
                  group = 'Area of Land',
                  weight=1, opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('OrRd',ALAND)(ALAND),
                  label = lapply(shapes$content,HTML)) %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = names(layer_input),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      showGroup(group_display)
  })
  
  output$map_async <- renderLeaflet({
    rvs$to_load <- isolate(rvs$to_load) + 1 # change the value to trigger observeEvent
    
    rvs$map <- shapes %>%
      leaflet() %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addPolygons(data = shapes,
                  group = 'Area of Land',
                  weight=1, opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('OrRd',ALAND)(ALAND),
                  label = lapply(shapes$content,HTML)) %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = names(layer_input),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      showGroup(group_display)
    
    rvs$map
  })
  
  #### observe event ####
  ## load layer data in an asynchronous fashion
  observeEvent(rvs$to_load,{
    req(rvs$map) # if it's not null or false
    
    group_names_to_load <- names(layer_input)
    group_names_to_load <- group_names_to_load[group_names_to_load != group_display] # take out the default layer already added
    
    for (group_name in group_names_to_load){
      leafletProxy("map_async") %>%
        addPolygons(data = shapes,
                    group = group_name,
                    weight=1, opacity = 1.0,color = 'white',
                    fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~colorBin('OrRd',get(layer_input[group_name]))(get(layer_input[group_name])),
                    label = lapply(shapes$content,HTML))
    }
  })
  
}

#### Run App ####
shinyApp(ui = ui, server = server)