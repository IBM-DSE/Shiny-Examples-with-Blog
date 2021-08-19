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
         content = paste0('<b>',NAME,' (',STATENAME,')</b>',
                          '<br>Area of Land: ', ALAND, 
                          '<br>Area of Water: ', AWATER),
         NAME = as.character(NAME))

shapes <- shapes[!is.na(shapes@data$STATENAME),] # remove shapes that are not in a state (e.g., Guam)

names_state <- sort(df_geo$STATENAME)

#### UI ####
header <- dashboardHeader(
  title = "Leaflet - Layer Specific Legend",
  titleWidth = 300
)

body <- dashboardBody(
  fluidRow(
    column(width=2,
           selectInput("select_state", label='Select State:',
                       choices = names_state,
                       selected= 'New York'),
           style='margin-left:20px;z-index:100000'
           )
  ),
  fluidRow(
    column(width = 12,
           box(width = NULL, height = 620,
               leafletOutput("map",height=595),
               status='warning')
    )
  )
)

ui <- dashboardPage(
  title = "Leaflet - Layer Specific Legend",
  skin = 'yellow',
  header,
  dashboardSidebar(disable = TRUE),
  body
)

#### Server ####
server <- function(input, output, session) {
  
  #### initialize reactive values ####
  rvs <- reactiveValues(poly_state=shapes[shapes@data$STATENAME == 'New York',])
  

  #### output ####
  ## output: leaflet map
  output$map <- renderLeaflet({
    
    rvs$map <- rvs$poly_state %>%
      leaflet() %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addPolygons(data = rvs$poly_state,
                  group = 'Area of Land',
                  weight=1, opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('OrRd',ALAND)(ALAND),
                  label = lapply(rvs$poly_state$content,HTML)) %>%
      addPolygons(data = rvs$poly_state,
                  group = 'Area of Water',
                  weight=1, opacity = 1.0,color = 'grey',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('YlGnBu',AWATER)(AWATER),
                  label = lapply(rvs$poly_state$content,HTML)) %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = c('Area of Land','Area of Water'),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(
        "topright",
        pal = colorBin('OrRd', rvs$poly_state$ALAND),
        values = rvs$poly_state$ALAND
      ) %>%
      hideGroup(c('Area of Land','Area of Water')) %>%
      showGroup('Area of Land')
    
  })
  
 
  #### observe mouse events ####
  ## update rv when the selected state changes
  observeEvent(input$select_state, {
    rvs$poly_state <- shapes[shapes@data$STATENAME == input$select_state,]
  })
  
  ## update legend when the selected layer group changes
  observeEvent(input$map_groups, {
    my_map <- leafletProxy("map") %>% clearControls()

    if (input$map_groups == 'Area of Land'){
      my_map <- my_map %>%
        addLegend(
          "topright",
          pal = colorBin('OrRd', rvs$poly_state$ALAND),
          values = rvs$poly_state$ALAND)
    }else{
      my_map <- my_map %>%
        addLegend(
          "topright",
          pal = colorBin('YlGnBu', rvs$poly_state$AWATER),
          values = rvs$poly_state$AWATER)
    }
  })
}

#### Run App ####
shinyApp(ui = ui, server = server)