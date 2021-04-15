library(dplyr)
library(readxl)
library(stringr)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(ggplot2)
library(scales)
library(plotly)

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
         content = paste0('Area of Water: ', AWATER),
         NAME = as.character(NAME))

shapes <- shapes[!is.na(shapes@data$STATENAME),] # remove shapes that are not in a state (e.g., Guam)

names_state <- sort(df_geo$STATENAME)

#### UI ####
header <- dashboardHeader(
  title = "Leaflet - Capture Mouse Events",
  titleWidth = 300
)

body <- dashboardBody(
  fluidRow(
    column(width=2,
           selectInput("select_state", label='Select State:',
                       choices = names_state,
                       selected= 'New York'),
           style='margin-left:20px;z-index:100000'
           ),
    column(width=4,
           radioButtons("select_event", label='Select Mouse Event to Follow:',
                       choices = c('click',
                                   'move'),
                       selected= 'click',
                       inline=TRUE)
    )
  ),
  fluidRow(
    column(width = 8,
           box(width = NULL, height = 720,
               leafletOutput("map_land",height=650),
               status='success')
    ),
    column(width = 4,
           box(width=NULL,height=150,
               htmlOutput('ui_county'),
               status='success'),
           box(width=NULL,height=550,
               plotlyOutput('bar_state'),
               status='success')
    )
  )
)

ui <- dashboardPage(
  title = "Leaflet - Capture Mouse Events",
  skin = 'green',
  header,
  dashboardSidebar(disable = TRUE),
  body
)

#### Server ####
server <- function(input, output, session) {
  
  #### initialize reactive values ####
  rvs <- reactiveValues(map=NULL,
                        poly_state=shapes[shapes@data$STATENAME == 'New York',])
  
  rv_shape <- reactiveVal(FALSE) # whether a click happened on polygon
  rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL) # the location of mouse event
  rv_location_move_old <- reactiveValues(lat=NULL,lng=NULL) # store the previous location of mouseover event to check if moving out to the background map
  rv_text <- reactiveValues(click='Click on the map to see geological information.',
                            move='Move mouse over the map to see geological information.')

  #### output ####
  ## output: leaflet map
  output$map_land <- renderLeaflet({
    
    rvs$map <- rvs$poly_state %>%
      leaflet() %>%
      addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addPolygons(data = rvs$poly_state,
                  weight=1, opacity = 1.0,color = 'white',
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~colorBin('RdBu',ALAND)(ALAND),
                  label = ~content,
                  layerId = paste0(rvs$poly_state$STATEFP,'|',rvs$poly_state$NAME)) %>%
      addLegend(
        "topright",
        pal = colorBin('RdBu', rvs$poly_state$ALAND),
        values = rvs$poly_state$ALAND
      )
    
  })
  
  ## output: geo info of the mouse event
  output$ui_county <- renderUI({
    location_info <- reactiveValuesToList(rv_location)
    if (!all(is.null(unlist(location_info)))){ # if any entry in rv_location is not NULL
      if (rv_shape()){
        HTML(paste(h3(rv_location$id),
                   pre(paste('latitude :',location_info$lat),
                       paste('longitude:', location_info$lng))
        ))
      }else{
        HTML(paste(h3(input$select_state),
                   pre(paste('latitude :',location_info$lat),
                       paste('longitude:', location_info$lng))
        ))
      }
    }else{
      h3(rv_text[[input$select_event]])
    }
  })
  
  ## output: bar plot
  output$bar_state <- renderPlotly({
    if (!rv_shape()){ # if 
      df_plot <- rvs$poly_state@data %>% mutate(Color='1')
    }else{
      df_plot <- rvs$poly_state@data %>% mutate(Color=ifelse(NAME==rv_location$id,'2','1'))
    }
    
    p <- df_plot %>%
      mutate(NAME=reorder(NAME,ALAND)) %>%
      top_n(50,ALAND) %>%
      ggplot(aes(x=NAME,y=ALAND)) +
      geom_bar(aes(fill=Color,text=paste('Area of Land:',ALAND)),stat="identity") +
      coord_flip() +
      scale_fill_manual(values=c('1'="#5aae61", '2'="#fdc086")) +
      scale_y_continuous(labels = comma) +
      theme(legend.position="none",
            axis.text.y = element_text(size=6),
            axis.text.x = element_text(size=6),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            )
    p %>%
      ggplotly(tooltip = "text",height = 530)
  })
  
  #### observe mouse events ####
  ## update rv when the selection of state changes
  observeEvent(input$select_state, {
    rvs$poly_state <- shapes[shapes@data$STATENAME == input$select_state,]
    
    ## reset click flag and location info if switching state
    rv_shape(FALSE)
    rv_location$id <- NULL
    rv_location$lat <- NULL
    rv_location$lng <- NULL
  })
  
  
  ## when any click happens, identify clicks on map and log new location info
  observeEvent(input$map_land_click,{
    map_land_shape_click_info <- input$map_land_shape_click
    map_land_click_info <- input$map_land_click
    
    if (input$select_event == 'click'){
      
      rv_location$id <- str_split_fixed(map_land_shape_click_info$id,'\\|',2)[2] # take the second part which is county name
      rv_location$lat <- round(map_land_click_info$lat,4)
      rv_location$lng <- round(map_land_click_info$lng,4)
      
      if (is.null(map_land_shape_click_info)){ # this happens when there hasn't been any click on polygons -> no shape click
        rv_shape(FALSE)
        rv_location$lat <- paste(rv_location$lat,'(outside of the state)')
        rv_location$lng <- paste(rv_location$lng,'(outside of the state)')
      }else if (!all(unlist(map_land_shape_click_info[c('lat','lng')]) == unlist(map_land_click_info[c('lat','lng')]))){ # this happens when there has been click on polygon
        rv_shape(FALSE)
        rv_location$lat <- paste(rv_location$lat,'(outside of the state)')
        rv_location$lng <- paste(rv_location$lng,'(outside of the state)')
      }else{
        rv_shape(TRUE)
      }

      
    }
  })
  
  
  ## track mouseover events
  observeEvent(input$map_land_shape_mouseover, {
    map_land_shape_mouseover_info <- input$map_land_shape_mouseover
    
    if (input$select_event == 'move'){
      rv_shape(TRUE)
   
      rv_location$id <- str_split_fixed(map_land_shape_mouseover_info$id,'\\|',2)[2]
      rv_location$lat <- round(map_land_shape_mouseover_info$lat,4)
      rv_location$lng <- round(map_land_shape_mouseover_info$lng,4)
    }
  })

  ## track mouseout events
  observeEvent(input$map_land_shape_mouseout, { # triggered when leaving a polygon, can be leaving to a new polygon or leaving to the background map, but doesn't trigger when entering a poylgon from the background map
    map_land_shape_mouseover_info <- input$map_land_shape_mouseover
    map_land_shape_mouseover_info_old <- reactiveValuesToList(rv_location_move_old)
    
    if (input$select_event == 'move'){
      if (all(unlist(map_land_shape_mouseover_info[c('lat','lng')]) == unlist(map_land_shape_mouseover_info_old[c('lat','lng')]))){ # if the logged location for mouseover doesn't change but mouseout changed
        rv_shape(FALSE)

        rv_location$lat <- 'not tracked (outside of the state)'
        rv_location$lng <- 'not tracked (outside of the state)'

      }else{
        rv_location_move_old$lat <- map_land_shape_mouseover_info$lat
        rv_location_move_old$lng <- map_land_shape_mouseover_info$lng
      }
    }
  })
  
}

#### Run App ####
shinyApp(ui = ui, server = server)


