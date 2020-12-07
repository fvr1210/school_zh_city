library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(shinydashboard)


# read in data ----
df_c <- read_delim("processed-data/districts_citizenship.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

district_geo <- read_sf("raw-data/geo_school_districts_zh_city/districts/stzh.adm_schulkreise_a_polygon.shp") 


# change swiss coordinate system to lat long
district_geo_l  <- district_geo %>%  
  sf::st_transform('+proj=longlat +datum=WGS84')



mytext <- paste(
    "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
    lapply(htmltools::HTML)




# define theme ----
mytheme <- theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7),
          legend.title = element_blank(),
          legend.position = "bottom")



ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "ZH City Schooldistricts", titleWidth = 300
                     ),
                     dashboardSidebar(
                       width = 300,
                         sidebarMenuOutput("menu"),
                         
                         tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
                             tags$head(
                                 tags$style(
                                     HTML(                   # Change position of different elements 
                                         ".checkbox-inline {    
                    margin-left: 10px;
                    margin-right: 10px;
                    }",
                                         ".shiny-html-output {    
                    margin-left: 20px;
                    margin-right: 20px;
                    }",
                                         ".action-button{ 
                    margin-left: 10px;
                    margin-top: -10px;
                    margin-bottom: 10px;
                    }",
                                         ".shiny-options-group{ 
                    margin-left: 15px;
                    }",
                                         
                                         ".control-label{ 
                    margin-left: 15px;
                    }",
                                     ))))         
                     ),
                     dashboardBody(
                        tabItem(tabName = "langauges_side",
                         fluidPage(
                             br(),
                             column(8,leafletOutput("map", height="600px")),
                             column(4,br(),br(),br(),br(),plotOutput("plot", height="300px")),
                             br()
                         )
                     )
                     ))









server <- function(input, output) {
    
    
    output$menu <- renderMenu({
        sidebarMenu(
            id = "tabs",
            # menuItem("Info", tabName = "Info_text", icon = icon("info"),
            #          menuSubItem("Intro", tabName = "Intro_tx", icon = icon("")),
            #          menuSubItem("Data Source", tabName = "Source_tx", icon = icon("")),
            #          menuSubItem("Final Data", tabName = "Final_tx", icon = icon(""))),
            menuItem("Languages", tabName = "langauges_side", icon = icon("mouth")))
        }) 
  
    
    
  # reactive Map for choosing district ----
  # codes from https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event and https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/
    
    # create a reactive value that will store the click position
    data_of_click <- reactiveValues(clickedMarker=NULL)
    
    
    # Leaflet map with districts
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = 8.5 ,lat =47.38, zoom = 12)%>%
            addPolygons(data=district_geo_l, 
                        layerId=~bezeichnun,
                        fillColor = "transparent",
                        highlight = highlightOptions(
                            color = "darkblue",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = mytext)
    }) 
    # store the click   https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/
    observeEvent(input$map_shape_click,{
        data_of_click$clickedShape <- input$map_shape_click
    })
    
    
    output$plot=renderPlot({
        my_place=data_of_click$clickedShape$id
        if(is.null(my_place)){my_place="Glattal"}
        if(my_place=="Zürichberg"){
            plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
        }else{
            barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
        }
    })
    }

ui <-  shinyApp(ui = ui, server = server)