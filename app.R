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
                             actionButton("unselect_district","Unselect school districts"),
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
  
    # store the click, an highlight the clicked districts 
    # used code from https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/ 
    # https://gis.stackexchange.com/questions/215342/changing-the-style-of-a-polygon-with-a-click-event-in-a-shiny-leaflet-app
    # https://stackoverflow.com/questions/41106547/how-to-save-click-events-in-leaflet-shiny-map#41106795
    
    selected<-reactiveValues(Clicks=vector()) # creat a dataframe where clicks will be stored
    
    observeEvent(input$map_shape_click,{
        selected$Clicks
        click <- input$map_shape_click
        selected$Clicks <-c(click$id, selected$Clicks) # selected$Clicks has to be added so that the dataframe select_dis has all selected districts in it
        # print(selected$Clicks)  # print(select_dis())
        
        
    })
    
  # creat a tibble with the selected school districts
  select_dis <- reactive({district_geo_l %>%  filter(bezeichnun %in% selected$Clicks)}) 
 
  
  # clear function if the unselect district is used  
  observeEvent(input$unselect_district, {
      selected$Clicks <- NULL}
    )
  

  # hovertext for the basemap
  mytext <- paste(
    "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
    lapply(htmltools::HTML)
  
  # define map with districts
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.5 ,lat =47.38, zoom = 12)%>%
      addPolygons(data=district_geo_l,
                  layerId=~bezeichnun,
                  fillColor = "transparent",
                  label = mytext)})

  
  
  observe(
  # if no district is choosen show defined map 
  if(is.logical(selected$Clicks)){
    leafletProxy("map")
  }
  # if clear button is used show again defined map
  else if(is.null(selected$Clicks)){
    leafletProxy("map") %>% 
        setView(lng = 8.5 ,lat =47.38, zoom = 12)%>%
        addPolygons(data=district_geo_l,
                    layerId=~bezeichnun,
                    fillColor = "transparent",
                    label = mytext)
  }
  # if districts are clicked on fill them with blue
  else{
    mytext2 <- paste(
      # hovertext when at least one district is selected
      "Schulgemeinde: ", select_dis()$bezeichnun,"<br/>")  %>%
      lapply(htmltools::HTML)
    
    leafletProxy("map") %>% 
      addPolygons(data = select_dis(),
                  layerId = ~bezeichnun,
                  fillColor = "blue",
                  label = mytext2)
  }
  )
  
  # output$selected_var <- renderText({ 
  #   paste(select_dis()$bezeichnun, sep=",")
  # })
  # 
  # test output
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