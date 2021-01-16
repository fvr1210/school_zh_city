library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(shinydashboard)
library(ggiraph)
library(RColorBrewer)

# read in data ----


#* Geo data for Zurich city school districts ----
district_geo <- read_sf("raw-data/geo_school_districts_zh_city/districts/stzh.adm_schulkreise_a_polygon.shp") 

# change swiss coordinate system to lat long
district_geo_l  <- district_geo %>%  
  sf::st_transform('+proj=longlat +datum=WGS84')

#* Total Data ----
#* Including all School levels
#** Data to daz, amount of students and average class size ----
df_t_daz <- read_delim("processed-data/districts_total_daz_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

#** Data to citizienship----
df_t_cs <- read_delim("processed-data/districts_total_citizenship.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

#** Data to amount of countries----
df_t_dc <- read_delim("processed-data/districts_total_diff_citizenship.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

#** description  text -----
total_text <-  read_file("processed-data/total_text.html", locale = locale(encoding = 'utf-8')) # text description for tab with all school levels

kp_text <-  read_file("processed-data/kinder_primar_text.html", locale = locale(encoding = 'utf-8')) # text description for tab with all school levels

sek_text <-  read_file("processed-data/sek_text.html", locale = locale(encoding = 'utf-8')) # text description for tab with all school levels



#* Unterstufen Data ----
#* Including Kindergarten, Uebergangsklassen and Primar
#** Data to daz, amount of students and average class size ----
df_kp_daz <- read_delim("processed-data/districts_kinder_primar_daz_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

# #** Data to citizienship----
# df_t_cs <- read_delim("processed-data/districts_citizenship.csv", delim = ";", locale = locale(encoding = 'utf-8')) 
# 
# #** Data to amount of countries----
# df_t_dc <- read_delim("processed-data/districts_diff_citizenship.csv", delim = ";", locale = locale(encoding = 'utf-8')) 
# 
# #** description  text -----
# total_text <-  read_file("processed-data/total.html", locale = locale(encoding = 'utf-8')) # text description for tab with all school levels

#* Sekundar Data ----
#* Including Sek A, B, C
#** Data to daz, amount of students and average class size ----
df_sek_daz <- read_delim("processed-data/districts_sek_daz_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8')) 




#* Info text




# define theme ----
mytheme <- theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          legend.title = element_blank(),
          legend.position = "bottom")

# colors for districts -----    
col_dis <- c( "Glattal" = "#d10070", "Letzi" = "#5D3D55", "Limmattal" = "#3F5C75", "Schwamendingen" = "#177972", "Uto" = "#548D51", "Waidberg" = "#A69439", "Zürichberg" = "#F48E5F") 

# color for countries
nb.cols <- 18
col_countries <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)


# Ui side -----
ui <- dashboardPage(skin = "blue",
                     dashboardHeader(title = "Schulboard", titleWidth = 300 # extend width because of the longer title
                     ),
                    
                    # sidebare ---------
                     dashboardSidebar( 
                       width = 300,
                       sidebarMenu(
                         menuItem("Info", tabName = "Info_text", icon = icon("info")),
                            #          menuSubItem("Intro", tabName = "Intro_tx", icon = icon("")),
                            #          menuSubItem("Data Source", tabName = "Source_tx", icon = icon("")),
                            #          menuSubItem("Final Data", tabName = "Final_tx", icon = icon(""))),
                         menuItem("Volksschulstufen aggregiert", tabName = "total", icon = icon("school")),    
                         menuItem("Kindergarten und Primarschule", tabName = "unterstufe", icon = icon("child")), 
                         menuItem("Sekundarschule", tabName = "sek", icon = icon("book"))),
                         
                         tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
                             tags$head(
                                 tags$style(
                                     HTML(                   # Change position of different elements 
                                         ".checkbox-inline {    
                    margin-left: 10px;
                    margin-right: 10px;
                    }",
                                         
                                         ".checkbox-inline {    
                    margin-left: 10px;
                    margin-right: 10px;
                    }",
                      
                                         ".action-button{ 
                    margin-left: 10px;
                    margin-top: 10px;
                    margin-bottom: 15px;
                    }",
                                         ".shiny-options-group{ 
                    margin-left: 15px;
                    }",
                                         
                                       ".control-label{ 
                    margin-left: 15px;
                    }",
                    
                    ".shiny-output-error { visibility: hidden; }",         # not displaying errors on the dashboard
                                         
                    ".shiny-output-error:before { visibility: hidden; }"  # not displaying errors on the dashboard
                                     ))))         
                     ),
                     
                     
                     #body ----
                     dashboardBody(
                       tabItems(
                      #* total ----
                      tabItem(tabName = "total",
                              
                              fluidPage(
                                # info text
                                htmlOutput("total_text"),
                                br(),
                                # text for district choosing
                                htmlOutput("choosing_t"),
                                br(),
                                # map
                                leafletOutput("map_t", height="480px", width="800px"),
                                # Action button to unselect the choosen districts - not sure yet where to place it
                                actionButton("unselect_district_t","Schulkreis Auswahl aufheben"),
                                br(),
                                sliderInput(inputId="d_Jahr_t", label = 'Beobachtungszeitraum wählen:',
                                            min=min(df_t_daz$Jahr), max=max(df_t_daz$Jahr), value=c(min(df_t_daz$Jahr), max(df_t_daz$Jahr)), sep = ""),
                                girafeOutput("plot_as_t", width = "100%", height = "400px"),
                                girafeOutput("plot_daz_t", width = "100%", height = "400px"),
                                # girafeOutput("plot_acs_t", width = "100%", height = "400px"), 
                                girafeOutput("plot_dc_t", width = "100%", height = "400px"),
                                girafeOutput("plot_cs_t", width = "100%", height = "1000px"),
                                
                                #   htmlOutput("total_text")),
                                # text for map
                                # htmlOutput("choosing_u"),
                                # # map
                                # leafletOutput("map", height="480px", width="800px"),
                                # # Action button to unselect the choosen districts - not sure yet where to place it
                                # actionButton("unselect_district","Schulkreis Auswahl aufheben"),
                                # # slider to select years on the sidebar
                                # sliderInput(inputId="d_Jahr", label = 'Beobachtungszeitraum:',
                                #             min=min(df_daz$Jahr), max=max(df_daz$Jahr), value=c(min(df_daz$Jahr), max(df_daz$Jahr)), sep = "")
                             )),
                              
                         
                      #* Unterstufe  -----
                      tabItem(tabName = "unterstufe",
                         fluidPage(
                           # info text
                           htmlOutput("kp_text"),
                           br(),
                           # text for district choosing
                           htmlOutput("choosing_kp"),
                           br(),
                           # map
                           leafletOutput("map_kp", height="480px", width="800px"),
                           # Action button to unselect the choosen districts - not sure yet where to place it
                           actionButton("unselect_district_kp","Schulkreis Auswahl aufheben"),
                           br(),
                           sliderInput(inputId="d_Jahr_kp", label = 'Beobachtungszeitraum wählen:',
                                       min=min(df_t_daz$Jahr), max=max(df_t_daz$Jahr), value=c(min(df_t_daz$Jahr), max(df_t_daz$Jahr)), sep = ""),
                           girafeOutput("plot_kp_d", width = "100%", height = "800px"),
                           radioButtons(inputId = "stufen_kp", label = "Schulstufen",
                                              choices = c(
                                                "Kindergarten",
                                                "Übergangsklassen",
                                                "Primarschule"),
                                              selected = c("Kindergarten"),
                                              inline = T
                           ),
                           girafeOutput("plot_as_kp", width = "100%", height = "400px"),
                           girafeOutput("plot_daz_kp", width = "100%", height = "400px"),
                           # girafeOutput("plot_daz_t", width = "100%", height = "400px"),
                           girafeOutput("plot_acs_kp", width = "100%", height = "400px"),
                           # girafeOutput("plot_dc_t", width = "100%", height = "400px"),
                           
                         )),
                        
                    #* Sekundarstufe  ----
                     tabItem(tabName = "sek",
                             fluidPage(
                               # info text
                               htmlOutput("sek_text"),
                               br(),
                               # text for district choosing
                               htmlOutput("choosing_sek"),
                               br(),
                               # map
                               leafletOutput("map_sek", height="480px", width="800px"),
                               # Action button to unselect the choosen districts - not sure yet where to place it
                               actionButton("unselect_district_sek","Schulkreis Auswahl aufheben"),
                               br(),
                               sliderInput(inputId="d_Jahr_sek", label = 'Beobachtungszeitraum wählen:',
                                           min=min(df_t_daz$Jahr), max=max(df_t_daz$Jahr), value=c(min(df_t_daz$Jahr), max(df_t_daz$Jahr)), sep = ""),
                               girafeOutput("plot_sek_d", width = "100%", height = "800px"),
                               radioButtons(inputId = "stufen_sek", label = "Schulstufen",
                                            choices = c(
                                              "Sekundarstufe A",
                                              "Sekundarstufe B",
                                              "Sekundarstufe C"),
                                            selected = c("Sekundarstufe A"),
                                            inline = T
                               ),
                               girafeOutput("plot_as_sek", width = "100%", height = "400px"),
                               girafeOutput("plot_daz_sek", width = "100%", height = "400px"),
                               # girafeOutput("plot_daz_t", width = "100%", height = "400px"),
                               girafeOutput("plot_acs_sek", width = "100%", height = "400px"),
                               # girafeOutput("plot_dc_t", width = "100%", height = "400px"),
                               ))),
                    ),
                     )



server <- function(input, output) {

# Total ----  
#**************************************************************************************************************************************************************************************************************  
  #*  Text ----
  # Introduction text 
  output$total_text <- renderUI({HTML(total_text)})
  # Choosing districts text
  output$choosing_t <- renderUI({HTML("<h4>Wählen Sie auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
  
  #* Map ----
  # reactive Map for choosing district 
  # codes from https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event and https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/
  
  
  # store the click, an highlight the clicked districts 
  # used code from https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/ 
  # https://gis.stackexchange.com/questions/215342/changing-the-style-of-a-polygon-with-a-click-event-in-a-shiny-leaflet-app
  # https://stackoverflow.com/questions/41106547/how-to-save-click-events-in-leaflet-shiny-map#41106795
  
  selected_t<-reactiveValues(Clicks=vector()) # creat a vector where clicks will be stored
  
  # save selected clicks in the vector created above
  observeEvent(input$map_t_shape_click,{
    selected_t$Clicks
    click <- input$map_t_shape_click
    selected_t$Clicks <-c(click$id, selected_t$Clicks) # selected$Clicks has to be added so that the vector select_dis has all selected districts in it
  })
  
  # creat a rective tibble with the selected school districts
  select_dis_t <- reactive({district_geo_l %>%  filter(bezeichnun %in% selected_t$Clicks)}) 
  
  
  # clear function if the unselect district is used  
  observeEvent(input$unselect_district_t, {
    selected_t$Clicks <- NULL}
  )
  
  # hovertext for the basemap
  mytext <- paste(
    "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
    lapply(htmltools::HTML)
  
  # define map with districts
  output$map_t <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
      addPolygons(data=district_geo_l,
                  layerId=~bezeichnun,
                  fillColor = "transparent",
                  label = mytext)})
  
  
  # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different orgins 
  
  observe(
    # if no district is choosen show defined map 
    if(is.logical(selected_t$Clicks)){
      leafletProxy("map_t")
    }
    # if clear button is used show again defined map
    else if(is.null(selected_t$Clicks)){
      leafletProxy("map_t") %>% 
        setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
        addPolygons(data=district_geo_l,
                    layerId=~bezeichnun,
                    fillColor = "transparent",
                    label = mytext)
    }
    # if districts are clicked on fill them with blue
    else{
      mytext2 <- paste(
        # hovertext when at least one district is selected
        "Schulgemeinde: ", select_dis_t()$bezeichnun,"<br/>")  %>%
        lapply(htmltools::HTML)
      
      leafletProxy("map_t") %>% 
        addPolygons(data = select_dis_t(),
                    layerId = ~bezeichnun,
                    fillColor = "blue",
                    label = mytext2)
    }
  )
  
  #* data ----
  # reactive dataframes / tibbles based on the selected district and year 
  
  # for students, daz and average class size
  daz_t_reactive <- reactive({
    dplyr::filter(df_t_daz, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
  })

  # for citizienship 
  df_t_cs_reactive <- reactive({
    dplyr::filter(df_t_cs, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
  })
  
  df_t_dc_reactive <- reactive({
    dplyr::filter(df_t_dc, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
  })
  
  
  
  # create max and min for scale
  # years
  j_t_max <- reactive({max(daz_t_reactive()$Jahr)})
  j_t_min <- reactive({min(daz_t_reactive()$Jahr)})
  
  #values
  s_t_max <- reactive({max(daz_t_reactive()$students)})
  
  
  
  
  #* Plots ----
  #** Amount of Students-----
  #* Pint and Line plot
  output$plot_as_t <-  renderGirafe ({
    p_t_as <-  ggplot() +
      geom_point_interactive(daz_t_reactive(), 
                             mapping = aes(x=Jahr, y=students, 
                                           color=factor(Schulgemeinde),
                                           size = 3,
                                           tooltip = paste("Schulkreis:", daz_t_reactive()$Schulgemeinde, "\nAnzahl Schüler*innen:", daz_t_reactive()$students)))+
      geom_line(daz_t_reactive(), 
                mapping = aes(x=Jahr, y=students, color = factor(Schulgemeinde))) +
      scale_color_manual(values = col_dis) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, s_t_max()*1.1)) +
      scale_x_continuous(breaks = seq(j_t_min(), j_t_max(), 1),
                         limits = c(j_t_min(), j_t_max())) +
      ylab("") +
      ggtitle("Total Anzahl Schüler*innen Schulstufen Kindergarten-, Primar- und Sekundarstufe aggregiert (ohne Aufnahme- und Kleinklassen)") +
      guides(size = "none") +
      mytheme
    
    girafe(code=print(p_t_as), width_svg = 20)
  })
  
  
  
  #** DaZ ----
  # Point and Line
  output$plot_daz_t <-  renderGirafe ({
    p_daz_t <-  ggplot() +
      geom_point_interactive(daz_t_reactive(), 
                             mapping = aes(x=Jahr, y=daz, 
                                           color=factor(Schulgemeinde),
                                           size = 3,
                                           tooltip = paste("Schulkreis:", daz_t_reactive()$Schulgemeinde, "\nDaZ Anteil:", round(daz_t_reactive()$daz*100,2), "%")))+
      geom_line(daz_t_reactive(), 
                mapping = aes(x=Jahr, y=daz, color = factor(Schulgemeinde))) +
      scale_color_manual(values = col_dis) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                         limits = c(0, 1)) +
      scale_x_continuous() +
      scale_x_continuous(breaks = seq(j_t_min(), j_t_max(), 1),
                         limits = c(j_t_min(), j_t_max())) +
      ylab("") +
      ggtitle("Anteil an Schüler*innen, die Deutsch als Zweitsprache haben (DaZ)") +
      guides(size = "none") +
      mytheme
    
    girafe(code=print(p_daz_t), width_svg = 20)
  })
  
  #** Average class size-----  
  # the average class size is influenced by the class size of the Sekundarstufe. The average class size on this level is not a good indicator because of the a, b, c system
  # output$plot_acs_t <-  renderGirafe ({
  #   p_acs_t <-  ggplot(daz_t_reactive(), aes(x=Jahr, y=acs, fill=Schulgemeinde)) +
  #   geom_col_interactive(aes(tooltip = paste("Schulkreis:", daz_t_reactive()$Schulgemeinde, "\nDurchschnittliche Klassengrösse:", daz_t_reactive()$acs)), position = position_dodge(width=0.9), width = 0.85, color=NA)+
  #   scale_y_continuous() +
  #   scale_fill_manual(values = col_dis) +
  #   scale_x_continuous() +
  #   scale_x_continuous(breaks = seq(j_t_min(), j_t_max(), 1),
  #                      limits = c(j_t_min()-1, j_t_max()+0.5)) +
  #   ylab("") +
  #   ggtitle("Durchschnittliche Klassengrösse") +
  #   mytheme
  # 
  # girafe(code=print(p_acs_t), width_svg = 20)
  # })
  # 
  
  #** amount of nations ----
  # Barplot
  output$plot_dc_t <-  renderGirafe ({
    p_dc_t <-  ggplot(df_t_dc_reactive(), aes(x=Jahr, y=diff_c, fill=Schulgemeinde)) +
      geom_col_interactive(aes(tooltip = paste("Schulgemeinde:", df_t_dc_reactive()$Schulgemeinde, "\nAnzahl Nationalitäten:", df_t_dc_reactive()$diff_c)), position = position_dodge(width=0.9), 
                           width = 0.85, 
                           color=NA)+
      scale_y_continuous() +
      scale_fill_manual(values = col_dis) +
      scale_x_continuous() +
      scale_x_continuous(breaks = seq(j_t_min(), j_t_max(), 1),
                         limits = c(j_t_min()-1, j_t_max()+0.5)) +
      ylab("") +
      ggtitle("Anzahl verschiedener Nationalitäten") +
      mytheme
    
    girafe(code=print(p_dc_t), width_svg = 20)
  })
  
  
  
  
  #** citizienship ----
  #*col plot
  output$plot_cs_t <-  renderGirafe ({
    p_cs_t <-  df_t_cs_reactive() %>% 
      mutate(country_g = fct_reorder(country_g, -rank, max)) %>% 
      ggplot() +
      geom_col_interactive(aes(x = Jahr, y=per_g, fill = country_g,
                               tooltip = paste("Staatsangehörigkeit:", country_g, "\nAnteil Schüler:", (round(df_t_cs_reactive()$per_g,2))*100, "%"),), 
                           color = NA) +
      scale_fill_manual(values = col_countries) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      scale_x_continuous() +
      scale_x_continuous(breaks = seq(j_t_min(), j_t_max(), 1),
                         limits = c(j_t_min()-1, j_t_max()+0.5)) +
      facet_wrap(~Schulgemeinde) +
      ylab("") +
      mytheme  
    
    girafe(code=print(p_cs_t), width_svg = 10)
  })
  

  
  
  
  
  
  
  
  
# Unterstufe  -----
#**************************************************************************************************************************************************************************************************************  
  
      
# text which is displayed above the map
  #*  Text ----
  # Introduction text 
  output$kp_text <- renderUI({HTML(kp_text)})
  # Choosing districts text
  output$choosing_kp <- renderUI({HTML("<h4>Wählen Sie auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
  
  
#* Map ----
  
    
    selected_kp<-reactiveValues(Clicks=vector()) # creat a vector where clicks will be stored
    
    # save selected clicks in the vector created above
    observeEvent(input$map_kp_shape_click,{
      selected_kp$Clicks
      click <- input$map_kp_shape_click
      selected_kp$Clicks <-c(click$id, selected_kp$Clicks) # selected$Clicks has to be added so that the vector select_dis has all selected districts in it
    })
    
    # creat a rective tibble with the selected school districts
    select_dis_kp <- reactive({district_geo_l %>%  filter(bezeichnun %in% selected_kp$Clicks)}) 
    
    
    # clear function if the unselect district is used  
    observeEvent(input$unselect_district_kp, {
      selected_kp$Clicks <- NULL}
    )
    
    # hovertext for the basemap
    mytext <- paste(
      "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
      lapply(htmltools::HTML)
    
    # define map with districts
    output$map_kp <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
        addPolygons(data=district_geo_l,
                    layerId=~bezeichnun,
                    fillColor = "transparent",
                    label = mytext)})
    
    
    # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different orgins 
    
    observe(
      # if no district is choosen show defined map 
      if(is.logical(selected_kp$Clicks)){
        leafletProxy("map_kp")
      }
      # if clear button is used show again defined map
      else if(is.null(selected_kp$Clicks)){
        leafletProxy("map_kp") %>% 
          setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
          addPolygons(data=district_geo_l,
                      layerId=~bezeichnun,
                      fillColor = "transparent",
                      label = mytext)
      }
      # if districts are clicked on fill them with blue
      else{
        mytext2 <- paste(
          # hovertext when at least one district is selected
          "Schulgemeinde: ", select_dis_kp()$bezeichnun,"<br/>")  %>%
          lapply(htmltools::HTML)
        
        leafletProxy("map_kp") %>% 
          addPolygons(data = select_dis_kp(),
                      layerId = ~bezeichnun,
                      fillColor = "blue",
                      label = mytext2)
      }
    )
    
    #* data ----
    # reactive dataframes / tibbles based on the selected district and year 
    
    # for distribution 
    daz_kp_d_reactive <- reactive({
      dplyr::filter(df_kp_daz, Schulgemeinde %in% select_dis_kp()$bezeichnun & Jahr >= input$d_Jahr_kp[1] & Jahr <= input$d_Jahr_kp[2])
    })
    
    # for amount of students, daz and classsize
    daz_kp_reactive <- reactive({
      dplyr::filter(df_kp_daz, Schulgemeinde %in% select_dis_kp()$bezeichnun & Jahr >= input$d_Jahr_kp[1] & Jahr <= input$d_Jahr_kp[2] & Schulart %in% input$stufen_kp)
    })
    

    # # for citizienship 
    # df_kp_cs_reactive <- reactive({
    #   dplyr::filter(df_t_cs, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
    # })
    # 
    # df_t_dc_reactive <- reactive({
    #   dplyr::filter(df_t_dc, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
    # })
    # 
    # 
    # 
    # # create max and min for scale
    # # years
    j_kp_max <- reactive({max(daz_kp_reactive()$Jahr)})
    j_kp_min <- reactive({min(daz_kp_reactive()$Jahr)})

    # # #values
    s_kp_max <- reactive({max(daz_kp_reactive()$students)})
    # 
    # 
    # 
    
    #* Plots ----
    #** distribution ----
    #*col plot
    output$plot_kp_d <-  renderGirafe ({
      p_kp_d <-  daz_kp_d_reactive() %>% 
        mutate(Schulart =  ordered(Schulart, levels = c("Kindergarten", "Übergangsklassen", "Primarschule"))) %>%  
        ggplot() +
        
        geom_col_interactive(aes(x = Jahr, y=per, fill = Schulart,
                                 tooltip = paste("Schulstufe:", Schulart, "\nAnteil Schüler:", daz_kp_d_reactive()$per, "%"),)) +
        # scale_fill_manual(values = col_countries) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
                           limits = c(j_kp_min()-1, j_kp_max()+0.5)) +
        facet_wrap(~Schulgemeinde) +
        ylab("") +
        ggtitle("Anteil Kindergarten, Übergangsklassen und Primarschule") +
        mytheme  
      
      girafe(code=print(p_kp_d), width_svg = 10)
    })
    
    
    
    
    
    
    
    #** Amount of Students-----
    #* Pint and Line plot
    output$plot_as_kp <-  renderGirafe ({
      p_kp_as <-  ggplot() +
       geom_point_interactive(daz_kp_reactive(), 
                             mapping = aes(x=Jahr, y=students, 
                                           color=factor(Schulgemeinde),
                                           size = 3,
                                           tooltip = paste("Schulkreis:", daz_kp_reactive()$Schulgemeinde, "\nAnzahl Schüler*innen:", daz_kp_reactive()$students)))+
        geom_line(daz_kp_reactive(), 
                  mapping = aes(x=Jahr, y=students, color = factor(Schulgemeinde))) +
        scale_color_manual(values = col_dis) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, s_kp_max()*1.1)) +
        scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
                           limits = c(j_kp_min(), j_kp_max())) +
     
        ylab("") +
        ggtitle(paste("Total Anzahl Schüler*innen", input$stufen_kp, "(ohne Aufnahme- und Kleinklassen)")) +
        guides(size = "none") +
        mytheme
      
      girafe(code=print(p_kp_as), width_svg = 20)
    })
    
    
    #** DaZ ----
    # Point and Line
    output$plot_daz_kp <-  renderGirafe ({
      p_daz_kp <-  ggplot() +
        geom_point_interactive(daz_kp_reactive(), 
                               mapping = aes(x=Jahr, y=daz, 
                                             color=factor(Schulgemeinde),
                                             size = 3,
                                             tooltip = paste("Schulkreis:", daz_kp_reactive()$Schulgemeinde, "\nDaZ Anteil:", round(daz_kp_reactive()$daz*100,2), "%")))+
        geom_line(daz_kp_reactive(), 
                  mapping = aes(x=Jahr, y=daz, color = factor(Schulgemeinde))) +
        scale_color_manual(values = col_dis) +
        scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                           limits = c(0, 1)) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
                           limits = c(j_kp_min(), j_kp_max())) +
        ylab("") +
        ggtitle(paste("Anteil Schüler*innen, die Deutsch als Zweitsprache haben (DaZ), auf der Schulstufe", input$stufen_kp)) +
        guides(size = "none") +
        mytheme
      
      girafe(code=print(p_daz_kp), width_svg = 20)
    })
    
    #** Average class size-----  
    
    output$plot_acs_kp <-  renderGirafe ({
      p_acs_kp <-  ggplot(daz_kp_reactive(), aes(x=Jahr, y=acs, fill=Schulgemeinde)) +
        geom_col_interactive(aes(tooltip = paste("Schulkreis:", daz_kp_reactive()$Schulgemeinde, "\nDurchschnittliche Klassengrösse:", daz_kp_reactive()$acs)), position = position_dodge(width=0.9), width = 0.85, color=NA)+
        scale_y_continuous() +
        scale_fill_manual(values = col_dis) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
                           limits = c(j_kp_min()-1, j_kp_max()+0.5)) +
        ylab("") +
        ggtitle("Durchschnittliche Klassengrösse") +
        mytheme
      
      girafe(code=print(p_acs_kp), width_svg = 20)
    })
    
    
    
    # Sekundarstufe  -----
    #**************************************************************************************************************************************************************************************************************  
    
    
    #*  Text ----
    # Introduction text 
    output$sek_text <- renderUI({HTML(sek_text)})
    # Choosing districts text
    output$choosing_sek <- renderUI({HTML("<h4>Wählen Sie auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
    
    #* Map ----
    
    
    selected_sek<-reactiveValues(Clicks=vector()) # creat a vector where clicks will be stored
    
    # save selected clicks in the vector created above
    observeEvent(input$map_sek_shape_click,{
      selected_sek$Clicks
      click <- input$map_sek_shape_click
      selected_sek$Clicks <-c(click$id, selected_sek$Clicks) # selected$Clicks has to be added so that the vector select_dis has all selected districts in it
    })
    
    # creat a rective tibble with the selected school districts
    select_dis_sek <- reactive({district_geo_l %>%  filter(bezeichnun %in% selected_sek$Clicks)}) 
    
    
    # clear function if the unselect district is used  
    observeEvent(input$unselect_district_sek, {
      selected_sek$Clicks <- NULL}
    )
    
    # hovertext for the basemap
    mytext <- paste(
      "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
      lapply(htmltools::HTML)
    
    # define map with districts
    output$map_sek <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
        addPolygons(data=district_geo_l,
                    layerId=~bezeichnun,
                    fillColor = "transparent",
                    label = mytext)})
    
    
    # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different orgins 
    
    observe(
      # if no district is choosen show defined map 
      if(is.logical(selected_sek$Clicks)){
        leafletProxy("map_sek")
      }
      # if clear button is used show again defined map
      else if(is.null(selected_sek$Clicks)){
        leafletProxy("map_sek") %>% 
          setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
          addPolygons(data=district_geo_l,
                      layerId=~bezeichnun,
                      fillColor = "transparent",
                      label = mytext)
      }
      # if districts are clicked on fill them with blue
      else{
        mytext2 <- paste(
          # hovertext when at least one district is selected
          "Schulgemeinde: ", select_dis_sek()$bezeichnun,"<br/>")  %>%
          lapply(htmltools::HTML)
        
        leafletProxy("map_sek") %>% 
          addPolygons(data = select_dis_sek(),
                      layerId = ~bezeichnun,
                      fillColor = "blue",
                      label = mytext2)
      }
    )
    
    #* data ----
    # reactive dataframes / tibbles based on the selected district and year 
    
    # for distribution 
    daz_sek_d_reactive <- reactive({
      dplyr::filter(df_sek_daz, Schulgemeinde %in% select_dis_sek()$bezeichnun & Jahr >= input$d_Jahr_sek[1] & Jahr <= input$d_Jahr_sek[2])
    })
    
    # for amount of students, daz and classsize
    daz_sek_reactive <- reactive({
      dplyr::filter(df_sek_daz, Schulgemeinde %in% select_dis_sek()$bezeichnun & Jahr >= input$d_Jahr_sek[1] & Jahr <= input$d_Jahr_sek[2] & Bildungsart_2 %in% input$stufen_sek)
    })
    
    
    # # for citizienship 
    # df_sek_cs_reactive <- reactive({
    #   dplyr::filter(df_t_cs, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
    # })
    # 
    # df_t_dc_reactive <- reactive({
    #   dplyr::filter(df_t_dc, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
    # })
    # 
    # 
    # 
    # # create max and min for scale
    # # years
    j_sek_max <- reactive({max(daz_sek_reactive()$Jahr)})
    j_sek_min <- reactive({min(daz_sek_reactive()$Jahr)})
    
    # # #values
    s_sek_max <- reactive({max(daz_sek_reactive()$students)})
    # 
    # 
    # 
    
    #* Plots ----
    #** distribution ----
    #*col plot
    output$plot_sek_d <-  renderGirafe ({
      p_sek_d <-  daz_sek_d_reactive() %>% 
        mutate(Bildungsart_2 =  ordered(Bildungsart_2, levels = c("Sekundarstufe A", "Sekundarstufe B", "Sekundarstufe C"))) %>%  
        ggplot() +
        
        geom_col_interactive(aes(x = Jahr, y=per, fill = Bildungsart_2,
                                 tooltip = paste("Schulstufe:", Bildungsart_2, "\nAnteil Schüler:", daz_sek_d_reactive()$per, "%"),)) +
        # scale_fill_manual(values = col_countries) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_sek_min(), j_sek_max(), 1),
                           limits = c(j_sek_min()-1, j_sek_max()+0.5)) +
        facet_wrap(~Schulgemeinde) +
        ylab("") +
        ggtitle("Anteil der verschiedenen Sekundarstufen") +
        mytheme  
      
      girafe(code=print(p_sek_d), width_svg = 10)
    })
    
    
    
    
    
    
    
    #** Amount of Students-----
    #* Pint and Line plot
    output$plot_as_sek <-  renderGirafe ({
      p_sek_as <-  ggplot() +
        geom_point_interactive(daz_sek_reactive(), 
                               mapping = aes(x=Jahr, y=students, 
                                             color=factor(Schulgemeinde),
                                             size = 3,
                                             tooltip = paste("Schulkreis:", daz_sek_reactive()$Schulgemeinde, "\nAnzahl Schüler*innen:", daz_sek_reactive()$students)))+
        geom_line(daz_sek_reactive(), 
                  mapping = aes(x=Jahr, y=students, color = factor(Schulgemeinde))) +
        scale_color_manual(values = col_dis) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, s_sek_max()*1.1)) +
        scale_x_continuous(breaks = seq(j_sek_min(), j_sek_max(), 1),
                           limits = c(j_sek_min(), j_sek_max())) +
        
        ylab("") +
        ggtitle(paste("Total Anzahl Schüler*innen", input$stufen_sek, "(ohne Aufnahme- und Kleinklassen)")) +
        guides(size = "none") +
        mytheme
      
      girafe(code=print(p_sek_as), width_svg = 20)
    })
    
    
    #** DaZ ----
    # Point and Line
    output$plot_daz_sek <-  renderGirafe ({
      p_daz_sek <-  ggplot() +
        geom_point_interactive(daz_sek_reactive(), 
                               mapping = aes(x=Jahr, y=daz, 
                                             color=factor(Schulgemeinde),
                                             size = 3,
                                             tooltip = paste("Schulkreis:", daz_sek_reactive()$Schulgemeinde, "\nDaZ Anteil:", round(daz_sek_reactive()$daz*100,2), "%")))+
        geom_line(daz_sek_reactive(), 
                  mapping = aes(x=Jahr, y=daz, color = factor(Schulgemeinde))) +
        scale_color_manual(values = col_dis) +
        scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                           limits = c(0, 1)) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_sek_min(), j_sek_max(), 1),
                           limits = c(j_sek_min(), j_sek_max())) +
        ylab("") +
        ggtitle(paste("Anteil Schüler*innen, die Deutsch als Zweitsprache haben (DaZ), auf der Schulstufe", input$stufen_sek)) +
        guides(size = "none") +
        mytheme
      
      girafe(code=print(p_daz_sek), width_svg = 20)
    })
    
    #** Average class size-----  
    
    output$plot_acs_sek <-  renderGirafe ({
      p_acs_sek <-  ggplot(daz_sek_reactive(), aes(x=Jahr, y=acs, fill=Schulgemeinde)) +
        geom_col_interactive(aes(tooltip = paste("Schulkreis:", daz_sek_reactive()$Schulgemeinde, "\nDurchschnittliche Klassengrösse:", daz_sek_reactive()$acs)), position = position_dodge(width=0.9), width = 0.85, color=NA)+
        scale_y_continuous() +
        scale_fill_manual(values = col_dis) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_sek_min(), j_sek_max(), 1),
                           limits = c(j_sek_min()-1, j_sek_max()+0.5)) +
        ylab("") +
        ggtitle("Durchschnittliche Klassengrösse") +
        mytheme
      
      girafe(code=print(p_acs_sek), width_svg = 20)
    })
    
    
  
}

  

ui <-  shinyApp(ui = ui, server = server)