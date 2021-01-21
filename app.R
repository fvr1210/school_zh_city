library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(tidyverse)
library(shinydashboard)
library(ggiraph)
library(RColorBrewer)

# loading own functions for repeating graphs, using eval to use encoding https://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse("scripts/graph_functions.R", encoding="UTF-8"))


# read in data ----


#* Geo data for Zurich city school districts ----
district_geo <- read_sf("geo-data/stzh.adm_schulkreise_a_polygon.shp") 

# change swiss coordinate system to lat long
district_geo_l  <- district_geo %>%  
  sf::st_transform('+proj=longlat +datum=WGS84')

#* Total Data ----
#* Including all School levels
#** Data to esd, amount of students and average class size ----
df_t_dsa <- read_delim("processed-data/districts_total_esd_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8')) 

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
#** Data to esd, amount of students and average class size ----
df_kp_dsa <- read_delim("processed-data/districts_kinder_primar_esd_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8'))  %>% 
  mutate(Schulart =  ordered(Schulart, levels = c("Kindergarten", "Übergangsklassen", "Primarschule")))

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
df_sek_dsa <- read_delim("processed-data/districts_sek_esd_students_acs.csv", delim = ";", locale = locale(encoding = 'utf-8')) %>% 
  mutate(Bildungsart_2 =  ordered(Bildungsart_2, levels = c("Sekundarstufe A", "Sekundarstufe B", "Sekundarstufe C"))) 




#* Info text

info_text <-  read_file("processed-data/info_text.html", locale = locale(encoding = 'utf-8')) # text description for tab with all school levels






# hovertext for the basemap ----
mytext <- paste(
  "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>")  %>%
  lapply(htmltools::HTML)


# define theme ----
# for "normal" plots
mytheme <- theme_bw() +
    theme(plot.title = element_text(size = 15, face = "bold",  margin = margin(0, 0, .05, 0., "cm")),
          axis.text.x = element_text(angle = 90, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_blank(),
          plot.caption = element_text(size = 14),
          legend.position = "bottom")

# for facet wrap plots
mytheme_facet <- theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold",  margin = margin(0, 0, .05, 0., "cm")),
        plot.subtitle = element_text(size = 7),
        axis.text.x = element_text(angle = 90, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        plot.caption = element_text(size = 7),
        legend.position = "bottom")

# colors for districts -----    
col_dis <- c( "Glattal" = "#d10070", "Letzi" = "#5D3D55", "Limmattal" = "#3F5C75", "Schwamendingen" = "#177972", "Uto" = "#548D51", "Waidberg" = "#A69439", "Zürichberg" = "#F48E5F") 

# color for countries
nb.cols <- 18
col_countries <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)


# Ui side -----
ui <- dashboardPage(skin = "blue",
                     dashboardHeader(title = "Schulboard", titleWidth = 320 # extend width because of the longer title
                     ),
                    
                    # sidebare ---------
                     dashboardSidebar( 
                       width = 320,
                       sidebarMenu(
                         menuItem("Info", tabName = "info_text", icon = icon("info")),
                         menuItem("Volksschulstufen aggregiert", tabName = "total", icon = icon("school")),    
                         menuItem("Kindergarten, Übergangsklassen, Primarschule", tabName = "unterstufe", icon = icon("child")), 
                         menuItem("Sekundarschule", tabName = "sek", icon = icon("book"))),
                         
                         tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
                             tags$head(
                                 tags$style(
                                     HTML(                   # Change position of different elements 
                                         ".checkbox-inline {    
                    margin-left: 10px;
                    margin-right: 10px;
                    }",
                                         
                    
                                         ".shiny-input-radiogroup{ 
                    margin-left: 10;
                    margin-top: -500px;
                    margin-bottom: 15px;
                    }",
                                         ".shiny-input-container{ 
                    margin-top: 0px;
                    margin-left: 0px;
                    margin-bottom: 20px;
                    }",
                                         ".shiny-options-group{ 
                    margin-left: 15px;
                    }",
                                         
                                       ".control-label{ 
                    margin-left: 0px;
                    }",
                    
                    ".shiny-output-error { visibility: hidden; }",         # not displaying errors on the dashboard
                                         
                    ".shiny-output-error:before { visibility: hidden; }",  # not displaying errors on the dashboard
                    "text {
                      font-family: helvetica,arial,sans-serf;
                    }"
                                     ))))         
                     ),
                     
                     
                     #body ----
                     dashboardBody(
                       tabItems(
                      #* total ----
                      #* 
                      tabItem(tabName = "info_text",
                              fluidPage(
                              htmlOutput("info_text"))),
                      
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
                                br(),
                                sliderInput(inputId="d_Jahr_t", label = 'Beobachtungszeitraum wählen:',
                                            min=min(df_t_dsa$Jahr), max=max(df_t_dsa$Jahr), value=c(min(df_t_dsa$Jahr), max(df_t_dsa$Jahr)), sep = ""),
                                girafeOutput("plot_as_t", width = "100%", height = "350px"),
                                girafeOutput("plot_esd_t", width = "100%", height = "350px"),
                                girafeOutput("plot_dc_t", width = "100%", height = "350px"),
                                girafeOutput("plot_cs_t", width = "100%", height = "1000px"),
                                
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
                           br(),
                           sliderInput(inputId="d_Jahr_kp", label = 'Beobachtungszeitraum wählen:',
                                       min=min(df_t_dsa$Jahr), max=max(df_t_dsa$Jahr), value=c(min(df_t_dsa$Jahr), max(df_t_dsa$Jahr)), sep = ""),
                           girafeOutput("plot_kp_d", width = "100%", height = "800px"),
                           radioButtons(inputId = "stufen_kp", label = "Schulstufen:",
                                              choices = c(
                                                "Kindergarten",
                                                "Übergangsklassen",
                                                "Primarschule"),
                                              selected = c("Kindergarten"),
                                              inline = T
                           ),
                           girafeOutput("plot_as_kp", width = "100%", height = "350px"),
                           girafeOutput("plot_acs_kp", width = "100%", height = "350px"),
                           girafeOutput("plot_esd_kp", width = "100%", height = "350px"),
                          
                           
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
                               br(),
                               sliderInput(inputId="d_Jahr_sek", label = 'Beobachtungszeitraum wählen:',
                                           min=min(df_t_dsa$Jahr), max=max(df_t_dsa$Jahr), value=c(min(df_t_dsa$Jahr), max(df_t_dsa$Jahr)), sep = ""),
                               girafeOutput("plot_sek_d", width = "100%", height = "800px"),
                               radioButtons(inputId = "stufen_sek", label = "Schulstufen:",
                                            choices = c(
                                              "Sekundarstufe A",
                                              "Sekundarstufe B",
                                              "Sekundarstufe C"),
                                            selected = c("Sekundarstufe A"),
                                            inline = T
                               ),
                               girafeOutput("plot_as_sek", width = "100%", height = "350px"),
                               girafeOutput("plot_esd_sek", width = "100%", height = "350px"),
                               ))),
                    ),
                     )



server <- function(input, output) {
  
  
# Info ----  
#**************************************************************************************************************************************************************************************************************  
  
  #*  Text ----
  # Introduction text 
  output$info_text <- renderUI({HTML(info_text)})

# Total ----  
#**************************************************************************************************************************************************************************************************************  
  #*  Text ----
  # Introduction text 
  output$total_text <- renderUI({HTML(total_text)})
  # Choosing districts text
  output$choosing_t <- renderUI({HTML("<h4>Wähle auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
  
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
  

  
  # 
  #  map with districts
  output$map_t <- renderLeaflet({map_function(district_geo_l, "unselect_district_t", mytext) 
  })
  
  
  
  # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different sources 
  
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
  
  # for students, esd and average class size
  dsa_t_reactive <- reactive({
    dplyr::filter(df_t_dsa, Schulgemeinde %in% select_dis_t()$bezeichnun & Jahr >= input$d_Jahr_t[1] & Jahr <= input$d_Jahr_t[2])
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
  j_t_max <- reactive({max(dsa_t_reactive()$Jahr)})
  j_t_min <- reactive({min(dsa_t_reactive()$Jahr)})
  
  #values
  s_t_max <- reactive({max(dsa_t_reactive()$students)})
  
  
  
  
  #* Plots ----
  #** Amount of Students-----
  #* Pont and Line plot
  output$plot_as_t <- renderGirafe ({
    p_t_as <- students_function(dsa_t_reactive, col_dis, mytheme, s_t_max, j_t_min, j_t_max, "Kindergarten-, Primar- und Sekundarstufe aggregiert")
    girafe(code=print(p_t_as), width_svg = 20)
  })
  
  
#** DaE ----
  # Point and Line
    output$plot_esd_t <-  renderGirafe ({
    p_esd_t <- esd_function(dsa_t_reactive, col_dis, mytheme, j_t_min, j_t_max, "Kindergarten-, Primar- und Sekundarstufe aggregiert")
   
    girafe(code=print(p_esd_t), width_svg = 20)
  })

  
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
      ggtitle("Anzahl verschiedener Nationalitäten (ohne Aufnahme- und Kleinklassen, keine Daten für 2019)") +
      labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
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
      labs(title = 'Staatsangehörigkeiten der Schüler*innen',
           subtitle = 'Länder die weniger als 2 Prozent ausmachen, sind in der Gruppe "Andere" zusammengefasst (keine Daten für 2019)',
           caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
      ylab("") +
      mytheme_facet  
    
    girafe(code=print(p_cs_t), width_svg = 10)
  })
  

  
  
  
  
  
  
  
  
# Unterstufe  -----
#**************************************************************************************************************************************************************************************************************  
  
      
# text which is displayed above the map
  #*  Text ----
  # Introduction text 
  output$kp_text <- renderUI({HTML(kp_text)})
  # Choosing districts text
  output$choosing_kp <- renderUI({HTML("<h4>Wähle auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
  
  
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
    

    # 
    #  map with districts
    output$map_kp <- renderLeaflet({map_function(district_geo_l, "unselect_district_kp", mytext) 
    })
    
    
    
    # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different sources 
    
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
    dsa_kp_d_reactive <- reactive({
      dplyr::filter(df_kp_dsa, Schulgemeinde %in% select_dis_kp()$bezeichnun & Jahr >= input$d_Jahr_kp[1] & Jahr <= input$d_Jahr_kp[2])
    })
    
    # for amount of students, esd and classsize
    dsa_kp_reactive <- reactive({
      dplyr::filter(df_kp_dsa, Schulgemeinde %in% select_dis_kp()$bezeichnun & Jahr >= input$d_Jahr_kp[1] & Jahr <= input$d_Jahr_kp[2] & Schulart %in% input$stufen_kp)
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
    j_kp_max <- reactive({max(dsa_kp_reactive()$Jahr)})
    j_kp_min <- reactive({min(dsa_kp_reactive()$Jahr)})

    # # #values
    s_kp_max <- reactive({max(dsa_kp_reactive()$students)})
    # 
    # 
    # 
    
    #* Plots ----
    #** distribution ----
    #*col plot
    output$plot_kp_d <-  renderGirafe ({
      p_kp_d <- dis_function(dsa_kp_d_reactive, Schulart, mytheme_facet, j_kp_min, j_kp_max, "Anteil Kindergarten, Übergangsklassen und Primarschule (ohne Aufnahme- und Kleinklassen, nach 2013 gibt es keine Übergangsklassen in der Stadt Zürich)")
      girafe(code=print(p_kp_d), width_svg = 10)
    })
    
        # ggplot() +
        # 
        # geom_col_interactive(aes(x = Jahr, y=per, fill = Schulart,
        #                          tooltip = paste("Schulstufe:", Schulart, "\nAnteil Schüler:", dsa_kp_d_reactive()$per, "%"),)) +
        # scale_y_continuous(labels = function(x) paste0(x, "%")) +
        # scale_x_continuous() +
        # scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
        #                    limits = c(j_kp_min()-1, j_kp_max()+0.5)) +
        # facet_wrap(~Schulgemeinde) +
        # ylab("") +
        # ggtitle() +
        # labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
        # mytheme_facet  
      
    
    
    
    
    
    
    
    #** Amount of Students-----
    output$plot_as_kp <- renderGirafe ({
      p_kp_as <- students_function(dsa_kp_reactive, col_dis, mytheme, s_kp_max, j_kp_min, j_kp_max, input$stufen_kp)
      girafe(code=print(p_kp_as), width_svg = 20)
    })
      
   
    #** DaE ----
    # Point and Line
    
    output$plot_esd_kp <-  renderGirafe ({
      p_esd_kp <- esd_function(dsa_kp_reactive, col_dis, mytheme, j_kp_min, j_kp_max, input$stufen_kp)
      girafe(code=print(p_esd_kp), width_svg = 20)
    })
    

    
    #** Average class size-----  
    
    output$plot_acs_kp <-  renderGirafe ({
      p_acs_kp <-  ggplot(dsa_kp_reactive(), aes(x=Jahr, y=acs, fill=Schulgemeinde)) +
        geom_col_interactive(aes(tooltip = paste("Schulkreis:", dsa_kp_reactive()$Schulgemeinde, "\nDurchschnittliche Klassengrösse:", dsa_kp_reactive()$acs)), position = position_dodge(width=0.9), width = 0.85, color=NA)+
        scale_y_continuous() +
        scale_fill_manual(values = col_dis) +
        scale_x_continuous() +
        scale_x_continuous(breaks = seq(j_kp_min(), j_kp_max(), 1),
                           limits = c(j_kp_min()-1, j_kp_max()+0.5)) +
        ylab("") +
        ggtitle("Durchschnittliche Klassengrösse (ohne Aufnahme- und Kleinklassen)") +
        labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
        mytheme
      
      girafe(code=print(p_acs_kp), width_svg = 20)
    })
    
    
    
    # Sekundarstufe  -----
    #**************************************************************************************************************************************************************************************************************  
    
    
    #*  Text ----
    # Introduction text 
    output$sek_text <- renderUI({HTML(sek_text)})
    # Choosing districts text
    output$choosing_sek <- renderUI({HTML("<h4>Wähle auf den Karten die Schulkreise aus, die Sie interessieren. Das Laden der Daten braucht etwas Zeit.</h4>")})
    
    #* Map ----
    # store the click, an highlight the clicked districts 
    # used code from https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/ 
    # https://gis.stackexchange.com/questions/215342/changing-the-style-of-a-polygon-with-a-click-event-in-a-shiny-leaflet-app
    # https://stackoverflow.com/questions/41106547/how-to-save-click-events-in-leaflet-shiny-map#41106795
    
   selected_sek<-reactiveValues(Clicks=vector()) # creat a vector where clicks will be stored

    # save selected clicks in the vector created above
    observeEvent(input$map_sek_shape_click,{
      selected_sek$Clicks
      click <- input$map_sek_shape_click
      selected_sek$Clicks <-c(click$id, selected_sek$Clicks) # selected_sek$Clicks has to be added so that the vector selected_sek has all selected districts in it
    })
   
    # creat a rective tibble with the selected school districts
    select_dis_sek <- reactive({district_geo_l %>%  filter(bezeichnun %in% selected_sek$Clicks)})


    # clear function if the unselect district is used  
    observeEvent(input$unselect_district_sek, {
      selected_sek$Clicks <- NULL}
    )

    # 
    #  map with districts
    output$map_sek <- renderLeaflet({map_function(district_geo_l, "unselect_district_sek", mytext) 
                                        })
      

    # code which is used to unselect all selected clicks - this is made from different codes, sadly can't remember the different sources 
    
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
    dsa_sek_d_reactive <- reactive({
      dplyr::filter(df_sek_dsa, Schulgemeinde %in% select_dis_sek()$bezeichnun & Jahr >= input$d_Jahr_sek[1] & Jahr <= input$d_Jahr_sek[2])
    })
    
    # for amount of students, esd and classsize
    dsa_sek_reactive <- reactive({
      dplyr::filter(df_sek_dsa, Schulgemeinde %in% select_dis_sek()$bezeichnun & Jahr >= input$d_Jahr_sek[1] & Jahr <= input$d_Jahr_sek[2] & Bildungsart_2 %in% input$stufen_sek)
    })
    
 
    # # create max and min for scale
    # # years
    j_sek_max <- reactive({max(dsa_sek_reactive()$Jahr)})
    j_sek_min <- reactive({min(dsa_sek_reactive()$Jahr)})
    
    # # #values
    s_sek_max <- reactive({max(dsa_sek_reactive()$students)})
    # 
    # 
    # 
    
    #* Plots ----
    #** distribution ----
    #*col plot
    output$plot_sek_d <-  renderGirafe ({
      p_sek_d <- dis_function(dsa_sek_d_reactive, Bildungsart_2, mytheme_facet, j_sek_min, j_sek_max, "Anteil der verschiedenen Sekundarstufen (nach 2010 gibt es keine Sekundarstufe C in der Stadt Zürich)")
      girafe(code=print(p_sek_d), width_svg = 10)
    })
    
  
    #** Amount of Students-----
    #* Pint and Line plot

    
    output$plot_as_sek <- renderGirafe ({
      p_sek_as <- students_function(dsa_sek_reactive, col_dis, mytheme, s_sek_max, j_sek_min, j_sek_max, input$stufen_sek)
      girafe(code=print(p_sek_as), width_svg = 20)
    })
    
    
    #** DaE ----
    # Point and Line
    
    output$plot_esd_sek <-  renderGirafe ({
      p_esd_sek <- esd_function(dsa_sek_reactive, col_dis, mytheme, j_sek_min, j_sek_max, input$stufen_sek)
      girafe(code=print(p_esd_sek), width_svg = 20)
    })
    

    
  
}

  

ui <-  shinyApp(ui = ui, server = server)