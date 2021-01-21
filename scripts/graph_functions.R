#Map -------

# reactive Map for choosing district 
# codes from https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event and https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/


map_function <- function(district_geo_l, .unselect, mytext){
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.52 ,lat =47.38, zoom = 12)%>%
      addPolygons(data=district_geo_l,
                  layerId=~bezeichnun,
                  fillColor = "transparent",
                  label = mytext)%>%
      addControl(actionButton(.unselect," Schulkreis Auswahl löschen",
                              style="color: #fff;
                                        background-color: #428bca;
                                        border-color: #428bca"),
                 position="topright",
                 className = "fieldset {
                   border: 0;
                   }"
      )
}


# Function for point and line graph which shows the amount of students for the different school districts -------
students_function <- function(.df, .students_color, .students_theme, .students_y_max, .students_x_min, .students_x_max, .title_input){    
  ggplot() +
    geom_point_interactive(.df(), 
                           mapping = aes(x=Jahr, y=students, 
                                         color=factor(Schulgemeinde),
                                         size = 3,
                                         tooltip = paste("Schulkreis:", .df()$Schulgemeinde, "\nAnzahl Schüler*innen:", .df()$students)))+
    geom_line(.df(), 
              mapping = aes(x=Jahr, y=students, color = factor(Schulgemeinde))) +
    scale_color_manual(values = .students_color) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, .students_y_max()*1.1)) +
    scale_x_continuous(breaks = seq(.students_x_min(), .students_x_max(), 1),
                       limits = c(.students_x_min(), .students_x_max())) +
    
    ylab("") +
    ggtitle(paste("Total Anzahl Schüler*innen", .title_input, "(ohne Aufnahme- und Kleinklassen)")) +
    labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
    guides(size = "none") +
    .students_theme

}



# Function for point and line graph which shows the DaE Ratio for the different school districts ---------
esd_function <- function(.df, .esd_color, .esd_theme, .esd_x_min, .esd_x_max, .title_input){    
  ggplot() +
    geom_point_interactive(.df(), 
                           mapping = aes(x=Jahr, y=esd, 
                                         color=factor(Schulgemeinde),
                                         size = 3,
                                         tooltip = paste("Schulkreis:", .df()$Schulgemeinde, "\nDaE Anteil:", round(.df()$esd*100,2), "%")))+
    geom_line(.df(), 
              mapping = aes(x=Jahr, y=esd, color = factor(Schulgemeinde))) +
    scale_color_manual(values = .esd_color) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(.esd_x_min(), .esd_x_max(), 1),
                       limits = c(.esd_x_min(), .esd_x_max())) +
    
    ylab("") +
    ggtitle(paste("Anteil Schüler*innen, die Deutsch als Erstsprache (DaE) haben, auf der Schulstufe", .title_input, "(ohne Aufnahme- und Kleinklassen)")) +
    labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
    guides(size = "none") +
    .esd_theme
}


# Function for col graph which shows the distribution of students for the different school levels and school districts ---------
dis_function <- function(.df, .var, .dis_theme, .dis_x_min, .dis_x_max, .title_input){ 
ggplot() +
  geom_col_interactive(.df(),
    mapping = aes(x = Jahr, y=per, fill = {{ .var }},      # I have to use double curly braces https://www.infoworld.com/article/3410295/how-to-write-your-own-ggplot2-functions-in-r.html 
                           tooltip = paste("Schulstufe:", {{ .var }}, "\nAnteil Schüler:", .df()$per, "%"),)) +   
  # scale_fill_manual(values = col_countries) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous() +
  scale_x_continuous(breaks = seq(.dis_x_min(), .dis_x_max(), 1),
                     limits = c(.dis_x_min()-1, .dis_x_max()+0.5)) +
  facet_wrap(~Schulgemeinde) +
  ylab("") +
  ggtitle(.title_input) +
  labs(caption = "Daten: Bildungsstatistik Zürich, bearbeite durch Flavio von Rickenbach, Grafik: Flavio von Rickenbach, CC-BY 4.0") +
  .dis_theme 
}
