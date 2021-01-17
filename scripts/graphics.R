library(tidyverse)
library(treemapify)
library(ggmap)
library(sp)
library(leaflet)
library(lubridate)


# using function for crating several plots https://stackoverflow.com/questions/51410553/generate-multiple-plots-from-generic-code-in-r
example_plot <- function(x){
  ggplot(x, aes(country_g, per)) +
    geom_col() +
    facet_grid(Jahr~.) +
    ggtitle(x$SG)
}

graphs <- df_cl_c %>% 
  group_by(Schulgemeinde, Jahr) %>% 
  nest() %>% 
  mutate(graph = map(data, ~ example_plot(.x))) %>% 
  pull(graph)

# Treeplot https://wilkox.org/treemapify/ 

tree_plot <- function(x){
  ggplot(x, aes(area = per_g, label = country_g, fill = country_g)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", 
                    grow = TRUE)+
    ggtitle(paste(x$SG)) # weiis noch nicht wie jahr reinnehmen
}


tree <- df_cl_cr %>% 
  mutate(Jahr2=Jahr) %>% 
  group_by(Schulgemeinde, Jahr2) %>% 
  nest() %>% 
  mutate(graph = map(data, ~ tree_plot(.x))) %>% 
  pull(graph)

df_test <- df_cl_cr[1:300,]

df_test$country_g <- fct_reorder(df_test$country_g, -df_test$rank, max)

df_test <- df_test %>% mutate(t2 = fct_reorder(country_g, rank, max))

ggplot(df_test, aes(area = per_g, label = country_g, fill = country_g)) +
  geom_treemap() +
  facet_wrap( ~Jahr + Schulgemeinde) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", 
                    grow = TRUE) +
  theme(legend.position = "none")
  
ggplot(arrange(df_test, rank), aes(x = Schulgemeinde, y=per_g, fill = country_g)) +
  geom_col() + 
  facet_wrap( ~Jahr) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Blues")
  



# # trying to make an gif 
# 
# library(gganimate)
# library(gapminder)
# 
# 
# test <- df_cl_cr %>% filter(Schulgemeinde=="Glattal")
# 
# p <- ggplot(test, aes(
#   label = country_g,
#   area = per_g, 
#   fill = country_g
# )) +
#   geom_treemap(layout = "fixed") +
#   geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
#   transition_time(Jahr) +
#   ease_aes('linear') +
#   labs(title = "Year: {frame_time}")
# 
# 
# animate(p, nframes = 300, fps = 5, end_pause = 20,renderer=gifski_renderer("test.gif"))


# loading school district boarders 
district_geo <- read_sf("raw-data/geo_school_districts_zh_city/districts/stzh.adm_schulkreise_a_polygon.shp") 

# names and positions
di_points <- sf::st_point_on_surface(district_geo)
cords_di <- as.data.frame(sf::st_coordinates(di_points))
cords_di$NAME <- district_geo$bezeichnun 

# register with api at google to use map layer  
register_google("AIzaSyBvdge6SJysb8BreGqSzhaDLJK9NbmmvQI")

# add map background https://cengel.github.io/R-spatial/mapping.html
zh_basemap <- get_map(location=c(lon = 8.53911613353666 , lat = 47.37 ), zoom=12, source = 'osm')


# changing cords_di in lon, lat format (idea and comments from https://gis.stackexchange.com/questions/155233/transforming-coordinate-in-swiss-reference-system-lv95-into-latlong-wgs84)

# We create a Spatial points data frame for our data, where X and Y are the fields with coordinates
coordinates(cords_di) <- ~ X + Y

# We tell R our data it's on LV95, EPSG=2056
cords_di@proj4string <- CRS("+init=epsg:2056")

# Then we transform the data to WGS84
cords_di2 <- spTransform(cords_di, CRS("+init=epsg:4326"))

# We can see the transformed coords
cords_di <- as_tibble(cords_di2@coords)
cords_di$NAME <- district_geo$bezeichnun

# creat static map ----
district_map <- ggmap(zh_basemap) +
  geom_sf(
    data = district_geo,
    fill = "transparent",
    color = "black",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  coord_sf(crs = st_crs(4326)) +
  geom_text(cords_di, mapping = aes(X, Y, label = NAME), colour = "black", size=3, fontface = "bold") +
  theme_bw()+
  theme_void()

district_map


# create interactive maps ----

# change swiss coordinate system to lat long
district_geo_l  <- district_geo %>%  
  sf::st_transform('+proj=longlat +datum=WGS84')

mytext <- paste(
  "Schulgemeinde: ", district_geo_l$bezeichnun,"<br/>"
  # "Area: ", world_spdf@data$AREA, "<br/>", 
  # "Population: ", round(world_spdf@data$POP2005, 2), 
  # sep=""
    )  %>%
  lapply(htmltools::HTML)


map1 <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=district_geo_l, 
              fillColor = "transparent",
              highlight = highlightOptions(
                color = "darkblue",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = mytext)  
  # addMarkers(data = cords_di, ~X, ~Y, label = ~htmlEscape(NAME))
  # 
  # 


ggplot(df_daz, aes(x=Jahr, y=DAZ, filter=Schulgemeinde))+
  geom_col(position = position_dodge(width=0.9), width = 0.85)+
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous() +
  ylab("Anteil DAZ Kinder")



