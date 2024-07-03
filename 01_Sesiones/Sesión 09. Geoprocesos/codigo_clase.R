
# Librerias ----
library(tidyverse)
library(sf)
library(cartogram)
library(leaflet)
library(openrouteservice)
library(osrm)
library(hexbin)
library(cartogram)
library(deldir)

# 1. Elabore un objeto sf con las coordenadas latitud/longitud del edificio del CONEVAL. Utilice el crs 4326 (lat/lon). 
punto_coneval <- tibble(latitud = coordenadas_coneval[1], 
                        longitud = coordenadas_coneval[2]) %>% 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = 4326)

# 2. Cambie el sistema de coordenadas de referencia al sistema 6362. 
punto_coneval_utm <- punto_coneval %>% 
  st_transform(crs = 6362)

# 3. Con este nuevo crs, elabore un buffer de un kilómetro alrededor del punto del CONEVAL. 
buffer_coneval <- st_buffer(punto_coneval_utm, dist = 1000) %>% 
  st_transform(crs = 4326)

leaflet(buffer_coneval) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons()

# 4. A partir de este buffer, obtenga todas las taquerías que se encuentran a un kilómetro alrededor del CONEVAL.
taquerias <- read_csv("01_Datos/taquerias.csv") %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
class(taquerias) # Si es sf!

taquerias_coneval <- st_intersection(taquerias, buffer_coneval) 

taquerias_coneval %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = buffer_coneval, color = "gray") %>% 
  addCircleMarkers()

# 5. Elabore un mapa en leaflet donde las taquerías tomen un color dependiendo del número de empleados que tienen. 
unique(taquerias_coneval$per_ocu)
pal_taquerias <- colorFactor(domain = taquerias_coneval$per_ocu, 
                             palette = wesanderson::wes_palettes$Zissou1)

taquerias_coneval %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = buffer_coneval, color = "gray") %>% 
  addCircleMarkers(color = pal_taquerias(taquerias_coneval$per_ocu)) %>% 
  addLegend(title = "Taquerías por personal ocupado", 
            pal = pal_taquerias, values = taquerias_coneval$per_ocu, 
            position = "bottomright")

# 6. Ahora, trabajemos con el archivo de colonias y de secciones de la CDMX. 
# Dadas las coordenadas del CONEVAL, determine en qué sección electoral y en qué colonia se encuentra ubicado el edificio del CONEVAL. 

colonias <- read_sf("01_Datos/georef-mexico-colonia.geojson") %>% st_make_valid()
secciones <- read_sf("01_Datos/secciones_electorales_cdmx.geojson")

colonia_coneval <- st_intersects(punto_coneval, colonias)[[1]]
seccion_coneval <- st_intersects(punto_coneval, secciones)[[1]]

colonia_coneval <- colonias[colonia_coneval,]
seccion_coneval <- secciones[seccion_coneval,]

colonia_coneval %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = "purple") %>% 
  addPolygons(data = seccion_coneval, color = "red") %>% 
  addMarkers(data = punto_coneval)

# 7. Ahora, extraiga los polígonos de las colonias que se encuentran a un kilómetro a la redonda del CONEVAL. ¿Cuál es la colonia más grande?
colonias_buffer_coneval <- st_intersects(buffer_coneval, colonias)[[1]]
colonias_buffer_coneval <- colonias[colonias_buffer_coneval,]

colonias_buffer_coneval %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = buffer_coneval) %>% 
  addPolygons(color = "purple", label = colonias_buffer_coneval$col_name) 
  
# colonia_mas_grande <- 
  colonias_buffer_coneval %>% 
    mutate(areas = st_area(.)) %>% 
    arrange(-areas) %>% 
    slice(1) %>% 
    pull(col_name) %>% 
    str_squish()
  
  colonias_buffer_coneval %>% 
    transmute(col_name, areas = st_area(.)) 
  
  
# 8. ¿Qué es más grande, una sección o una colonia? Para todas las colonias que están a un kilómetro del CONEVAL, determine a que secciones irán a votar sus habitantes.   
secciones_coneval_colonias <- st_intersects(colonias_buffer_coneval, secciones) %>% 
  unlist() %>% 
  unique()

secciones_coneval_colonias <- secciones[secciones_coneval_colonias,]

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = secciones_coneval_colonias, color = "green", 
              label = secciones_coneval_colonias$seccion)


# 9. Suponga que quiere ir del CONEVAL a la pizzería CANCINO, ubicada en la glorieta de las Cibeles, en la Roma Norte. ¿Cuál sería la distancia desde el CONEVAL hasta dicha pizzería, en línea recta?

punto_cancino <- tibble(latitud = 19.420310541166778, 
                        longitud = -99.16695565395658) %>% 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = 4326)


st_distance(punto_cancino, punto_coneval) # 3,45 km en línea recta


# 10. ¿Y siguiendo la ruta en la calle? 


trayecto_cancino_coneval <- osrm::osrmRoute(punto_coneval, punto_cancino)
trayecto_cancino_coneval_utm <- st_transform(trayecto_cancino_coneval, crs = 6362)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(data = trayecto_cancino_coneval)

trayecto_cancino_coneval

# 11. Calcule las isócronas de 30 minutos para el CONEVAL, tanto para ir caminando como para ir en coche. ¿Si llega a dicha pizzería en 30 minutos, o mejor nos vamos a otro lado? 
punto_partida <- c(st_coordinates(punto_coneval)[,"X"], st_coordinates(punto_coneval)[,"Y"])
isocronas <- ors_isochrones(locations =  list(punto_partida),
                            profile = "driving-car",
                            range = c(1800), 
                            output = "sf") # 600s y 1200s (10 y 20 minutos)
plot(isocronas, max.plot = 1)

# 60*30

isocronas %>% 
  leaflet() %>%
  addTiles() %>% 
  addMarkers(data = punto_coneval) %>% 
  addMarkers(data = punto_cancino) %>% 
  addPolygons()

# 12. Elabore un mapa hexbin de las taquerías de la Ciudad de México. 
taquerias <- taquerias %>% 
  cbind(taquerias %>% 
          st_coordinates() %>% 
          as_tibble())

municipios <- st_read("01_Datos/municipios_2022.geojson") %>% 
  filter(CVE_ENT == "09")

ggplot() + 
  geom_hex(data = taquerias %>%
             as_tibble() %>%
             select(-geometry), 
           aes(x = X, y = Y, fill = ..count..),
           bins = 50) + 
  geom_sf(data = municipios, fill = NA, color = "red") +
    scale_fill_viridis_c() +
    theme_minimal()
  

# 13. Elabore un cartograma de Dorling con el tamaño de la Lista Nominal de cada uno de los municipios del estado de Puebla.
munis_puebla <- st_read("01_Datos/municipios_2022.geojson") %>% 
  filter(CVE_ENT == "21")
datos_prep <- read_csv("01_Datos/prep_municipal.csv")
mapx <- left_join(munis_puebla, datos_prep, by = c("CVEGEO" = "CVE_INEGI")) %>% 
  st_transform(crs =6362 )

dorling <- cartogram_dorling(x = mapx, weight = "lista_nominal" ) %>% 
  st_transform(crs = 4326) %>% 
  mutate(X = st_centroid(.) %>% st_coordinates() %>% as_tibble() %>% pull("X"), 
         Y = st_centroid(.) %>% st_coordinates() %>% as_tibble() %>% pull("Y")) %>% 
  mutate(etiqueta = ifelse(lista_nominal > 200000, yes = str_c(NOMGEO, "\n", prettyNum(lista_nominal, big.mark = ",")), no = ""))

dorling %>% 
  ggplot() +
  geom_sf(data = munis_puebla, fill = "yellow") + 
  geom_sf(fill = "red") + 
  geom_text(aes(x = X, 
                y = Y,
                label = etiqueta), 
            color = "black", 
            size = 3,
            family = "Arial")

# 14. Elabore un mapa de polígonos de Voronoi para determinar que Costcos están más cerca de cada punto de la Ciudad de México. 
costcos <- read_csv("01_Datos/costcos.csv") %>%
  st_as_sf(coords = c("longitud", "latitud"),
           crs = 4326) 
v <- deldir(costcos_csv$longitud, costcos_csv$latitud)

voronoi_to_sf <- function(voronoi) {
  library(sp)
  library(dplyr)
  tiles <- tile.list(voronoi)
  polys <- lapply(tiles, function(tile) {
    coords <- cbind(tile$x, tile$y)
    poly <- Polygon(coords)
    Polygons(list(poly), ID = tile$ptNum)
  })
  sp_polys <- SpatialPolygons(polys)
  sf_polys <- st_as_sf(sp_polys) 
  st_crs(sf_polys) <- 4326
  return(sf_polys)
}

poligonos_cdmx <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  filter(CVE_EDO == "09")

poligonos_voronoi <- voronoi_to_sf(voronoi = v)

voronois_cdmx <- st_intersection(poligonos_voronoi, poligonos_cdmx)
costcos_cdmx <- st_intersection(costcos, poligonos_cdmx)

voronois_cdmx %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addCircleMarkers(data = costcos_cdmx,
                   color = "red")
