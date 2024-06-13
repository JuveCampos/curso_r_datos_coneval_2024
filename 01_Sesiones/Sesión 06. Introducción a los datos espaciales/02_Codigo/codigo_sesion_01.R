# Librerias: 
library(sf)
library(readxl)
library(tidyverse)

# 01. Cargamos el geojson de internet. 
# Recordemos que los geojsons son archivos de texto plano, y que se pueden cargar desde internet: 
estados <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
plot(estados) # Gráfica exploratoria: 

# Ejercicio 01.  Cargue el excel de Delitos 2019 y, a partir de las coordenadas de latitud y longitud, conviertan en un objeto sf. Grafique.
delitos_2019 <- read_xlsx("01_datos/delitos_2019.xlsx")  # Cargamos el excel de los delitos en la CDMX
class(delitos_2019) # Verificamos que no es clase sf

# Convertimos a clase sf, para poder hacer mapas con la información: 
delitos_2019_shp <- delitos_2019 %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
plot(delitos_2019_shp, max.plot = 1) # Graficamos. 

# 2. Cargue los polígonos de las alcaldías (alcaldias.kml). Explore los datos. identifique geometrías y atributos. Grafique

# Cargamos los datos de las alcaldías: 
alcaldias <- read_sf("01_datos/alcaldias.kml")
plot(alcaldias, max.plot = 1) # Graficamos

# Hacemos el mapa en ggplot de alcaldías y municipios: 
alcaldias %>%
  ggplot() + 
  geom_sf() + 
  geom_sf(data = delitos_2019_shp, 
          alpha = 0.01)

# Lo tuneamos: 
alcaldias %>%
  ggplot() + 
  geom_sf() + 
  geom_sf(data = delitos_2019_shp, 
          alpha = 0.01,
          color = "red") + 
  labs(title = "Distribución de la ubicación de los\ndelitos en la Ciudad de México") + 
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        plot.title = element_text(family = "Arial", face = "bold", hjust = 0.5, size = 20))

delitos_2019_shp <- delitos_2019_shp %>% 
  mutate(x = st_coordinates(delitos_2019_shp)[,1], 
         y = st_coordinates(delitos_2019_shp)[,2])

# Más tuneo: 
ggplot(delitos_2019_shp) +
  geom_sf(data = alcaldias, fill = "beige", linetype = 2) + 
  stat_density_2d(aes(fill = ..level.., x=x, y=y), geom = "polygon", colour="white") + 
  scale_fill_gradientn(colors = wesanderson::wes_palettes$Zissou1) + 
  labs(title = "Distribución de la ubicación de los\ndelitos en la Ciudad de México", 
       x = NULL, y = NULL, fill = "Nivel de delitos") + 
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "bottom",
        plot.title = element_text(family = "Arial", face = "bold", 
                                  hjust = 0.5, size = 20, 
                                  color = "navyblue")) + 
  guides(fill = guide_colorbar(barwidth = 20, 
                               barheight = 0.5, 
                               title.position = "top", 
                               title.hjust = 0.5))


# Mapa de delitos para la Cuauhtémoc

delitos_cuautemoc <- delitos_2019_shp %>%
  filter(!(AlcaldiaHechos %in% c("CUAUHTEMOC", "IZTAPALAPA")))

alcaldias %>% 
  filter(!(Name %in% c("Cuauhtémoc", "Iztapalapa"))) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = delitos_cuautemoc, 
          alpha = 0.01, 
          color = "red") 

# Municipios 
# 3. Cargue los datos de “municipios_2022_simplificado.rds”. ¿Qué información encuentra? ----
mun <- readRDS("01_datos/municipios_2022_simplificado.rds") # Esta linea de código no funcionará si no cargaste antes la librería sf 
plot(mun, max.plot = 1)
# Se encuentra información de los municipios de mexico. 

# 4. Cargue los datos del archivo “prec_9_extrema.tif” sobre precipitación extrema en territorio mexicano. ¿Qué tipo de información es? Grafique. ----

# Cargamos la librería raster: 
library(raster)

# Cargar el archivo raster
raster_file <- raster("01_datos/prec_9_extrema.tif")
raster_df <- as.data.frame(raster_file, xy = TRUE)

# Renombrar la columna con los valores del raster
colnames(raster_df)[3] <- "value"

# Crear el mapa con ggplot2
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +  # Puedes cambiar la escala de colores según tus preferencias
  coord_fixed() +
  labs(title = "Mapa Raster", x = "Longitud", y = "Latitud", fill = "Valor") +
  theme_minimal()

# 5. Exporte solo los delitos pertenecientes a la alcaldía Cuauhtémoc a un archivo llamado “delitos_cuauhtemoc.geojson”. ----
delitos_cuautemoc <- delitos_2019_shp %>% 
  filter(AlcaldiaHechos == "CUAUHTEMOC") # Filtramos los datos de los delitos para la alcaldía Cuauhtémoc

# Guardamos los datos en un archivo geojson
st_write(delitos_cuautemoc, "delitos_cuauhtemoc.geojson")

# 6. Cargue el archivo de “Índice de Desarrollo Humano.csv” y replique en ggplot el siguiente mapa (sin el post-it). ----

# Mapa del IDH 
idh <- read_csv("01_datos/Indice de Desarrollo Humano.csv") %>%
  filter(Year == max(Year))

# Hacemos la unión de los datos: 
left_join(mun, idh, by = c("CVEGEO" = "CODGEO")) # Con left_join

mapx <- merge(mun, idh, by.x = "CVEGEO", by.y = "CODGEO")

# Mapa del IDH para el estado de Morelos: 
# ¿Qué mas le falta para replicar el del ejercicio? 
mapx %>%
  filter(Entidad == "Morelos") %>%
  ggplot(aes(fill = Valor)) + 
  geom_sf() + 
  scale_fill_gradientn(
    colors = 
      # c("red", "yellow", "green", "blue")
      wesanderson::wes_palettes$Zissou1
    # RColorBrewer::brewer.pal(name = "YlOrRd", n = 6)
  ) + 
  labs(title = "Índice de Desarrollo Humano", 
       subtitle = "Año = 2015") + 
  theme_bw() + 
  theme(axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

