# Sesión 02. Ejercicios. 

# Cargamos librerias. 
library(sf)
library(tidyverse)
library(leaflet)

library(ggimage)
library(scales)
library(wesanderson)

# Ejercicio 01. Repasar el proceso para hacer mapas con datos externos. 
# Realiza el mapa de porcentaje de participación electoral con los datos de `prep_presidencial_municipios`

edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE)

prep <- readRDS("00_Datos/carpeta_final/prep_presidencial_municipios.rds")
write_csv(prep, "00_Datos/carpeta_final/prep_municipal.csv")

prep <- read_csv("00_Datos/carpeta_final/prep_municipal.csv")
# Referencia de Joins: https://rpubs.com/Juve_Campos/juntando_tablas

# leemos los datos geograficos 
geo <- readRDS("00_Datos/carpeta_final/municipios_2022_simplificado.rds")
st_write(geo, "00_Datos/carpeta_final/municipios_2022.geojson")

shp <- read_sf("00_Datos/carpeta_final/municipios_2022.geojson")
plot(shp, max.plot = 1)

# Unimos las capas: 
prep
shp

mapx <- left_join(shp, prep, by = c("CVEGEO" = "CVE_INEGI"))

mapx %>% 
  ggplot(aes(fill = porcentaje_participacion)) + 
  geom_sf()

colores <- wesanderson::wes_palettes$Zissou1
valores <- c(0, 20, 40, 60, 80, 100)

plt <- mapx %>% 
  # filter(nom_ent == "MORELOS") %>%
  ggplot() + 
  geom_sf(aes(fill = porcentaje_participacion), 
          color = "transparent") + 
  geom_sf(data = edo, color = "black", linewidth = 0.8, fill = NA) +
  labs(title = "Porcentaje de participación a nivel municipal", 
       subtitle = "Proceso electoral presidencial 2024", 
       fill = "Porcentaje de participación: ", 
       caption = "\n\n\nFuente: Elaboración propia con datos del INE, 2024") + 
  scale_fill_gradientn(colors = colores,
                       values = rescale(valores, to = c(0, 1)),
                       labels = scales::comma_format(suffix = "%")) + 
  scale_x_continuous(expand = expansion(c(0.2, 0.2))) + 
  theme_minimal() + 
  theme(text = element_text(family = "Arial"), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        plot.caption = element_text(color = "white", size = 16, hjust = 0),
        plot.margin= margin(4, 0, 0.8, 0, "cm"), # margin(top,right, bottom,left)
        plot.title = element_text(size = 25, color = wesanderson::wes_palettes$Zissou1[5], face = "bold"), 
        plot.subtitle = element_text(size = 18, color = wesanderson::wes_palettes$Zissou1[1], face = "bold"), 
        ) + 
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barheight = 0.9, 
                               barwidth = 30))

plt
plt <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")

ggsave("02_Graficas/mapa_participación_01.png", 
       height = 10, width = 12)


# Ahora, hagamos un mapa de los municipios donde ganó cada candidata: 

unique(mapx$coalicion_ganadora)
plt <- mapx %>% 
  ggplot() + 
  geom_sf(aes(fill = coalicion_ganadora), 
          color = "transparent") + 
  geom_sf(data = edo, color = "black", linewidth = 0.8, fill = NA) +
  labs(title = "Coalición ganadora para la elección presidencial, por municipio", 
       subtitle = "Proceso electoral presidencial 2024", 
       fill = "Porcentaje de participación: ", 
       caption = "\n\n\nFuente: Elaboración propia con datos del INE, 2024") + 
  scale_fill_manual(values = c("FyCXM" = "blue", 
                               "SHH" = "brown", 
                               "MC" = "orange")) + 
  # scale_fill_gradientn(colors = colores,
  #                      values = rescale(valores, to = c(0, 1)),
  #                      labels = scales::comma_format(suffix = "%")) + 
  scale_x_continuous(expand = expansion(c(0.15, 0.15))) + 
  theme_minimal() + 
  theme(text = element_text(family = "Arial"), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        plot.caption = element_text(color = "white", size = 16, hjust = 0),
        plot.margin= margin(4, 0.1, 0.6, 0.1, "cm"), # margin(top,right, bottom,left)
        plot.title = element_text(size = 25, color = wesanderson::wes_palettes$Zissou1[5], face = "bold"), 
        plot.subtitle = element_text(size = 18, color = wesanderson::wes_palettes$Zissou1[1], face = "bold"), 
  ) + 
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barheight = 0.9, 
                               barwidth = 30))

plt
plt <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")

ggsave("02_Graficas/mapa_coalicion_ganadora.png", 
       height = 10, width = 13)


# Finalmente, hagan un mapa donde se separen por cuartiles los municipios más y menos participativos. 

# quantile(mapx$porcentaje_participacion, na.rm = T)
# mapx$percentil_participacion
mapx <- mapx %>% 
  mutate(percentil_participacion = case_when(between(porcentaje_participacion, 0, 56.16) ~ "Entre 0 y 56.16%", 
                                             between(porcentaje_participacion, 56.16, 63.45) ~ "Entre 56.16 y 63.45%", 
                                             between(porcentaje_participacion, 63.45, 70.62771) ~ "Entre 63.45 y 70.62%", 
                                             between(porcentaje_participacion, 70.62771, 94.58384) ~ "Entre 70.62 y 94.58%"))

plt <- mapx %>% 
  ggplot() + 
  geom_sf(aes(fill = percentil_participacion), 
          color = "transparent") + 
  geom_sf(data = edo, color = "black", linewidth = 0.8, fill = NA) +
  labs(title = "Cuartiles de participación, por municipio", 
       subtitle = "Proceso electoral presidencial 2024", 
       fill = "Rangos de cuartiles de participación: ", 
       caption = "\n\n\nFuente: Elaboración propia con datos del INE, 2024") + 
  scale_fill_manual(values = c(wes_palettes$Zissou1[-1])) + 
  scale_x_continuous(expand = expansion(c(0.15, 0.15))) + 
  theme_minimal() + 
  theme(text = element_text(family = "Arial"), 
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        plot.caption = element_text(color = "white", size = 16, hjust = 0),
        plot.margin= margin(4, 0.1, 0.6, 0.1, "cm"), # margin(top,right, bottom,left)
        plot.title = element_text(size = 25, color = wesanderson::wes_palettes$Zissou1[5], face = "bold"), 
        plot.subtitle = element_text(size = 18, color = wesanderson::wes_palettes$Zissou1[1], face = "bold"), 
  ) + 
  guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5))

plt <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")

ggsave("02_Graficas/mapa_cuartiles.png", 
       height = 10, width = 13)



# 2. Leaflet:
library(leaflet)

norte <- function(mapa_leaflet,
                  ancho = 40,
                  position = 'topleft',
                  direccion_img_norte = "http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-north-arrow-2.png"){
  # 1. Descargamos la imagen
  north.arrow.icon <- paste0("<img src='",
                             direccion_img_norte,
                             "' style='width:",
                             as.character(ancho), "px;'>")
  # Lo incluimos en una funcion de RLeaflet
  if (!require("leaflet")) install.packages("leaflet") # Asegurarnos que este instalado Leaflet
  addControl(mapa_leaflet,
             html = north.arrow.icon, position = position,
             className = "fieldset {
             border: 0;}")
}


# Elabore el mismo mapa que hizo de las ganadoras, pero ahora en leaflet. 
paleta <- colorFactor(palette = c("blue", "brown", "orange"), 
                      domain = c("FyCXM","SHH","MC") %>% 
                        factor(levels = c("FyCXM","SHH","MC")))

labels = str_c(mapx$NOMGEO, ", ", mapx$nom_ent %>% str_to_title())

popupses <- str_c("<b>Municipio: </b>", mapx$NOMGEO, ", ", mapx$nom_ent %>% str_to_title(), "<br>", 
               "<b>Coalición ganadora: </b>", mapx$coalicion_ganadora, "<br>", 
               "<b>Total de votos Xóchitl: </b>", mapx$votos_xochitl, "<br>", 
               "<b>Total de votos Claudia: </b>", mapx$votos_cs,"<br>", 
               "<b>Total de votos Máynez: </b>", mapx$votos_jam
               )

# unique(mapx$coalicion_ganadora)

mapx %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(fillColor = paleta(mapx$coalicion_ganadora), 
              color = "white", 
              weight = 1, 
              opacity = 1, 
              label = labels, 
              popup = popupses, 
              fillOpacity = 0.6)  %>% 
  addPolygons(data = edo, 
              fill = NA, 
              weight = 4, 
              color = "black") %>% 
  addLegend(position = "bottomleft", 
            title = "Coalición ganadora por municipio",
            pal = paleta, values = mapx$coalicion_ganadora)
  

# Ahora, póngale etiquetas (labels) y popups


# Ahora, en un ejercicio nuevo, cargue los siguientes datos y elabore un mapa de rutas de transporte y delitos: 

# Puntos de delitos en 2018 a Transporte Público
delitos <- st_read("https://raw.githubusercontent.com/JuveCampos/30DayMapChallenge2019/master/19.%20Urban/roboAPasajerosCDMX.geojson",
                   quiet = T) 

# Rutas de transporte público
rutas <- st_read("https://raw.githubusercontent.com/JuveCampos/30DayMapChallenge2019/master/19.%20Urban/rutas-y-corredores-del-transporte-publico-concesionado.geojson", quiet = T) %>% 
  st_zm() # Corrección para coordenadas en el eje Z

# Municipios/Alcaldías de la CDMX
mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE) %>% 
  filter(CVE_ENT == "09")

# Perimetro de la CDMX
edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE) %>% 
  filter(ENTIDAD == "CIUDAD DE MÉXICO")


