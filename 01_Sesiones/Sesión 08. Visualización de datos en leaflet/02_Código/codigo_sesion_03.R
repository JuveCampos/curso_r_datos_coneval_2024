# Para evitar la notación científica: 
options(scipen = 999)

library(tidyverse)

# 1. Función nb ####

x = 1234556.000001
n = 4

nb <- function(x, n){
  round(x = x, digits = n) %>% 
    format(nsmall = n) %>% 
    prettyNum(big.mark = ",")
}

nb(x = 100000, n = 5)
nb(x = 100, n = 1)

# 2. A partir del código necesario para realizar el mapa de la coalición ganadora en la sesión pasada, elabore una función para generar mapas por entidad.

# Librerias: 
library(sf)
library(tidyverse)
library(viridis)
library(wesanderson)
library(ghibli)
library(ggthemes)
library(scales)
library(ggimage)

# 1. Con los datos del PREP municipal (prep_municipal.csv) de la carpeta datos y el geojson de municipios (municipios_2022.geojson), elabore un mapa donde se observe la participación electoral a nivel municipal. ----

# Polígono de los estados: 
shp_ent <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# Datos del PREP: 
prep <- read_csv("01_Datos/prep_municipal.csv") %>% mutate(CVE_INEGI = str_pad(CVE_INEGI, width = 5, 
                                                                               side = "left",
                                                                               pad = "0"))
shp <- st_read("01_Datos/municipios_2022.geojson") %>% mutate(CVEGEO = str_pad(CVEGEO, width = 5, 
                                                                               side = "left",
                                                                               pad = "0"))

# Juntamos los datos para poder tener un objeto con todo lo necesario para 
#  hacer un mapa
mapx <- left_join(shp,prep, by = c("CVEGEO" = "CVE_INEGI"))

# Hacemos el mapa 
# El mapa lo guardamos en el objeto plt: 
plt <- mapx %>%
  ggplot(aes(fill = porcentaje_participacion)) + 
  geom_sf(color = "transparent") + 
  geom_sf(data = shp_ent, fill = NA, linewidth = 0.5) + 
  # Cuando tenemos una variable numérica usamos scale_fill_gradientn()
  scale_fill_gradientn(colors = wesanderson::wes_palettes$Zissou1, 
                       breaks = seq(0, 100, 20), 
                       labels = comma_format(suffix = "%")
  ) + 
  # Esto es para expandir el espacio en horizontal (X) que usa el mapa
  scale_x_continuous(expand = expansion(c(0.2, 0.2))) + 
  labs(title = "Mapa de participación a nivel municipal", 
       subtitle = "Elecciones 2024 para presidencia", 
       caption = "\n\n\nFuente: Elaboración propia con datos del PREP, 2024", 
       fill = "Porcentaje de participación: ") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 30, face = "bold", 
                                  family = "Arial", 
                                  color = "olivedrab"), 
        plot.subtitle = element_text(size = 25), 
        plot.caption = element_text(size = 15, hjust = 0, color = "white"), 
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18), 
        axis.text = element_blank(), 
        panel.grid = element_blank(), 
        # t = TOP, r = Right, b = Bottom, l = left
        # Recordemos que esto lo fuimos sacando por prueba y error: 
        plot.margin = margin(t = 3.5, 
                             r = 0, 
                             b = 0.8, 
                             l = 0, 
                             unit = "cm")) + 
  guides(fill = guide_colorbar(title.position = "top", 
                               barwidth = 40, 
                               barheight = 1, 
                               title.hjust = 0.5))

# 2. Utilizando la plantilla de mapas coneval (plantilla.pdf), dele una imagen institucional al mapa de (1). ----
plt2 <- ggimage::ggbackground(plt, "01_Datos/plantilla.pdf")
ggsave("03_Visualizaciones/01_mapa_participacion.png", 
       height = 10, 
       width = 12)

# Realice un nuevo mapa, ahora para la candidata ganadora o para la coalición ganadora.

unique(mapx$coalicion_ganadora)

plt <- mapx %>%
  ggplot(aes(fill = coalicion_ganadora)) + 
  geom_sf(color = "transparent") + 
  geom_sf(data = shp_ent, fill = NA, linewidth = 0.5) + 
  scale_fill_manual(values = c("FyCXM" = "#040b6e", 
                               "SHH" = "#691205", 
                               "MC" = "orange")) + 
  scale_x_continuous(expand = expansion(c(0.2, 0.2))) + 
  labs(title = "Mapa de coalición ganadora", 
       subtitle = "Elecciones 2024 para presidencia", 
       caption = "\n\n\nFuente: Elaboración propia con datos del PREP, 2024", 
       fill = "Coalición: ") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 30, face = "bold", 
                                  family = "Arial", 
                                  color = "olivedrab"), 
        plot.subtitle = element_text(size = 25), 
        plot.caption = element_text(size = 15, hjust = 0, color = "white"), 
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18), 
        axis.text = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = margin(t = 3.5, 
                             r = 0, 
                             b = 0.8, 
                             l = 0, 
                             unit = "cm")) + 
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.5))

# plt

# Utilizando la plantilla de mapas coneval (plantilla.pdf), dele una imagen institucional al mapa de (1).
plt2 <- ggimage::ggbackground(plt, "01_Datos/plantilla.pdf")
ggsave("03_Visualizaciones/02_mapa_coalicion_ganadora.png", 
       height = 10, 
       width = 12)

# Funcion para hacer el mapa de coaliciones ganadoras por estado. 


mapx <- left_join(shp,prep, by = c("CVEGEO" = "CVE_INEGI"))

edo_sel = 32


gen_mapa_coalicion_ganadora <- function(edo_sel){
  
mapx2 <- mapx %>% 
  filter(cve_ent == edo_sel)
shp_ent2 <- shp_ent %>% 
  filter(as.numeric(CVE_EDO) == edo_sel)
nombre_entidad <- mapx2$nom_ent %>% unique() %>% str_to_sentence()

plt <- mapx2 %>%
  ggplot(aes(fill = coalicion_ganadora)) + 
  geom_sf(color = "transparent") + 
  geom_sf(data = shp_ent2, fill = NA, linewidth = 0.5) +
  scale_fill_manual(values = c("FyCXM" = "#040b6e", 
                               "SHH" = "#691205", 
                               "MC" = "orange")) + 
  scale_x_continuous(expand = expansion(c(0.2, 0.2))) + 
  labs(title = "Mapa de coalición ganadora", 
       subtitle = str_c("Elecciones 2024 para presidencia", 
                       "\n",
                       nombre_entidad), 
       caption = "\n\n\nFuente: Elaboración propia con datos del PREP, 2024", 
       fill = "Coalición: ") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 30, face = "bold", 
                                  family = "Arial", 
                                  color = "olivedrab"), 
        plot.subtitle = element_text(size = 25), 
        plot.caption = element_text(size = 15, hjust = 0, color = "white"), 
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18), 
        axis.text = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = margin(t = 3.5, 
                             r = 0, 
                             b = 0.8, 
                             l = 0, 
                             unit = "cm")) + 
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.5))
plt

plt2 <- ggimage::ggbackground(plt, "01_Datos/plantilla.pdf")
ggsave(str_c("03_Visualizaciones/02_mapa_coalicion_ganadora", 
             "_", edo_sel,
             ".png"), 
       height = 10, 
       width = 12)
}

gen_mapa_coalicion_ganadora(edo_sel = 32)

for (estado_para_mapa in 1:32){
  gen_mapa_coalicion_ganadora(edo_sel = estado_para_mapa)
  print(str_c("Ya está listo el mapa de ", estado_para_mapa, " :3 "))
}

# 4. Elabore nuevamente el mapa de coaliciones ganadoras, pero ahora en la librería leaflet. ----


mapx <- left_join(shp,prep, by = c("CVEGEO" = "CVE_INEGI"))

library(leaflet)

etiquetas_mpios <- str_c(mapx$CVEGEO, " - ", 
                         mapx$NOMGEO, ", ",
                         mapx$nom_ent
                         )

popup_mapa <- str_c(
  "<b>Municipio: </b>", mapx$NOMGEO, ", ",
  mapx$nom_ent, "<br>",
  "<b>Coalición ganadora: </b>", mapx$coalicion_ganadora, "<br>",
                    "<b>Porcentaje de participación: </b>", round(mapx$porcentaje_participacion, 2), "%<br>",
  "<b>Votos Claudia:</b> ", mapx$votos_cs, "<br>",
  "<b>Votos Xóchitl:</b> ", mapx$votos_xochitl, "<br>",
  "<b>Votos Máynez:</b> ", mapx$votos_jam
  )


unique(mapx$coalicion_ganadora)
paleta_coaliciones <- colorFactor(palette = c("blue",
                                              "orange", 
                                              "brown"), 
                                  domain = c("FyCXM",
                                             "MC",
                                             "SHH" 
                                             ))


mp <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = mapx, 
              weight = 0.8, 
              color = "black", 
              fillColor = paleta_coaliciones(mapx$coalicion_ganadora), 
              opacity = 1, 
              label = etiquetas_mpios,
              popup = popup_mapa,
              fillOpacity = 0.7, 
              highlightOptions = 
                highlightOptions(color = "green",
                                 weight = 5, 
                                 bringToFront = F,
                                 opacity = 1)) %>% 
  addPolygons(data = shp_ent, 
              fill = NA, 
              opacity = 1,
              color = "white", 
              weight = 2) %>% 
  addLegend(position = "bottomright", 
            pal = paleta_coaliciones, 
            values = mapx$coalicion_ganadora, 
            title = "Coalición ganadora")

mp

htmlwidgets::saveWidget(mp, "mapa_pagina.html")

