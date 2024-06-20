
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
prep <- read_csv("00_Datos/prep_municipal.csv") %>% mutate(CVE_INEGI = str_pad(CVE_INEGI, width = 5, 
                                                                               side = "left",
                                                                               pad = "0"))
shp <- st_read("00_Datos/municipios_2022.geojson") %>% mutate(CVEGEO = str_pad(CVEGEO, width = 5, 
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
plt2 <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")
ggsave("02_Graficas/01_mapa_participacion.png", 
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
plt2 <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")
ggsave("02_Graficas/02_mapa_coalicion_ganadora.png", 
       height = 10, 
       width = 12)

# Mapa 03 ####

# Esta es para sacar los grupos: 
quantile(mapx$porcentaje_participacion, c(0.33, 0.66, 1), na.rm = T)

plt <- mapx %>%
  mutate(grupo_participacion = case_when(
    porcentaje_participacion < 58.55 ~ "Baja participación", 
    between(porcentaje_participacion, 58.55, 67.81) ~ "Media participacion", 
    porcentaje_participacion > 67.81 ~ "Alta participación"
    )) %>%
  ggplot(aes(fill = grupo_participacion)) + 
  geom_sf(color = "transparent") + 
  geom_sf(data = shp_ent, fill = NA, linewidth = 0.5) + 
  scale_fill_manual(values = c("Baja participación" = "gray90", 
                               "Media participacion" = "gray50", 
                               "Alta participación" = "black")) + 
  # scale_fill_gradientn(colors = wesanderson::wes_palettes$Zissou1, 
  #                      breaks = seq(0, 100, 20), 
  #                      labels = comma_format(suffix = "%")
  # ) + 
  scale_x_continuous(expand = expansion(c(0.2, 0.2))) + 
  labs(title = "Mapa de municipios por grupo de participación", 
       subtitle = "Elecciones 2024 para presidencia", 
       caption = "\n\n\nFuente: Elaboración propia con datos del PREP, 2024", 
       fill = "Grupo: ") + 
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

# 4. Utilizando la plantilla de mapas coneval (plantilla.pdf), dele una imagen institucional al mapa de (1). ----

plt2 <- ggimage::ggbackground(plt, "00_Datos/plantilla.pdf")
ggsave("02_Graficas/03_mapa_grupos_participacion.png", 
       height = 10, 
       width = 12)

