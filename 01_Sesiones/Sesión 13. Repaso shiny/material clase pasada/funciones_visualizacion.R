
# Librerias 
library(sf)
library(leaflet)
library(tidyverse)
library(readxl)
library(ggthemes)
library(scales)
library(plotly)

# Elabore una función que le permita, dado un indicador y un año seleccionado, generar un mapa coroplético interactivo en leaflet. Seleccione la paleta viridis o la que considere mejor.

# Cargar los datos 
datos <- read_xlsx("01_Datos/datos.xlsx")
metadatos <- read_xlsx("01_Datos/metadatos.xlsx")
catalogo <- read_xlsx("01_Datos/catalogo_estatal.xlsx")
shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
# plot(shp, max.plot = 1)

# nombre <- function(argumentos, parametros){}
# gen_mapa(ind_sel = 13, anio_sel = 2000, paleta_sel = "viridis")
gen_mapa <- function(ind_sel, anio_sel, paleta_sel = "viridis"){
  
  meta_sel <- metadatos %>% 
    filter(no == ind_sel)
  
  datos_sel <- datos %>% 
    filter(no == ind_sel) %>% 
    filter(year == anio_sel)
  
  mapx <- left_join(shp, datos_sel, by = 
                      c("CVE_EDO" = "cve_ent"))
  
  paleta <- colorNumeric(palette = paleta_sel,
                         domain = mapx$valor, 
                         reverse = T)
  
  etiquetas <- str_c("Estado: ", mapx$ENTIDAD)
  popups <- str_c("<b>Estado: </b>", mapx$ENTIDAD, "<br>", 
                  "<b>Valor: </b>", prettyNum(round(mapx$valor, 1),
                                              big.mark = ","), "<br>", 
                  "<b>Unidad de medida: </b>",
                  ifelse(is.na(meta_sel$umedida), 
                         yes = "Sin unidad", 
                         no = meta_sel$umedida))
  
  # Hacemos el mapa: 
  mapx %>% 
    leaflet() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron) %>% 
    addPolygons(color = "black", weight = 1, 
                fillColor = paleta(mapx$valor), 
                label = etiquetas, 
                popup = popups,
                fillOpacity = 0.8) %>% 
    addLegend(title = str_wrap(str_c(meta_sel$indicador, ", ", anio_sel), 15) %>% 
                str_replace_all(pattern = "\n", replacement = "<br>"),
              pal = paleta, 
              values = mapx$valor, 
              position = "bottomright")
  
}

# Explorar base de datos
datos %>% 
  group_by(no) %>% 
  summarise(n = n())

# unique(datos$no)
ind_sel = 1042
anio_sel = 2022

anios_disponibles <- datos %>% 
  filter(no == ind_sel) %>% 
  pull(year) %>% 
  unique()

gen_mapa(ind_sel = 1092, 
         anio_sel = 2022)

# 2. Elabore una función que le permita, dado un indicador seleccionado y un año seleccionado, generar una gráfica de barras estática con ggplot.

gen_barras <- function(ind_sel, anio_sel){
  
  meta_sel <- metadatos %>% 
    filter(no == ind_sel)
  
  unidad <- ifelse(
    str_detect(meta_sel$umedida, "orcentaje") & 
      !is.na(meta_sel$umedida), 
    yes = "%", 
    no = "")
  
  datos_sel <- datos %>% 
    filter(no == ind_sel) %>% 
    filter(year == anio_sel) %>% 
    left_join(catalogo)
  
  datos_sel %>% 
    ggplot(aes(y = reorder(entidad, valor), 
               x = valor)) + 
    geom_col(fill = "#cf6981", 
             width = 0.7) + 
    geom_text(aes(label = str_c(prettyNum(round(valor, 1), 
                                          big.mark = ","), 
                                unidad)), 
              hjust = -0.1) + 
    labs(x = NULL, y = NULL, 
         title = str_wrap(str_c(meta_sel$indicador, 
                                ", ", 
                                anio_sel), 50)) + 
    # ggthemes::theme_stata() + 
    scale_x_continuous(expand = expansion(c(0, 0.2)), 
                       labels = comma_format(suffix = unidad)) + 
    theme_minimal() + 
    theme(axis.text.y = element_text(angle = 0))
  
  
}

gen_barras(ind_sel = 885, anio_sel = 2022)

ind_sel = 1051
anio_sel = 2021

anios_disponibles <- datos %>% 
  filter(no == ind_sel) %>% 
  pull(year) %>% 
  unique()


# Elabore una función que le permita, dado un indicador seleccionado y un estado seleccionado, generar una gráfica de líneas interactiva mostrando la evolución del indicador para todos los años con información disponible.  

ind_sel = 1020
edo_sel = c("17", "01", "09")

# gen_lineas(ind_sel = 885, edo_sel = c("02", "10", "32"))
gen_lineas <- function(ind_sel, edo_sel){
  
  meta_sel <- metadatos %>% 
    filter(no == ind_sel)
  
  unidad <- ifelse(
    str_detect(meta_sel$umedida, "orcentaje") & 
      !is.na(meta_sel$umedida), 
    yes = "%", 
    no = "")
  
  datos_sel <- datos %>% 
    filter(no == ind_sel) %>% 
    filter(cve_ent %in% edo_sel) %>% 
    left_join(catalogo)
  
  plt = datos_sel %>% 
    ggplot(aes(x = factor(year), 
               y = valor, 
               group = entidad, 
               color = entidad
               # , 
               # text = str_c("Entidad: ", entidad, "<br>", 
               #              "Valor: ", round(valor, 1)))
    )) + 
    geom_line() + 
    geom_point() + 
    geom_label(aes(label = str_c(prettyNum(round(valor, 1), 
                                           big.mark = ","), 
                                 unidad)), 
               vjust = -0.5, 
               alpha = 0.7, 
               show.legend = FALSE
    ) + 
    labs(x = NULL, y = NULL, 
         color = "Entidad seleccionada: ",
         title = str_wrap(str_c(meta_sel$indicador), 50)) + 
    scale_x_discrete(expand = expansion(c(0.2, 0.2))) + 
    scale_y_continuous(expand = expansion(c(0.2, 0.2)), 
                       labels = comma_format()) + 
    theme_minimal() + 
    theme(axis.text.y = element_text(angle = 0), 
          legend.position = "bottom")
  
  plt
  
}

