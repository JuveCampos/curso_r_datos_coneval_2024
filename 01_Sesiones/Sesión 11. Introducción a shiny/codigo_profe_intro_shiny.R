library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(ggrepel)
library(shiny)

# Funciones propias
nb <- function(x,n){
  round(x = x, digits = n) %>% 
    format(nsmall = n) %>% 
    prettyNum(big.mark = ",") 
}

# Datos 
datos <- readxl::read_xlsx("01_Datos/base_datos_estados_completa.xlsx")
metadatos <- readxl::read_xlsx("01_Datos/metadatos_estados_completa.xlsx")

inds_sel <- c(1092, 1051, 1042, 1036, 1020, 991, 949, 885, 835, 13,1032,987)


datos_i <- datos %>% 
  filter(no %in% inds_sel) %>% 
  arrange(no)
metadatos_i <- metadatos %>% 
  filter(no %in% inds_sel) %>% 
  arrange(no)

openxlsx::write.xlsx(datos_i, "01_Datos/datos.xlsx")
openxlsx::write.xlsx(metadatos_i, "01_Datos/metadatos.xlsx")

datos <- readxl::read_xlsx("01_Datos/datos.xlsx")
metadatos <- readxl::read_xlsx("01_Datos/metadatos.xlsx")
cat_edos <- readxl::read_xlsx("01_Datos/catalogo_estatal.xlsx")
edos_shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")


anios_disponibles_por_indicador <- function(ind_sel){
  datos %>% 
    filter(no == ind_sel) %>% 
    pull(year) %>% 
    unique() %>% 
    sort()
}

# datos %>% 
#   select(no, year) %>% 
#   unique() %>% 
#   ungroup() %>% 
#   group_by(no) %>% 
#   summarise(anios = str_c(year))
  
  

# 1. Mapa: ----
ind_sel <- 1042
anios_disponibles_por_indicador(ind_sel = ind_sel)
anio_sel = 2022

gen_mapa <- function(ind_sel, anio_sel){
  
  # 1. Filtramos los datos para solo quedarnos con el indicador que buscamos
  datos_sel <- datos %>% 
    filter(no == ind_sel) %>% 
    filter(year == anio_sel)
  
  # 2. Filtramos los metadatos para quedarnos con la información de ese indicador
  meta_sel <- metadatos %>% 
    filter(no == ind_sel)
  
  # 2. Hacemos el merge con el shp
  mapx <- left_join(edos_shp, datos_sel, by = c("CVE_EDO" = "cve_ent"))
  
  # 3. Ahora si, hacemos el mapa de leaflet: 
  paleta <- colorNumeric(palette = "viridis", domain = mapx$valor)
  
  label = str_c("Estado: ", mapx$ENTIDAD)
  popup = str_c("<b>Estado: </b>", mapx$ENTIDAD, "<br>", 
                "<b>Indicador: </b>", meta_sel$indicador, "<br>", 
                "<b>Valor: </b>", nb(mapx$valor, 1), "<br>", 
                "<b>Unidad: </b>", meta_sel$umedida
  )
  
  mapx %>% 
    leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(color = "white", 
                weight = 1, 
                label = label,
                popup = popup,
                fillColor = paleta(mapx$valor), 
                fillOpacity = 1,
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 3,
                                                    opacity = 1, 
                                                    bringToFront = TRUE)) %>% 
    addLegend(title = str_wrap(str_c(meta_sel$indicador, ", ", anio_sel), 15) %>% str_replace_all(pattern = "\n", replacement = "<br>"), 
              pal = paleta, values = mapx$valor, 
              position = "bottomright")
  
  
}

# Ahora hacemos la función de la gráfica de barras: 
# gen_barras(ind_sel = 1042, anio_sel = 2021)
gen_barras <- function(ind_sel, anio_sel){

    # 1. Filtramos los datos para solo quedarnos con el indicador que buscamos
    datos_sel <- datos %>% 
      filter(no == ind_sel) %>% 
      filter(year == anio_sel) %>% 
      left_join(cat_edos) %>% 
      mutate(is.nacional = ifelse(entidad == "Nacional", yes = "Nacional", no = "Estados"))
    
    # 2. Filtramos los metadatos para quedarnos con la información de ese indicador
    meta_sel <- metadatos %>% 
      filter(no == ind_sel)
    
    # 3. Hacemos la gráfica: 
    datos_sel %>% 
      ggplot(aes(x = reorder(entidad, valor), y = valor, 
                 fill = is.nacional, color = is.nacional)) + 
      geom_col() + 
      geom_text(aes(label = nb(valor, 1)), 
                hjust = 0, fontface = "bold") + 
      coord_flip() + 
      scale_y_continuous(expand = expansion(c(0, 0.4))) + 
      scale_fill_manual(values = c("Nacional" = "#E1AF00", "Estados" = "#3B9AB2")) + 
      scale_color_manual(values = c("Nacional" = "#E1AF00", "Estados" = "#3B9AB2")) + 
      labs(x = NULL, y = NULL, title = str_wrap(meta_sel$indicador, 50), 
           caption = str_wrap(meta_sel$fuentes, 100)) + 
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title = element_text(face = "bold", size = 18))

}


ind_sel = 1042
edo_sel = c("17", "01")

# gen_lineas(ind_sel = 1042, edo_sel = c("09", "20", "25"))
gen_lineas <- function(ind_sel, edo_sel){

  # 1. Filtramos los datos para solo quedarnos con el indicador que buscamos
  datos_sel <- datos %>% 
    filter(no == ind_sel) %>% 
    filter(cve_ent %in% edo_sel) %>% 
    left_join(cat_edos) %>% 
    mutate(is.nacional = ifelse(entidad == "Nacional", yes = "Nacional", no = "Estados"))
  
  estado_seleccionado <- datos_sel$entidad %>% unique()
  
  # 2. Filtramos los metadatos para quedarnos con la información de ese indicador
  meta_sel <- metadatos %>% 
    filter(no == ind_sel)
  
  plt <- datos_sel %>% 
    ggplot(aes(x = year, y = valor, group =entidad, color = entidad, 
               text = str_c("<b>Entidad: </b>", datos_sel$entidad, "<br>", 
                            "<b>Valor: </b>", nb(datos_sel$valor, 1), "<br>"
                            ))) + 
    geom_line() + 
    geom_point() + 
    geom_label_repel(aes(label = nb(valor, 1)), 
                     hjust = 0, fontface = "bold", alpha = 0.7, show.legend = F) + 
    labs(x = NULL, y = NULL, title = str_wrap(meta_sel$indicador, 50), 
         subtitle = str_c(estado_seleccionado, collapse = ", "),
         caption = str_wrap(meta_sel$fuentes, 100), 
         color = "Entidad: ") + 
    theme_minimal() +
    theme(legend.position = "bottom", 
          plot.title = element_text(face = "bold", size = 18), 
          legend.key.width = unit(2, units = "cm"),
          plot.subtitle = element_text(face = "bold", size = 16, color = "gray50"))
  
  plotly::ggplotly(plt, tooltip = "text")
    
}

library(shiny)

ui <- fluidPage(
  # Control para seleccionar indicador
  # Control para seleccionar año
  # COntrol para seleccionar estado
  # Espacio para colocar el mapa
  # Espacio para colocar las barras
  # Espacio para colocar las lineas
)

server <- function(input, output, session) {
  # Codigo para cocinar el control de año que depende del indicador
  # Codigo para cocinar el mapa
  # Codigo para cocinar las barras
  # Codigo para cocinar las lineas
}

shinyApp(ui, server)


