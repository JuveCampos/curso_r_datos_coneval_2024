
# Librerias ----
library(tidyverse) # Manipular bases de datos
library(networkD3) # Generar redes con D3
library(readxl) # Leer datos de excel 
library(highcharter) # Graficas de HighCharter
library(r2d3) # Convertir visualizaciones de D3
library(viridis) # Colores de la paleta viridis
library(xts) # Generar datos xts de series de tiempo
library(dygraphs) # Generar viz de Series de Tiempo
library(leaflet) # Generar mapas interactivos
library(plotly) # Generar gráficas interactivas
library(DT) # Generar tablas interactivas
library(kableExtra) # Generar tablas bonitas
library(r2d3)

# Guardamos bases de ejemplo 
MisLinks <- networkD3::MisLinks
MisNodes <- networkD3::MisNodes 

# Exploramos datos 
head(MisLinks)

# Exploramos nodos
head(MisNodes)

# Realizar la visualización.
forceNetwork(Links = MisLinks, 
             Nodes = MisNodes,
             Source = "source", 
             Target = "target", 
             Value = "value", 
             NodeID = "name", 
             Nodesize = "size",
             Group = "group", 
             opacity = 0.8,
             width = 600, 
             height = 600, 
             fontSize = 15)

# MAPAS DE CALOR ----
#Series de tiempo del número de personas/ciclistas/peatones atropelladas
accidentes_peatones <- read.csv("Bases de datos/accidentes_peatones.csv")  

# Exploramos la base
head(accidentes_peatones)

# Elaboramos un heatmap 
accidentes_peatones %>%
  # Creamos el heatmap  
  hchart(type = "heatmap", 
         name = "Atropellamientos",
         # Tooltip (ventanita)       
         tooltip = list(pointFormat = "Fecha: {point.x}<br>Cantidad:  {point.value} sucesos"),
         hcaes(x = Date, 
               y = reorder(delegacion_inicio,value),
               value = value)) %>% 
  # Titulo de la grafica
  hc_title(text = "Atropellamientos a ciclistas o peatones en la Ciudad de México") %>% 
  # Modificaciones del eje X
  hc_xAxis(categories = accidentes_peatones$Date, 
           title = list(text = "<b>Fecha</b>"), 
           type = "datetime", 
           labels = list(rotation = 90)) %>% 
  # Modificacion del eje Y
  hc_yAxis(title = list(text = "")) %>%
  # Modificacion de la paleta de colores
  hc_colorAxis(stops = color_stops(colors = viridis::viridis(10)))

# SERIES DE TIEMPO CON DYGRAPH ----

# Datos. Reciclaremos los datos de arriba. 
iztapalapa <- accidentes_peatones %>% 
  filter(delegacion_inicio == "IZTAPALAPA")

izta_xts <- xts(x = iztapalapa$value, 
                order.by = as.Date(iztapalapa$Date)) 

# Función para formatear las etiquetas del eje X en español
customDateFormatter <- 'function(d) {
  var months = ["Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"];
  return months[d.getMonth()] + " " + d.getFullYear();
}'

# Generamos la gráfica
dygraph(izta_xts, 
        main = "Accidentes en Iztapalapa") %>%
  dyRangeSelector(dateWindow = c(as.Date(iztapalapa$Date[1]), 
                                 iztapalapa$Date[nrow(iztapalapa)])) %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyAxis("x", axisLabelFormatter = customDateFormatter) %>%
  dyRangeSelector(height = 20)


# Gráfica de burbujas
# Leemos los datos
data <- read_excel("Bases de datos/flare.xlsx")

# Elaboramos la visualización
r2d3(data, 
     d3_version = 4, 
     script = "Bases de datos/bubbles.js")

# https://rstudio.github.io/r2d3/

# DATATABLES ----

iris2 = head(iris, 20)
datatable(iris2, options = list(
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
  pageLength = 5
))

# https://rstudio.github.io/DT/
