# Opciones ----
options(scipen = 999)
# En Mac suele funcionar la opción que marca Segasi: 
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
# En Windows/Linux suele funcionar esta función: 
Sys.setlocale("LC_ALL","Spanish")

# Librerias a utilizar ----
library(tidyverse) # Manejo de bases de datos 
library(plotly) # Graficas interactivas
library(htmlwidgets) # Widgets de html (para guardar las paginas)
library(dygraphs) # Para hacer series de tiempo
library(highcharter) # Para hacer treemaps y otras visualizaciones
library(networkD3) # Generar redes con D3
library(igraph) # Para manejo de información de redes
library(janitor) # Limpieza de nombres en las tibbles
library(readxl) # Leer exceles
library(scales) # Formatos especiales en los ejes
library(xts) # Creación de objetos de series de tiempo
library(hrbrthemes) # Temas de ggplot
library(viridis) # Paletas viridis
library(gapminder) # Bases de datos de gapminder. 
library(ggiraph) # Extiende ggplot
library(DT) # Tablas interactivas
library(shiny) # Aplicaciones web
library(r2d3) # Convertir visualizaciones de D3
library(xts) # Generar datos xts de series de tiempo
library(leaflet) # Generar mapas interactivos
library(DT) # Generar tablas interactivas
library(ggiraph)

# PLOTLY ----
## Grafica 01: Puntos ----

# bd GAPMINDER: 
data <- gapminder %>% 
  filter(year=="2007") %>% 
  select(-year)

# Gráfica de burbujas: 
bd_gap <- data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) 

plt <-  bd_gap %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(x = "Gdp per Capita", y = "Life Expectancy") +
  theme(legend.position = "bottom")

plt # Gráfica normal de ggplot
ggplotly(plt) # Gráfica de plotly

## Grafica 02: Líneas (BITCOIN) ----
bitcoin <- read_csv("Bases de datos/bitcoin.csv") %>% 
  mutate(Volume = as.numeric(Volume), 
         Close = as.numeric(Close)) %>% 
  filter(!is.na(Volume) | !is.na(Close)) %>% 
  mutate(popup = str_c("<b>Precio de Cierre:</b> $", 
                       prettyNum(Close, 
                                 big.mark = ","), "<br>", 
                       "<b>Fecha:</b> ", Date, "<br>",
                       "<b>Volumen de transacciones:</b> $", prettyNum(Volume, 
                                                               big.mark = ",")))
plt = bitcoin %>% 
  ggplot(aes(x = Date, 
             y = Close, 
             text = popup, 
             group = "a")) + 
  geom_line() + 
  labs(title = "Bitcoin. Precio de Cierre diario.<br>Datos del 17 de mayo del 2020 al 16 de mayo del 2021.") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, family = "Arial")) + 
  scale_y_continuous(labels = scales::dollar_format())

# Gráfica de ggplot. 
plt 
# Gráfica interactiva de ggplot
plt_interactiva_btc <- plotly::ggplotly(plt, tooltip = "text") 
plt_interactiva_btc

class(plt_interactiva_btc)

# Lo podemos guardar como widget: 
saveWidget(plt_interactiva_btc, 
           "plt_interactiva_btc.html")

## Gráfica 03: Barras (COMPARATIVO INEGI) ----
educ <- readxl::read_xlsx("Bases de datos/grado_educacion_promedio.xlsx") %>% 
  mutate(popup = str_c("<b>Entidad Federativa: </b>", entidad_federativa, "<br>", 
                       "<b>Municipio: </b>", municipio, "<br>", 
                       "<b>Grado promedio educación: </b>", round(valor, 1), " años"))

plt = educ %>% 
  filter(entidad_federativa == "Morelos") %>% 
  ggplot(aes(x = reorder(municipio, valor), y = valor, text = popup)) + 
  geom_col(fill = "salmon") + 
  coord_flip() + 
  # geom_text(aes(label = str_c(round(valor, 2), " años")), hjust = 0.5) + 
  scale_y_continuous(breaks = 0:11, 
                     limits = c(0, 11), 
                     expand = expansion(c(0,0.001), 0)) +
  # ggthemes::theme_wsj() + 
  labs(title = "Grado Educativo para los municipios de Morelos", 
       y = "Municipios", x = "Años de escuela")

ggplotly(plt, tooltip = "text")

# WORDCLOUD2 ----
## 01. WordCloud de Twitter (TWITTER) ----
tw = readxl::read_xlsx("Bases de datos/tweets_delfina_gómez__2023_04_12_20_01_59.xlsx")

# Nube de palabras: 
create_wordcloud <- function(data, stop_words = c(), num_words = 100, background = "white",  mask = NULL) {
  # Checar si esta instalado Pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(wordcloud2, tm, stringr)
  
  # Pre-Función para eliminar simbolos raros
  quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))
  
  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras 
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minusculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuacion (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los numeros
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiracion e interrogacion al reves
    corpus <- tm_map(corpus, quitar_signos)    
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el numero de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra 
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  freq_palabras <<- data
  
  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  wordcloud2(data, backgroundColor = background, color = "random-dark", fontFamily = "Asap", size = 0.5) 
}

# Generamos el wordcloud ----
create_wordcloud(tw$text, num_words = 100)

# HIGHCHARTER ----
## Grafica 01: Treemaps (EXPORTACIONES) ----
exp <- read_csv("Bases de datos/mexico_exports.csv")

exp %>% 
  mutate(level = as.numeric(factor(Sector))) %>% 
  hchart(type= "treemap", hcaes(x = Name, 
                                value = Share, 
                                color = level)) %>% 
  hc_title(text = "Productos de exportación 2018") %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(20)))

## Grafica 02: (PEF) ----
pef <- read_csv("Bases de datos/pef.csv")
pef %>% 
  hchart(type= "treemap", hcaes(x = DESC_RAMO, 
                                value = total, 
                                color = total)) %>% 
  hc_title(text = "Presupuesto de Egresos de la Federación 2020 (Cifras en millones de pesos)") %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))


# REDES ----

## Gráfica 01. Relaciones entre personajes de Los Miserables ----

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


## Grafica 02: Networks (NETWORK DE ONE PIECE) ----

# Datos 
nodos = readxl::read_xlsx("Bases de datos/relaciones One Piece.xlsx",
                          sheet = "nodos")

enlaces = readxl::read_xlsx("Bases de datos/relaciones One Piece.xlsx",
                            sheet = "conexiones")

# Armamos los datos de la red 
network <- graph_from_data_frame(d=enlaces,
                                 vertices=nodos,
                                 directed=F)
# Exploramos el objeto
network
# Checamos de que clase es el objeto 
class(network)
# Graficamos la red
plot(network)

# Generamos datos compatibles para un force Network
Links = networkD3::igraph_to_networkD3(network)[[1]]
Nodes = networkD3::igraph_to_networkD3(network)[[2]]
Nodes$size = nodos$tamaño
class(Nodes)
Nodes$group = nodos$tipo

# Funcion para presionarle a algun nodo de interes
MyClickScript <- 'alert("Seleccionaste a  " + d.name + " que esta en la fila " +
       (d.index + 1) +  " de tu dataframe original");'

net = forceNetwork(Links = Links,
                   Nodes = Nodes,
                   Nodesize = "size",
                   NodeID = "name",
                   Group = "group",
                   Value = "value",
                   # height="550px",
                   # width="350px",
                   radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   linkWidth = JS("function(d) {return d.linkWidth;}+2"),
                   zoom = T,
                   opacityNoHover = TRUE,
                   clickAction = MyClickScript,
                   colourScale = JS('d3.scaleOrdinal().range(["salmon","red","orange"]);'))

net

## Grafica 03: Sankeys (ENCUESTA ORIGEN-DESTINO) ----

#Número de viajes provenientes de la CDMX reportados por la Encuesta Origen y Destino del INEGI.
datosankey <- read.csv("Bases de datos/datosankey.csv")
# Corrección:
datosankey$target <- paste(datosankey$target, " ", sep="") 

# Generamos un objeto para guardar los nodos: 
nodes <- data.frame(name =
                      c(as.character(datosankey$source), 
                        as.character(datosankey$target)) %>% 
                      unique() %>% sort()) 

# Generamos la columna IDSource (iniciando desde el cero)
datosankey$IDsource=match(datosankey$source, nodes$name)-1 
# Generamos la columna IDtarget (iniciando desde el cero)
datosankey$IDtarget=match(datosankey$target, nodes$name)-1

# Generamos la visualización: 
sankey = sankeyNetwork(Links = datosankey, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, nodeWidth=60,
              fontSize=10,
              nodePadding=8)

sankey

# DYGRAPH ----
## Grafica 01: Series de tiempo con dygraph (BITCOIN OTRA VEZ) ----
# DOCUMENTACION: https://rstudio.github.io/dygraphs/

# Generamos la serie de tiempo
ts_bitcoin <- xts(x = bitcoin$Close, 
    order.by = bitcoin$Date)
class(ts_bitcoin) # [1] "xts" "zoo"

# Función de JS 
valueFormatter<- 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'

# Generamos la gráfica: 
dygraph(ts_bitcoin, 
        main = "Precio de Cierre de Bitcoin") %>% 
  dyAxis("y", label = "Close price in USD",
         axisLabelFormatter=JS(valueFormatter)) %>% 
  dyRangeSelector(dateWindow = c(bitcoin$Date[1], 
                                 bitcoin$Date[nrow(bitcoin)])) %>% 
  dySeries("V1", "Close Price ($USD)") %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20) %>% 
  dyAxis("x", axisLabelFormatter = customDateFormatter) %>%
  dyCrosshair(direction = "vertical") %>% 
  dyUnzoom()

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

# GGIRAPH: ----
# https://www.ardata.fr/ggiraph-book/starting.html
# Ejemplo de ggiraph: Puntos símples ----
g <- ggplot(mpg, aes(x = displ, y = cty))
my_gg <- g + 
  geom_point_interactive(aes(tooltip = model, data_id = model), 
                         size = 3, hover_nearest = TRUE)
girafe(ggobj = my_gg)

bitcoin_gg <- bitcoin %>% 
  ggplot(aes(x = Date, 
             y = Close, 
             text = popup, 
             group = "a")) + 
  geom_line() + 
  geom_point_interactive(aes(tooltip = str_c("$", prettyNum(round(Close, 1), big.mark = ","))), 
                         size = 2)

girafe(ggobj = bitcoin_gg)

# Ejemplos de integración con ggiraph + shiny: 
run_girafe_example("crimes")
run_girafe_example("DT")

# R2D3 ----
# Gráfica de burbujas
# Leemos los datos
data <- read_excel("Bases de datos/flare.xlsx")

# Elaboramos la visualización
r2d3(data, 
     d3_version = 4, 
     script = "Bases de datos/bubbles.js")

# DATATABLES ----
iris2 = head(iris, 20)
datatable(iris2, options = list(
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
  pageLength = 5
))




