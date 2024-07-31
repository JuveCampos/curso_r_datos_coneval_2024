
library(shiny)
library(DT)
library(shinycssloaders)

source("funciones_visualizacion.R")

indicador_seleccionado = "13"
anios_disponibles <- function(indicador_seleccionado){
  datos %>% 
    filter(no == indicador_seleccionado) %>% 
    pull(year) %>% 
    unique() %>% 
    sort()
}

# Opciones: 
opciones_estados <- catalogo$cve_ent
names(opciones_estados) <- catalogo$entidad
opciones_indicador <- metadatos$no
names(opciones_indicador) <- metadatos$indicador

ui <- fluidPage(
  
  fluidRow(
    column(4, 
           HTML("<h1>Mi segundo shiny</h1>")
           ), 
    column(4, offset = 4, 
           tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/5/5b/Logo_CONEVAL.svg", 
                    style = "height:20vh; max-height:20vh;")
           )
  ), 
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      uiOutput("controles"), 
      uiOutput("definicion")
    #   tags$p("Lorem Ipsum es simplemente el texto de relleno de las imprentas y archivos de texto. Lorem Ipsum ha sido el texto de relleno estándar de las industrias desde el año 1500, cuando un impresor (N. del T. persona que se dedica a la imprenta) desconocido usó una galería de textos y los mezcló de tal manera que logró hacer un libro de textos especimen. No sólo sobrevivió 500 años, sino que tambien ingresó como texto de relleno en documentos electrónicos, quedando esencialmente igual al original. Fue popularizado en los 60s con la creación de las hojas Letraset, las cuales contenian pasajes de Lorem Ipsum, y más recientemente con software de autoedición, como por ejemplo Aldus PageMaker, el cual incluye versiones de Lorem Ipsum.")
    ),
    mainPanel = mainPanel(
      tabsetPanel(id = "pestañas",
        tabPanel(title = "Mapa: ", leafletOutput("mapa") %>% withSpinner()), 
        tabPanel(title = "Comparativa: ", plotOutput("barras") %>% withSpinner()), 
        tabPanel(title = "Evolución", plotOutput("lineas") %>% withSpinner()), 
        tabPanel(title = "Tabla", DT::dataTableOutput("tabla") %>% withSpinner(), 
                 br(), 
                 downloadButton(outputId = "btn_1", 
                                "Descargar datos"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$controles <- renderUI({
    if(input$pestañas == "Evolución"){
      tagList(
        selectizeInput(inputId = "selIndicador", 
                       label = "Seleccione un indicador", 
                       choices = opciones_indicador, 
                       selected = "13"
        ), 
        selectizeInput(inputId = "selEntidad", 
                       label = "Seleccione estados: ", 
                       choices = opciones_estados, 
                       selected = 17, 
                       multiple = T
        )
      )
    } else {
      tagList(
        selectizeInput(inputId = "selIndicador", 
                       label = "Seleccione un indicador", 
                       choices = opciones_indicador, 
                       selected = "13"
        ), 
        uiOutput("selAnio")
      )
    }
  })
  
  output$selAnio <- renderUI({
    selectizeInput(inputId = "selAnio", 
                   label = "Seleccione un año: ", 
                   choices = anios_disponibles(input$selIndicador))
  })
  # Mapa: 
  output$mapa <- renderLeaflet({
    gen_mapa(ind_sel = input$selIndicador, 
             anio_sel = input$selAnio,
             paleta_sel = "viridis")
  })
  
  # Barras: 
  output$barras <- renderPlot({
    gen_barras(ind_sel = input$selIndicador, anio_sel = input$selAnio)
  })
  
  # Lineas: 
  output$lineas <- renderPlot({
    gen_lineas(ind_sel = input$selIndicador, 
               edo_sel = input$selEntidad)
  })
  
  output$definicion <- renderUI({
    txt <- metadatos %>% 
      filter(no == input$selIndicador) %>% 
      pull(definicion)
    
    tags$p(tags$strong("Definicion: "), txt)
    
  })
  
  output$tabla <- DT::renderDT({
      DT::datatable(datos %>% 
                      filter(no == input$selIndicador), 
        options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 5
      ))
  })
  
  output$btn_1 <- downloadHandler(
    filename = function(){
      "indicador.xlsx"
    },
    content = function(file){
      datos %>% 
        filter(no == input$selIndicador) %>% 
        # filter(year = input$selAnio) %>% 
        openxlsx::write.xlsx(file)  
    }
  )
  
}

shinyApp(ui, server)

