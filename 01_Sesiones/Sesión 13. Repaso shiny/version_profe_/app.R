# Aplicación: ----
library(shiny)

source("global.R")

ui <- fluidPage(
    h1("Monitor de inflación"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectizeInput("selGenerico", 
                           "Seleccione el(los) genérico(s) a analizar", 
                           choices = sel_genericos, 
                           multiple = TRUE, 
                           selected = "Total"), 
            downloadButton("descarga_graficas", "Descargue la gráfica")
            ),
        mainPanel = mainPanel(
            tabsetPanel(id = "pestañas",
                tabPanel("Evolución",    plotOutput("grafica_evolucion", height = "90vh")), 
                tabPanel("Cambio Anual", plotOutput("grafica_cambio", height = "90vh")), 
                tabPanel("Tabla", 
                         h2("Incrementos anuales de inflación (con respecto al periodo más reciente)", style = "color:#6950D8;"), 
                         radioButtons(inputId = "rdMostrarTodo", 
                                      label = "Información en la tabla",
                                      choices = c("Mostrar todos los genéricos" = TRUE, 
                                                  "Mostrar genéricos seleccionados" = FALSE),
                                      inline = T),
                         DT::dataTableOutput("tabla"))
            )
        )
    ) 
)

# gen_barras_cambio_anual(genericos = "Total")

server <- function(input, output, session) {
    output$grafica_evolucion <- renderPlot({
        gen_grafica(genericos = input$selGenerico)
    })
    
    output$grafica_cambio <- renderPlot({
        gen_barras_cambio_anual(genericos = input$selGenerico)
    })
    
    output$tabla <- DT::renderDT({
        gen_tabla(genericos = input$selGenerico, mostrar_todos = input$rdMostrarTodo)
    })
    
    output$descarga_graficas <- downloadHandler(
        filename = function(){
            if(input$pestañas == "Evolución"){
                str_c("evolucion_inflacion.png")
            } else {
                str_c("evolucion_cambio.png")
            }
        }, content = function(file){
            if(input$pestañas == "Evolución"){
                    ggsave(file,
                           plot = gen_grafica(genericos = input$selGenerico),
                           device = "png", 
                           height = 6,
                           width = 10)
            } else {
                    ggsave(file, 
                           plot = gen_barras_cambio_anual(genericos = input$selGenerico),
                           device = "png", 
                           height = 6, 
                           width = 10)
            }
        }
    )
}

shinyApp(ui, server)
