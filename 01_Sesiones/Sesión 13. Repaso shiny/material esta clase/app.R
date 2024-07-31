
# Librerias ----
library(shiny)

# Global ----


# Interfaz de usuario ----
ui <- fluidPage(

    # Ponerle un título a la app, que sea "Monitor de inflación"
    
    # Generar control de genéricos. 
    # El control debe de ser de selección multiple
    
    # Generar un panel de pestañas 
    # En la primera pestaña, colocar la gráfica de evolución 
    # En la segunda pestaña, colocar la gráfica de barras
    # *Recuerda utilizar las funciones output, y que a cada render le toca su output
    
)

# Servidor ----
server <- function(input, output, session) {
    # Generar la gráfica de lineas,
    # Generar la gráfica de barras
    # BONUS; genere un botón de descarga para cada una de las gráficas.
    # BONUS; genere una tabla interactiva que muestre el porcentaje de crecimiento anual de inflación para todos los genéricos
    # BONUS: genere una cuenta gratuita en shinyapps.io y cargue la aplicación a su servidor. 
}
    
# Correr aplicación ----
shinyApp(ui, server)
