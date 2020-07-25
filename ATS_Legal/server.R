#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output) {
    output$texto <- renderTable({
        #browser()
        if (!is.null(input$select)) Extrae_Resumen(input$select)
        
        })
    }
)
