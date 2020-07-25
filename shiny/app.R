library(shiny)


# UI Interfaz de Usuario #########################################################
ui <- fluidPage(
  
  # Título de la App #############################################################
  titlePanel('"Automatic Text Summarization" de textos legales'),
  
  # Layout de la App #############################################################
  sidebarLayout(
    
    # Panel lateral de la App. Inputs. ###########################################
    sidebarPanel(
      helpText(h3("Selecciona el texto legal para resumir")),
      
      # Multiple choice
      selectInput(inputId = "select", 
                  label = h3("Select box"), 
                  choices = list("BOE-A-1994-26003-consolidado_LAU.pdf" = "../data/BOE-A-1994-26003-consolidado_LAU.pdf"), 
                  selected = "data/BOE-A-1994-26003-consolidado_LAU.pdf"),
      ),
    # End. Panel lateral de la App. Inputs. ######################################
    
    # Panel principal de la App. Outputs. ########################################
    mainPanel(
      textOutput(outputId = "texto"))
    # End. Panel principal de la App. Outputs. ###################################
    
  )
  # End. Layout de la App ########################################################
)

 
# Servidor de la App #############################################################
server <- function(input, output) {
  output$texto <- renderText({
    fulltext::ft_extract(input$select)$data
    
  })
  
}
# End. Servidor de la App #########################################################

# Ejecución de la App #############################################################
shinyApp(ui = ui, server = server)