#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    
    # Título de la App #############################################################
    titlePanel( "Resumen automático de textos legales"),
    
    # Layout de la App #############################################################
    sidebarLayout(
        
        # Panel lateral de la App. Inputs. ###########################################
        sidebarPanel(
            helpText(h3("Selecciona el texto legal para resumir")),
            
            # Multiple choice
            selectInput(inputId = "select", 
                        label = "", 
                        choices = list("Ley de Arrendamientos Urbanos" = "data/BOE-A-1994-26003-consolidado_LAU.pdf"), 
                        selected = NULL,
                        multiple = TRUE),
            submitButton()
        ),
        # End. Panel lateral de la App. Inputs. ######################################
        
        # Panel principal de la App. Outputs. ########################################
        mainPanel(
            #textOutput(outputId = "texto"))
            tableOutput(outputId = "texto"))
        # End. Panel principal de la App. Outputs. ###################################
        
    )
    # End. Layout de la App ########################################################
))
