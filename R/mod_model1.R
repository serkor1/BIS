#' model1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model1_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' model1 Server Functions
#'
#' @noRd 
mod_model1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_model1_ui("model1_1")
    
## To be copied in the server
# mod_model1_server("model1_1")
