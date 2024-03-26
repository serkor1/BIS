#' model2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' model2 Server Functions
#'
#' @noRd 
mod_model2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_model2_ui("model2_1")
    
## To be copied in the server
# mod_model2_server("model2_1")
