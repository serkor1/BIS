#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' data Server Functions
#'
#' @noRd 
mod_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_data_ui("data_1")
    
## To be copied in the server
# mod_data_server("data_1")
