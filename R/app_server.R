#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  card_clicked <- mod_landing_server(
    "landing"
  )

  # output$ui <- renderUI({
  #   mod_landing_ui("landing")
  # })
  observeEvent(
    input$home,
    ignoreInit = FALSE,
    ignoreNULL = FALSE,
    output$ui <- renderUI(
      mod_landing_ui("landing")
    )
  )



  observeEvent(
    card_clicked(),ignoreInit = TRUE,ignoreNULL = TRUE,
    {
      output$ui <- renderUI(
        h1("sd")
      )


    }
  )
}
