#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  card_clicked <- mod_landing_server(
    "landing",
    home = reactive(input$home)
  )

  # output$ui <- renderUI({
  #   mod_landing_ui("landing")
  # })
  observeEvent(
    input$home,
    ignoreInit = FALSE,
    ignoreNULL = FALSE,
    {
      output$body <- renderUI(
        mod_landing_ui("landing")
      )

      output$sidebar <- renderUI({})
    }


  )


  mod_data_server(
    id = "data",
    model = card_clicked
  )



  mod_model2_server(
    "model2",
    theme = reactive({input$app_theme})
  )




  observeEvent(
    card_clicked(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {


      if (card_clicked() == "model1") {

        # Generate UI
        ui <- mod_model1_ui(
          id = "model1"

        )

        mod_model1_server(
          "model1",
          theme = reactive({input$app_theme})
        )

        output$body <- renderUI(
          ui$body
        )

        output$sidebar <- renderUI(
          ui$sidebar
        )
      }

      if (card_clicked() == "model2") {

        # Generate UI
        ui <- mod_model2_ui(
          id = "model2"
        )

        output$body <- renderUI(
          ui
        )

      }





    }
  )
}
