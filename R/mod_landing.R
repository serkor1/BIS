#' landing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landing_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(HTML("
      /* The overlay: full-screen, semi-transparent, behind cards */
      .dim-overlay {
        position: fixed; /* or 'absolute' */
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.5); /* Adjust color and opacity */
        z-index: 1; /* Behind the cards */
      }

      /* Ensure card is above the overlay */
      .shiny-bs4-card {
        position: relative;
        z-index: 2; /* Above the overlay */
      }

      /* Additional styling for centering the card */
      .card-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        position: relative;
        z-index: 2; /* Ensure it's above the overlay */
      }
    "))
    ),

    div(class="dim-overlay",
      div(class="card-container",
            div(
              class = "d-flex justify-content-center align-items-center",
              style = "height: 100%;",  # This ensures the div takes the full viewport height
              bslib::layout_columns(
                # div(id = ns("clickableCard1"),
                #     bslib::card(
                #       title = "Clickable Card 1",
                #       "Click me to trigger a server action for Model 1.",
                #       style = "cursor: pointer;"
                #     )
                # ),
                # div(id = ns("clickableCard2"),
                #     bslib::card(
                #       title = "Clickable Card 2",
                #       "Click me to trigger a server action for Model 2.",
                #       style = "cursor: pointer;"
                #     )
                # )
                # ,
                clickable_card(
                  inputid = ns("model1"),width = "20vw",
                  bslib::card_header(
                    shiny::span(
                      bsicons::bs_icon("box"),
                      "Målgruppemodellen"
                    )
                  ),
                  bslib::card_body(
                    "
                    Beregn sundhedsøkonomiske omkostninger
                    af forskellige sygdomme.
                    "
                    ),
                  outputval = "model1"

                ),
                clickable_card(
                  inputid = ns("model2"),
                  width = "20vw",
                  bslib::card_header(
                    shiny::span(
                      bsicons::bs_icon("box"),
                      "Forældremodellen"
                    )
                  ),
                  bslib::card_body(
                    "
                    Beregn produktivitetstab ved syge
                    børn.
                    "
                    ),
                  outputval = "model2"

                )
              )


            )
      )
  )
  )

    # # Javascript:
    # tags$script(HTML(sprintf("
    #   $(document).ready(function() {
    #     $('#%s').on('click', function() {
    #       Shiny.setInputValue('%s', 'model1', {priority: 'event'});
    #     });
    #     $('#%s').on('click', function() {
    #       Shiny.setInputValue('%s', 'model2', {priority: 'event'});
    #     });
    #   });
    # ", ns("clickableCard1"), ns("card_clicked"), ns("clickableCard2"), ns("card_clicked")))),

}

#' landing Server Functions
#'
#' @noRd
mod_landing_server <- function(id, home){
  moduleServer(id, function(input, output, session){

    # 0) Determine Namespace
    ns <- session$ns


    observeEvent(home(), {
      value(NULL) # Update the reactive value to "model1"
    })
    # Create a reactive value to store the selected model
    value <- reactiveVal() # This initializes `value` without any data

    # Observe changes to `input$model1` and update `value`
    observeEvent(input$model1, {
      value("model1") # Update the reactive value to "model1"
    })

    # Observe changes to `input$model2` and update `value`
    observeEvent(input$model2, {
      value("model2") # Update the reactive value to "model2"
    })

    # Return a reactive expression that provides the value of `value`
    return(reactive({ value() }))


  })
}

## To be copied in the UI
# mod_landing_ui("landing_1")

## To be copied in the server
# mod_landing_server("landing_1")
