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

  # main
  body <- tagList(
    card(
      title = "Carousel",
      body = tags$div(
        id = "carouselExampleControls",
        class = "carousel slide",
        `data-bs-ride` = "carousel", # Note the 'bs-' prefix for Bootstrap 5
        tags$div(
          class = "carousel-inner",
          tags$div(class = "carousel-item active",
                   bslib::card(
                     class = "d-block w-100", # Ensure full width
                     title = "Card 1",
                     body = "This is the first card."
                   )),
          tags$div(class = "carousel-item",
                   bslib::card(
                     class = "d-block w-100",
                     title = "Card 2",
                     body = "This is the second card."
                   )),
          tags$div(class = "carousel-item",
                   bslib::card(
                     class = "d-block w-100",
                     title = "Card 3",
                     body = "This is the third card."
                   ))
        ),
        tags$a(class = "carousel-control-prev",
               href = "#carouselExampleControls",
               role = "button",
               `data-bs-slide` = "prev",
               tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
               tags$span(class = "visually-hidden", "Previous")),
        tags$a(class = "carousel-control-next",
               href = "#carouselExampleControls",
               role = "button",
               `data-bs-slide` = "next",
               tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
               tags$span(class = "visually-hidden", "Next"))
      )
    )
  )

  sidebar <- tagList(
    h1("sidebar")
  )


  return(
    list(
      body = body,
      sidebar = sidebar
    )

  )

}

#' model1 Server Functions
#'
#' @noRd
mod_model1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$table <- renderTable(
      DT
    )

  })
}

## To be copied in the UI
# mod_model1_ui("model1_1")

## To be copied in the server
# mod_model1_server("model1_1")
