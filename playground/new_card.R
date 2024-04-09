# library(shiny)
# library(bslib)
#
# ui <- bslib::page(
#   # Apply some custom CSS to layout divs side by side
#   tags$head(
#     tags$style(HTML("
#       .card-header-divs {
#         display: flex;
#         justify-content: flex-end;  /* Aligns items to the right */
#       }
#       .header-div {
#         margin: 0 10px;  /* Adjust or remove margin as needed */
#       }
#     "))
#   ),
#
#   bslib::card(
#
#     bslib::card_header(
#       class = "card-header-divs",
#       bslib::card_title("title"),
#       div(class = "header-div", "left Div Content"),
#       div(class = "header-div", "Middle Div Content"),
#       div(class = "header-div", "Right Div Content")
#     ),
#     "This is the main content of the card."
#   )
# )
#
# server <- function(input, output, session) {
#   # Server logic can be defined here
# }
#
# shinyApp(ui, server)


# library(shiny)
# library(bslib)
#
# ui <- bslib::page(
#   # Apply some custom CSS to layout divs side by side
#   tags$head(
#     tags$style(HTML("
#       .card-header-divs {
#         display: flex;
#         justify-content: space-between;  /* Aligns items on both ends */
#         align-items: center;  /* Centers items vertically */
#         width: 100%;  /* Ensures the header takes full width */
#       }
#       .header-title {
#         flex-grow: 1;  /* Allows the title to take necessary space */
#       }
#       .header-right {
#         display: flex;
#         justify-content: flex-end;  /* Aligns the divs to the right */
#       }
#       .header-div {
#         margin: 0 10px;  /* Adjust or remove margin as needed */
#       }
#     "))
#   ),
#
#   bslib::card(
#
#     bslib::card_header(
#       div(class = "card-header-divs",
#           div(class = "header-title", bslib::card_title("Title")),
#           div(class = "header-right",
#               div(class = "header-div", "farleft"),
#               div(class = "header-div", "Left Div Content"),
#               div(class = "header-div", "Middle Div Content"),
#               div(class = "header-div", "Right Div Content")
#           )
#       )
#     ),
#     "This is the main content of the card."
#   )
# )
#
# server <- function(input, output, session) {
#   # Server logic can be defined here
# }
#
# shinyApp(ui, server)


library(shiny)
library(bslib)

ui <- bslib::page(
  # Apply some custom CSS for layout and vertical centering
  tags$head(
    tags$style(HTML("
      .card-header-divs {
        display: flex;
        justify-content: space-between;  /* Aligns items on both ends */
        align-items: center;  /* Centers items vertically in the header */
        width: 100%;  /* Ensures the header takes full width */
      }
      .header-title {
        flex-grow: 1;  /* Allows the title to take necessary space */
      }
      .header-right {
        display: flex;
        justify-content: flex-end;  /* Aligns the divs to the right */
        align-items: center;  /* Ensures divs are vertically centered within their container */
      }
      .header-div {
        margin: 0 10px;  /* Adjust or remove margin as needed */
      }
    "))
  ),

  bslib::card(
    bslib::card_header(
      div(class = "card-header-divs",
          div(class = "header-title", bslib::card_title("Title")),
          div(class = "header-right",
              div(class = "header-div", "Left Div Content"),
              div(class = "header-div", "Middle Div Content"),
              div(class = "header-div", "Right Div Content")
          )
      )
    ),
    "This is the main content of the card."
  )
)

server <- function(input, output, session) {
  # Server logic can be defined here
}

shinyApp(ui, server)
