#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Main page
    bslib::page_navbar(

      tags$head(
        tags$style(HTML("
      body {
        -webkit-user-select: none; /* Safari */
        -moz-user-select: none;    /* Firefox */
        -ms-user-select: none;     /* Internet Explorer/Edge */
        user-select: none;         /* Non-prefixed version, currently supported by Chrome, Opera, and Edge */
      }
    "))
      ),

    shiny::tags$style(HTML("

    .btn-transparent {
      background-color: transparent !important;
      border-color: transparent !important;
    }
  ")),

  shiny::tags$script('
    document.addEventListener("DOMContentLoaded", function() {
      var button = document.getElementById("openLinkBtn");
      button.addEventListener("click", function() {
        window.open("https://github.com/serkor1/BIS", "_blank");
      });
    });
  '),

  title = shinyWidgets::actionBttn(
    label = "Beregner til Investeringer i Sundhed",
    inputId = "home", style = "simple", color = "default",class = "btn-transparent",icon = bsicons::bs_icon("box")),
  window_title = "Beregner til Investeringer i Sundhed",
  theme = bslib::bs_theme(
    version =  5,
    preset = c("flatly")
  ),
  # bslib::nav_item(
  #     shinyWidgets::actionBttn(
  #       inputId = "home",label = NULL,icon = bsicons::bs_icon(name = "house"), style = "simple", color = "default",class = "btn-transparent")
  #
  #
  #
  #
  #   ),
  bslib::nav_spacer(),

  bslib::nav_item(bslib::input_dark_mode(mode = "light",id = "app_theme")),
  bslib::nav_item(
    shinyWidgets::actionBttn(
      inputId = "openLinkBtn",
      label = NULL,icon = bsicons::bs_icon("github"), style = "simple", color = "default",class = "btn-transparent")
  )

  ,

  bslib::layout_columns(
    uiOutput(
      "ui"
    )
  )





    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BIS"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
