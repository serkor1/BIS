#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(
    # tagList Start;

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Main page
    bslib::page_navbar(
      # main page start;
      window_title = "Beregner til Investeringer i Sundhed",

      # main page options and themes;
      fillable = TRUE,
      collapsible = FALSE,
      theme = bslib::bs_theme(
        version =  5,
        preset = c("flatly")
      ),

      # navbar content;
      title = shinyWidgets::actionBttn(
        label = "Beregner til Investeringer i Sundhed",
        inputId = "home",
        style = "simple",
        color = "default",
        class = "btn-transparent",
        icon = bsicons::bs_icon("box")
      ),

      # the spacer moves
      # everything to the
      # right
      bslib::nav_spacer(),

      # dark-mode toggle;
      bslib::nav_item(
        bslib::input_dark_mode(
          mode = "light",
          id = "app_theme"
        )
      ),

      # github link;
      bslib::nav_item(
        shinyWidgets::actionBttn(
          inputId = "openLinkBtn",
          label = NULL,
          icon = bsicons::bs_icon("github"),
          style = "simple",
          color = "default",
          class = "btn-transparent"
        )
      ),

      # page body;
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1,1),
        uiOutput(
          "body"
        )
      ),

      # page sidebar;
      sidebar = bslib::sidebar(
        # sidebar options;
        # NOTE: we need the ID for
        # togglers
        id = "sidebar",
        position = "left",
        open = "always",
        title = "Parametre",
        uiOutput(
          "sidebar"
        )
      )

      # main page end;
    )

    # tagList end;
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
