darkModeTheme <- function() {
  list(
    font = list(color = '#e0e0e0'), # Light grey for text
    xaxis = list(
      range=c(-2,5),
      title = 'Tid',
      gridcolor = '#444444', # Slightly lighter grey for grid lines
      tickfont = list(color = '#e0e0e0') # Light grey for tick labels
    ),
    yaxis = list(
      gridcolor = '#444444',
      tickfont = list(color = '#e0e0e0')
    ),
    legend = list(
      orientation = "h",
      bgcolor = '#303030', # Match the plot background
      font = list(color = '#e0e0e0') # Light grey for legend text
    )
  )
}


lightModeTheme <- function() {
  list(
    # plot_bgcolor = '#ffffff', # White background for the plotting area
    # paper_bgcolor = '#f0f0f0', # Light grey for the surrounding paper
    font = list(color = '#333333'), # Dark grey for text, ensuring readability
    xaxis = list(
      range=c(-2,5),
      title = 'Tid',
      gridcolor = '#cccccc', # Light grey for grid lines, subtle but visible
      tickfont = list(color = '#333333') # Dark grey for tick labels
    ),
    yaxis = list(
      gridcolor = '#cccccc',
      tickfont = list(color = '#333333')
    ),
    legend = list(
      orientation = "h",
      bgcolor = '#f0f0f0', # Match the paper background
      font = list(color = '#333333') # Dark grey for legend text
    )
  )
}



subset_list <- function(
    list,
    pattern = NULL
    ) {

  if (!is.null(pattern)) {
    list[
      grep(
        pattern     = pattern,
        x           = names(list),
        ignore.case = TRUE
      )
    ]
  }


}




# Just for a clicable
# card
# choice_card <- function(
#     ...) {
#
#
#   tagList(
#     # tagList start;
#
#     # define the clickable
#     # card
#     div(
#       bslib::card(
#         style = "transition: box-shadow 0.3s ease-in-out; cursor: pointer; user-select: none;",
#         ... = ...,
#         class = "hover-elevate"
#       )
#     ),
#
#
#     # set the style
#     # of the hover-elevate class
#     # TODO: Change to something
#     # else to avoid overwriting
#     # other tags
#     tags$style(
#       HTML("
#       .hover-elevate:hover {
#         box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
#         transform: scale(1.05);
#       }
#     ")
#     )
#     # tagList end;
#   )
# }

# Options card
# are for picker inputs
# with overflows



# clickable_card <- function(
    #     inputid,
#     outputval,
#     ...) {
#
#
#   tagList(
#     div(
#       id = inputid,
#       #id = "clickableCard",
#       bslib::card(
#         style = "transition: box-shadow 0.3s ease-in-out; cursor: pointer; user-select: none; transition: transform 0.5s ease;",
#         ... = ...,
#         class = "hover-elevate"
#
#       )
#     ),
#     tags$script(
#       HTML(
#         sprintf("
#       $(document).ready(function() {
#         $('#%s').on('click', function() {
#           Shiny.setInputValue('%s', '%s', {priority: 'event'});
#         });
#       });
#     ",
#     inputid,
#     #"clickableCard",
#     # Has to be wrapped in
#     # ns
#     inputid,
#     outputval
#     # ,
#     # id
#         )
#       )
#     ),
#     tags$style(HTML("
#
#     .hover-elevate:hover {
#       box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
#     }
#   "))
#   )
# }

card <- function(
    title  = "title",
    header = NULL,
    footer = NULL,
    body   = NULL) {

  bslib::card(
    fill = TRUE,
    full_screen = TRUE,
    height = "100%",
    #style = "overflow-y: auto;",
    style = "margin-top: var(--bslib-mb-spacer);",
    # Card header
    bslib::card_header(
      shiny::tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        shiny::tags$div(class = "custom-title", bslib::card_title(title)),
        shiny::tags$div(
          style = "margin-left: auto; ", # This will push all inside elements to the far right

          header


        )
      )
    ),

    # Card Body
    bslib::card_body(
      shiny::tagList(
        body
      )
    ),

    bslib::card_footer(shiny::tagList(
      footer
    )
    )
  )

}



#' Add a pickerinput
#'
#' A wrapper of the [shinyWidgets::pickerInput()]-function, with predefined
#' danish wording.
#'
#'
#' @param inputid A [character]-vector of [length] 1.
#' @param label A [character]-vector of [length] 1.
#' @param choices A [character]-vector of [length] N.
#' @param selected A [character]-vector of [length] N.
#' @param multiple A [logical]-vector of [length] 1.
#' @param search A [logical]-vector of [length] 1.
#' @param size A [numeric]-vector of [length] 1.
#'
#' @details
#' See [shinyWidgets::pickerInput()] for usage.
#'
#' @return
#' @export
#'
#' @examples
picker_input <- function(
    inputid,
    label,
    choices,
    selected = NULL,
    multiple = FALSE,
    search    = TRUE,
    size     = "auto"
) {

  shiny::tagList(
    shinyWidgets::pickerInput(
      inputId = inputid,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      width = "100%",
      options  = shinyWidgets::pickerOptions(
        size   = size,
        liveSearch = search,
        actionsBox = TRUE,
        noneSelectedText = "Intet valgt",
        deselectAllText = "Fravælg alle",
        selectAllText   = "Vælg alle",
        noneResultsText = "Ingen match"
      )
    )
  )

}
