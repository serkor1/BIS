#' Assert truthfulness of conditions before evaluation
#'
#'
#' @description
#' This function is a wrapper of [stopifnot()], [tryCatch()] and [cli::cli_abort()] and asserts
#' the truthfulness of the passed expression(s).
#' @param ... expressions >= 1. If named the names are used as error messages, otherwise
#' R's internal error-messages are thrown
#'
#' @param error_message character. An error message, supports [cli]-formatting.
#' @example man/examples/scr_assert.R
#' @seealso [stopifnot()], [cli::cli_abort()], [tryCatch()]
#' @keywords internal
#' @returns [NULL] if all statements in ... are [TRUE]
assert <- function(..., error_message = NULL) {

  # 1) count number of expressions
  # in the ellipsis - this
  # is the basis for the error-handling
  number_expressions <- ...length()
  named_expressions  <- ...names()


  # 2) if there is more than
  # one expression the condtions
  # will either be stored in an list
  # or pased directly into the tryCatch/stopifnot
  if (number_expressions != 1 & !is.null(named_expressions)){

    # 2.1) store all conditions
    # in a list alongside its
    # names
    conditions <- list(...)

    # 2.2) if !is.null(condition_names) the
    # above condition never gets evaluated and
    # stopped otherwise, if there is errors
    #
    # The condition is the names(list()), and is
    # the error messages written on lhs of the the assert
    # function
    for (condition in named_expressions) {

      # if TRUE abort
      # function
      if (!eval.parent(conditions[[condition]])) {

        cli::cli_abort(
          c("x" = condition),

          # the call will reference the caller
          # by default, so we need the second
          # topmost caller
          call = sys.call(
            1 - length(sys.calls())
          )
        )


      }

    }

    # stop the function
    # here
    return(NULL)

  }

  # 3) if there length(...) == 1 then
  # above will not run, and stopped if anything

  tryCatch(
    expr = {
      eval.parent(
        substitute(
          stopifnot(exprs = ...)
        )
      )
    },
    error = function(error){

      # each error message
      # has a message and call
      #
      # the call will reference the caller
      # by default, so we need the second
      # topmost caller

      cli::cli_abort(
        # 3.1) if the length of expressions
        # is >1, then then the error message
        # is forced to be the internal otherwise
        # the assert function will throw the same error-message
        # for any error.
        message = if (is.null(error_message) || number_expressions != 1) error$message else error_message,
        call    = sys.call(
          1 - length(sys.calls())
        )
      )

    }
  )

}

plot <- function(
    data,
    x = list(
      x_axis = "temperature"
    ),
    y = list(
      y_axis = "pressure"
    ),
    ...
) {

  # check if data is passed
  # as data.frame
  assert(
    is.data.frame(data),
    error_message = c(
      "x" = "{.arg data} must be class {.cls data.frame}",
      "i" = sprintf(
        "{.arg data} is class {.cls %s}",
        class(data)
      )
    )
  )

  # create plot
  plotly::layout(
    p = plotly::plot_ly(
      data = data,
      x    = as.formula(paste0("~", x)),
      y    = as.formula(paste0("~", y)),
      showlegend = TRUE,
      ...
    ),
    xaxis = list(
      title = gsub(
        pattern = "_",
        replacement = " ",
        x = names(x)
      )
    ),
    yaxis = list(
      title = gsub(
        pattern = "_",
        replacement = " ",
        x = names(y)
      )
    )
  )


}

create_workbook <- function(
    DT,
    f = NULL) {


  if (!is.null(f)) {

    DT_list <- split(
      x = DT,
      f = eval(f)
    )

  } else {

    DT_list <- list(
      DT
    )

    names(DT_list) <- "data"

  }


  # 1) create workbook
  # locally
  wb <- openxlsx::createWorkbook()

  # 2) write data
  # while createing sheets
  invisible({
    lapply(
      X = names(DT_list),
      FUN = function(name) {

        # 2.1) add worksheet
        # to the data
        openxlsx::addWorksheet(
          wb = wb,
          sheetName = name
        )

        # 2.2) write as datatable
        # to the work sheet
        openxlsx::writeDataTable(
          wb = wb,
          x = DT_list[[name]],
          sheet = name
        )

      }
    )
  })


  wb


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
    size     = "auto",
    placeholder_text = "Intet valgt"
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
        noneSelectedText = placeholder_text,
        deselectAllText = "Fravælg alle",
        selectAllText   = "Vælg alle",
        noneResultsText = "Ingen match"
      )
    )
  )

}




layout <- function(
    plot,
    title,
    dark = TRUE) {
  # Initial color configuration for the light theme
  color <- list(
    plot_bgcolor = 'rgb(0,0,0,0)',  # Dark background for plotting area
    paper_bgcolor = 'rgb(0,0,0,0)',  # Dark background for the surrounding paper
    font = list(color = '#333333'),  # Dark grey for text, ensuring readability
    xaxis = list(
      gridcolor = '#cccccc',  # Light grey for grid lines, subtle but visible
      tickfont = list(color = '#333333')  # Dark grey for tick labels
    ),
    yaxis = list(
      gridcolor = '#cccccc'  # Blue for grid lines in light theme
    ),
    legend = list(
      orientation = 'h',
      x = 0,
      y = 100,
      yref = "container",
      bgcolor = 'transparent'  # Transparent background for the legend
    )
  )

  # Overwrite the initial settings if the dark theme is selected
  if (dark) {
    color <- list(
      plot_bgcolor = 'rgb(0,0,0,0)',  # Dark background for plotting area
      paper_bgcolor = 'rgb(0,0,0,0)',  # Dark background for the surrounding paper
      font = list(color = '#e0e0e0'),  # Light grey for text
      xaxis = list(
        gridcolor = '#444444',  # Red grid lines for dark theme
        tickfont = list(color = '#e0e0e0')  # Light grey for tick labels
      ),
      yaxis = list(
        gridcolor = '#444444',  # Dark grey for grid lines
        tickfont = list(color = '#e0e0e0')  # Light grey for tick labels
      ),
      legend = list(
        orientation = 'h',
        x = 0,
        y = 100,
        yref = "container",
        bgcolor = 'transparent'  # Transparent background for the legend
      )
    )
  }

  # Create a unified list of parameters to pass to layout
  args <- c(
    list(plot),
    list(
      title = list(
        text = title,
        x = 1,
        xref = "paper",
        xanchor = "right"
      ),
      margin = list(
        l = 10,
        r = 10,
        b = 15,
        t = 25
      )
    ),
    color  # Merge the color settings directly into the arguments
  )

  # Call plotly::layout with the constructed list of arguments
  do.call(plotly::layout, args)
}
