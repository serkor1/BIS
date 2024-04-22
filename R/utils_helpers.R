
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
