# script: Helpers
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-03-28
# objective: Create a set of functions
# that reduce repetetive coding
# script start;

#' Create a list of input values
#'
#' @param DT A [data.frame]-object containing the input parameters and
#' labels.
#' @param value A [character]-vector of length 1. The values of the variables
#' @param label A [character]-vector of length 1. The label of the variables
#' @param as_list A [logical]-vector of length 1. If [TRUE] the values are
#' split by label
#'
#'
#'
#' @return
#' @export
#'
#' @examples
input_parameters <- function(
    DT,
    value   = "value",
    label   = "label",
    as_list = NULL
) {

  parameters <- DT[[value]]
  names(parameters) <- DT[[label]]


  if (!is.null(as_list)) {

    parameters <- split(
      x = parameters,
      f = DT[[as_list]]
    )

  }

  parameters

}



#
#
# create_tabs <- function(
#     X,
#     body = ...,
#     header = ...
# ) {
#
#
#   dynamic_tabs <- lapply(X, function(x) {
#     bslib::nav_panel(title = x, body)
#   })
#
#   # Now add the non-dynamic parts like nav_spacer and nav_menu to the list
#   dynamic_tabs <- c(dynamic_tabs, list(
#     header
#   ))
#
#   # Use do.call to pass the list of tabs as separate arguments
#   do.call(bslib::navset_card_tab, c(
#     dynamic_tabs,
#     list(full_screen = TRUE, sidebar = NULL)
#   ))
#
# }
#
#
#
# create_tabs(
#   X = 1:10,
#   body = p("s"),
#   header = "s"
# )


create_tabs <- function(
    X,
    fn,
    header) {


  dynamic_tabs <- lapply(seq_along(X), function(x) {


    body <- fn(x)

    bslib::nav_panel(
      title = as.character(X[x]),
      body
      )
  }
  )


 names(dynamic_tabs) <- NULL

  # Add the header (or non-dynamic part) to the list
  dynamic_tabs <- c(dynamic_tabs, list(bslib::nav_spacer(),header))

  # Use do.call to pass the list of tabs as separate arguments
  do.call(bslib::navset_card_tab, c(
    dynamic_tabs,
    list(
      full_screen = TRUE,
      sidebar = NULL)
  ))
}




# script end;
