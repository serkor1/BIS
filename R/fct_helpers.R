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
#' @example
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


#' Dynamically create multiple tabs
#'
#' @inheritParams base::lapply
#' @inheritParams bslib::navset_card_tab
#' @param fn A [function] to be passed into the body
#' of the panel
#' @param ...
#'
#' @returns A [shiny.tag]. More specifically a dynamically rendered
#' navigation panel.
#' @export
#'
#' @example man/examples/scr_renderTabs.R
create_tabs <- function(
    X,
    fn,
    ...) {

  # 1) create tabs and
  # tab content
  dynamic_tabs <- lapply(
    X = seq_along(X),
    FUN = function(x) {

      bslib::nav_panel(
        title = as.character(X[x]),
        fn(x)
      )
    }
  )

  # 2) the created tabs
  # cant have names other
  # wise the function breaks
  names(dynamic_tabs) <- NULL

  # 3) this part is the fixed
  # part of the tabs and applies globally
  dynamic_tabs <- c(
    dynamic_tabs,
    list(
      bslib::nav_spacer(),
      ...
    )
  )

  # 4) collaect all cards and store
  # in navset_card_tab
  do.call(
    what = bslib::navset_card_tab,
    args =  c(
      dynamic_tabs,
      list(
        full_screen = TRUE,
        sidebar = NULL
      )
    )
  )
}



tooltip <- function(
    msg) {


    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      msg,
      placement = "auto"
    )

}





# script end;
