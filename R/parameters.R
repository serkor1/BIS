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
    as_list = FALSE
) {

  parameters <- DT[[value]]
  names(parameters) <- DT[[label]]


  if (as_list) {

    parameters <- split(
      x = parameters,
      f = names(parameters)
    )

  }

  parameters

}
