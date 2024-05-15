# script: Tables
# date: 2024-04-01
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create function for generating datatables
# for shiny
# script start;


#' Title
#'
#' @param DT
#' @param header
#'
#' @return
#' @export
#'
#' @examples
generate_table <- function(
    DT,
    header = list()) {



  DT::formatStyle(
    table = DT::datatable(
      escape = FALSE,
      data = DT,
      rownames = FALSE,
        container = add_header(
          column_names = colnames(DT),
          where        = header
      ),
      extensions = c("Buttons", "RowGroup"),
      options = list(
        rowGroup = list(dataSrc = grep(
          pattern = "group",
          x       = colnames(DT)
        ) - 1),
        language = list(
          paginate = list(
            `previous` = "Forrige",
            `next` =     "Næste"
          ),
          search = "Søg:"
        ),


        buttons = c(
          "csv",
          "excel"
        ),
        searching = FALSE,
        lengthChange = FALSE,
        dom = "ptBR",
        ordering = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = grep(pattern = "group", x = colnames(DT), ignore.case = TRUE) - 1),
          list(className = 'dt-left', targets = 0), # Left align the first column (0-based index)
          list(className = 'dt-center', targets = 1:(ncol(DT) - 1)) # Center align all columns
        )
      )
    ),
    columns = colnames(DT),

    # Color changes the body
    # text
    background = "transparent"
  )



}



#' Title
#'
#' @param column_names
#' @param where
#'
#' @return
#' @export
#'
#' @example
add_header <- function(
    column_names,
    where = list(
      `Group Header` = c("mpg", "cyl")
    )
){


  # NOTE: All the column names
  # get moved.


  # Generate a list of all subheader names to include those not explicitly mentioned
  allSubheaders <- unlist(where)
  unspecifiedSubheaders <- setdiff(column_names, allSubheaders)

  # Extend the subheaders list to include the unspecified columns
  # Each unspecified column becomes its own group with a single subheader
  extendedSubheaders <- c(where, lapply(unspecifiedSubheaders, function(x) c(x)))
  extendedHeaderNames <- c(names(where), NULL)


  htmltools::withTags(
    {


      table(
        class = 'display',
        thead(

          tr({
            lapply(seq_along(extendedHeaderNames), function(i) {
              colspanValue <- length(extendedSubheaders[[i]])
              th(colspan = colspanValue, extendedHeaderNames[i], style = "text-align: center;")
            })
          }),
          tr({
            unlist(
              recursive = FALSE,
              lapply(extendedSubheaders, function(group) {
                lapply(group, function(subheader) {
                  th(subheader)
                })
              })
            )

          })
        )
      )
    })


}





# script end;
