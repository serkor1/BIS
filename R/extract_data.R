#' Extract data from SQL database
#'
#' @param DB_connection
#' @param table
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
extract_data <- function(
    DB_connection = NULL,
    table = 'model1',
    ...){



  # NOTE: Nothing is passed
  # to ... it returns the entire
  # table from DB

  # 0) Extract all relevant
  # parameters from ellipsis

  # the arg_list contains
  # all actual values
  # these are passed in variable = arg_list
  arg_list <- list(...)

  # the call list is the column
  # names from each argument
  # in the function
  call_list <- ...names()

  # 1) construct the SQL
  # query for model 1
  query <- vapply(
    names(arg_list),
    FUN.VALUE = character(1),
    FUN = function(x) {


      # extract element
      arg_list <- arg_list[[grep(pattern = x, ignore.case = TRUE, x = names(arg_list))]]


      paste0(
        x,
        " IN (",
        paste0("'",arg_list, "'", collapse = ", "),
        ")"
      )

    }
  )

  # 2) checks for empty
  # query and returns the entire
  # table if TRUE
  if (identical(query, character(0))) {

    query <- paste("SELECT * FROM", table)

  } else {

    query <- paste(
      paste("SELECT * FROM", table, "WHERE"),
      paste0(
        query,
        collapse = " AND "
      )
    )

  }

  # 3) send query and wrap
  # in try catch to capture possible
  # errors
  get_results <- tryCatch(
    expr = {
      DBI::dbSendQuery(
        conn = DB_connection,
        statement = query
      )
    },
    error = function(error) {
      # Print the error-message
      # alongside the query
      # for debugging
      cli::cli_abort(
        message = c(
          "x" = error$message,
          "i" = query
        ),
        call = sys.call(
          1 - length(sys.calls())
        )
      )
    }
  )

  # 3) Get the results
  # and store as data.table
  DT <- data.table::as.data.table(
    DBI::dbFetch(
      res = get_results,
      n = -1
    )
  )

  # 4) clear results;
  DBI::dbClearResult(get_results)

  DT
}
