# script: scr_utils
# date: 2024-02-29
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate utility functions
# that are relevant for the backend
# script start;

darkModeTheme <- function() {
    list(
        # plot_bgcolor = '#303030', # Dark grey background for the plotting area
        # paper_bgcolor = '#1a1a1a', # Even darker grey for the surrounding paper
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



extract_data <- function(
        DB_connection = NULL,
        table = 'model1',
        ...){
    
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
    
    query <- paste(
        paste("SELECT * FROM", table, "WHERE"),
        paste0(
            query,
            collapse = " AND "
        )
    )

    # 2) send query and wrap
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

# script end;