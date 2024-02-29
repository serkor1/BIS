# script: global.R
# date: 2024-02-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: global script
# script start;

# garbage collection
rm(list = ls()); invisible(gc())

# libraries for
# shiny development
suppressPackageStartupMessages(
    {
        library(shiny)
        library(bslib)
        library(shinyWidgets)
        library(data.table)
        library(workbookR)
        library(DBI)
    }
)

# source; 
invisible(
    lapply(
        X = list.files(
            path = 'R',
            full.names = TRUE,
            recursive = TRUE
        ),
        FUN = source
    )
)

# 1) determine wether
# its development version
# or not
developer_mode <- FALSE

# 2) determine plot-color
# TODO: Remove this at some point
plot_color <- sample(
    data.table::as.data.table(
        RColorBrewer::brewer.pal.info,
        keep.rownames = TRUE
    )[category %chin% 'seq']$rn,
    1
)

# 3) establish connection
# to database
DB_connection <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = data.table::fifelse(
        developer_mode,
        yes = 'input/db_dev',
        no = 'input/db'
        )
)

# 4) Extract global
# parameters for the model

# 1) fetch distinct parameters
# from database
get_results <- DBI::dbSendQuery(
    conn = DB_connection,
    statement = "SELECT * FROM parameters"
)

# 1.1) fetch results from
# database
parameters <- data.table::as.data.table(
    DBI::dbFetch(
        res = get_results
    )
)

# 1.2) Clear results and
# prepare for next query
DBI::dbClearResult(get_results)


# 2) extract parameters;

# 2.1) get diseases
diseases <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'disease',
    as_list = TRUE
)



names(diseases) <- stringr::str_to_sentence(
    names(diseases) 
)


# 2.2) get characteristics
gender <-  input_parameters(
    DT = parameters[model == 1],
    variable_ = 'gender'
)

education <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'education'
)

socioeconomics <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'socioeconomic'
)


age <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'age'
)

allocator <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'allocator'
)



get_results <- dbSendQuery(
    conn = DB_connection,
    "
  SELECT * FROM sector
  "
)


# NOTE: Needs some work here
sector <- as.data.table(
    dbFetch(
        res = get_results
    )
)



units <- as.data.table(
  dbFetch(
    dbSendQuery(
      conn = DB_connection,
      "
      SELECT * FROM units
      "
    )
  )
)




# script end;