## code to prepare `model1_parameters` dataset goes here

# 1) Establish connection
# to data.base
DB_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "inst/extdata/db"
)

# 2) Extract all parameters
# from db
DT <- extract_data(
  DB_connection = DB_connection,
  table = "parameters",
  model = 1
)

# 3) split by variable
DT_list <- split(
  x = DT,
  f = DT$variable
)

# 3.1) The diseases
# should be split by type
# DT_list$k_disease[
#   ,
#     c('group', 'label') := data.table::tstrsplit(
#       label,
#       "_"
#     )
#
#   ,
# ][]




# 4) Prepare parameters
get_names <- names(DT_list)

model1_parameters <- lapply(
  X = DT_list,
  FUN = function(DT) {

    input_parameters(
      DT = DT
    #
    #   as_list = if (any(
    #     grepl(
    #       pattern = "disease",
    #       x = DT$variable
    #     )
    #   )
    # ) "group" else NULL
    )


  }
)



model1_parameters$sector <- extract_data(
  DB_connection = DB_connection,
  table = "sector"
)





usethis::use_data(
  model1_parameters,
  overwrite = TRUE
  )
