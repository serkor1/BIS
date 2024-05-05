## code to prepare `DATASET` dataset goes here



# 1) Establish connection
# to data.base
DB_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "inst/extdata/db.sqlite"
)

# 2) Extract all values from
# model2 - NOTE: This only works
# because the values are directly
# inferrable
DT <- extract_data(
  DB_connection = DB_connection,
  table = "model2"
)

# 3) extract parameters
# from the the data
#
# NOTE: there is only k_ types
# here.
idx <- grep(
  pattern     = "k_",
  x           = colnames(DT),
  ignore.case = TRUE
)

DT <- DT[,idx,with = FALSE]


# 3.1) Split the data.table
# because the input_parameters-function
# doent accept vectors
DT_list <- lapply(names(DT), function(col) unique(DT[, ..col, with=FALSE]))

# Names the list elements according to original column names
names(DT_list) <- names(DT)


# 4) Add labels
# to the data
DT_list <- lapply(
  names(DT_list),
  function(name) {

    if (name == "k_sector") {

    DT <- DT_list[[name]]$k_sector

    }

    if (name == "k_education") {

      DT_list[[name]][
        ,
        label := stringr::str_to_title(
          k_education
        )
        ,
      ]

     DT <-  input_parameters(
        DT =   DT_list[[name]],
        value = "k_education"
      )

    }

    if (name == "k_allocator") {

      DT_list[[name]][
        ,
        label := data.table::fcase(
          default = "Delt",
          k_allocator %chin% "low", "Lavest Uddannede",
          k_allocator %chin% "high", "Højest Uddannede"
        )
        ,
      ]

    DT <-   input_parameters(
        DT =   DT_list[[name]],
        value = "k_allocator"
      )

    }

    DT


  }
)

# Names the list elements according to original column names
names(DT_list) <- names(DT)

model2_parameters <- DT_list
usethis::use_data(model2_parameters, overwrite = TRUE)
