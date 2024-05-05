# script: scr_sampleData
# date: 2023-07-10
# author: Serkan Korkmaz, serkor1@duck.com
# objective: This script generates sample data
# and stores it in a SQL Lite database. The actual
# database is generate outside this sandbox found at github
# script start;

# gc
rm(list = ls()); invisible(gc())


cli::cli_inform(
  message = c(
    'i' = 'Starting script'
  )
)


# generate data base; ####
#
# generate synthetic data
# for the model and store
# in SQL Lite database

# 1) generate data;
DT_list <- list(
  data.table::fread(
  "inst/extdata/DT.csv"
  )
)


# 2) generate SQL Lite
# database
DB_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = 'inst/extdata/db',
)

# 3) clear table if any;
invisible(
  {
    lapply(
      X = c('model1'),
      FUN = function(name) {
        suppressMessages(
          {
            try(
              {
                DBI::dbRemoveTable(
                  conn = DB_connection,
                  name = paste(
                    name
                  )
                )
              }
            )
          }
        )


      }
    )
  }
)


# 4) create table
# for each model
model <- c('model1')

i <- 1
invisible(
  {
    lapply(
      X = DT_list,
      FUN = function(DT) {

        # k_treatment is the study population
        DT <- DT[
          k_treatment == 1
        ]

        DT[
          ,
          k_treatment := NULL
          ,
        ]


        DBI::dbWriteTable(
          conn = DB_connection,
          value = DT,
          name = paste(
            model[i]
          )
        )

        i <<- i + 1


      }
    )
  }
)

# 5) create a table for
# general population
DBI::dbWriteTable(
  con = DB_connection,
  value = DT_list[[1]][k_treatment == 0][,k_treatment := NULL,],overwrite = TRUE,
  name = 'population'
)


# 5) write units;
unit <- data.table::rbindlist(
  lapply(
    seq_along(DT_list),
    function(i) {

      idx <- which(grepl(
        pattern = 'unit|allocator',
        x = colnames(DT_list[[i]])
      ))


      DT <- unique(DT_list[[i]][
        ,
        idx,
        with = FALSE
      ])


      DT[
        ,
        model := i
        ,
      ]

      return(
        DT
      )


    }


  ),fill = TRUE
)


DBI::dbWriteTable(
  conn = DB_connection,
  value = unique(
    unit
  ),
  name = 'units',
  overwrite = TRUE
)



# 6) write parameters;
#
# These parameters are the
# input values that can be chosen by the
# user
i <- 1

parameters <- data.table::rbindlist(
  lapply(
    X = DT_list,
    FUN = function(DT) {

      # 1) detect all char
      # columns
      idx <- which(grepl(
        pattern = 'c_|disease|allocator|sector',
        x = colnames(DT)
      ))

      # 2) extract
      DT <- DT[
        ,
        idx,
        with = FALSE
      ]

      # 3) add indicator
      DT[
        ,
        `:=`(
          model = i
        )
        ,
      ]

      i <<- i + 1

      DT <- data.table::melt(
        DT,
        id.vars = 'model'
      )




      DT <- unique(
        DT
      )

      # remove prefix and
      # TODO: needs to be translated

      # DT[
      #   ,
      #   `:=`(
      #     label = stringr::str_to_sentence(
      #       value
      #     )
      #   )
      #   ,
      # ]

      return(DT[])
    }
  )
)



# add sectors from model
# 1
DBI::dbWriteTable(
  conn = DB_connection,
  value = unique(
    DT_list[[1]][,k_sector,k_allocator,]
  ),
  name = 'sector',
  overwrite = TRUE
)



# 5) write units
DBI::dbWriteTable(
  conn = DB_connection,
  value = parameters,
  name = 'parameters',
  overwrite = TRUE
)

# script end;
cli::cli_inform(
  message = c(
    'i' = 'Script end!'
  )
)



