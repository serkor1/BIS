## code to prepare `baseline` dataset goes here

# 1) extract data without anu
# arguments what so ever
#
# This will extract ALL
# available data. Its equivalent
# of SELECT * FROM table
DT <- extract_data(
  DB_connection = DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = "inst/extdata/db.sqlite"
  )
)

# 2) From baseline year
# take all pairwise unique values
# This is necessary as these numbers are
# duiplicated across sectors.
DT <- unique(
  DT[
    k_year == 0
    ,
    .(
      k_disease = k_disease,
      c_education = c_education,
      c_age = c_age,
      c_socioeconomic  = c_socioeconomic,
      c_gender = c_gender,
      c_type = c_type,
      v_obs = v_obs
    )
    ,

  ]
)

# 3) For each k_disease
# and c_type count the number
# of each category in the
# other columns

# 3.1) Define unique
# diseases
disease <- unique(DT$k_disease)
type    <- unique(DT$c_type)


# 3.2) define the columns
# needed
columns_needed <-  colnames(DT)[
  grepl(pattern = "educ|age|socioeconomic|gender", x = colnames(DT))
  ]


DT <- data.table::rbindlist(
  lapply(
  X = disease,
  FUN = function(x) {

    # For each disease
    # filter by each
    # c_type
    type_list <- data.table::rbindlist(
      lapply(
      X = type,
      FUN = function(y) {

        DT <- DT[
          k_disease %chin% x & c_type %chin% y
        ]

        data.table::rbindlist(
          lapply(
            X = columns_needed,
            FUN = function(z) {

              DT[
                ,
                .(

                  k_disease   = x,
                  c_type      = y,
                  k_variable  = z,
                  v_obs = sum(
                    v_obs,
                    na.rm = TRUE
                  )
                )
                ,
                by = c(
                  c_variable = z
                )
              ]

            }
          ),
          fill = TRUE
        )

      }
    )
    )



  }


),
fill = TRUE
)

DT[
  ,
  model := "model1"
  ,
]


DB_connection = DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "inst/extdata/db.sqlite"
)


DBI::dbWriteTable(
  conn = DB_connection,
  value =DT,
  name = 'baseline',
  overwrite = TRUE
)

