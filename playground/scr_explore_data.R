rm(list = ls()); invisible(gc());

devtools::load_all()



disease <- c("Prostatakræft")

DT <- extract_data(
  DB_connection = DB_connection,
  # DB_connection = DBI::dbConnect(
  #   drv = RSQLite::SQLite(),
  #   dbname = "inst/extdata/db.sqlite"
  # ),
  k_disease = disease
)



DT <- DT[
  k_allocator %chin% "Personlig pleje"
]



DT <- prepare_data(
  DT = DT,
  recipe = recipe(
    treatment = list(
      k_disease = disease[1]
    ),
    control = list(
      k_disease = disease[2]
    )
  )
)


DT <- aggregate_data(
  DT = DT,
  calc = expression(
    .(
      v_qty = round(
        x = sum(
          v_qty * v_weights, na.rm = TRUE
        )/sum(v_weights, na.rm = TRUE),
        digits = 2
      ),

      v_cost = round(
        x = sum(
          v_cost * v_weights, na.rm = TRUE
        )/sum(v_weights, na.rm = TRUE),
        digits = 2
      ),
      qty_missing = all(is.na(v_qty)),
      cost_missing = all(is.na(v_cost))
    )
  ),
  by = c(
    "k_year",
    "k_sector",
    "k_disease",
    "k_assignment",
    "k_allocator",
    "c_type",
    "c_unit"
  )
)



DT <- effect_data(
  DT = DT,
  effect = data.table::data.table(
    effect = c(
      0,
      0,
      0,
      50/100,
      50/100,
      50/100,
      50/100,
      50/100

    ),
    k_year = -2:5
  )

)[
  k_allocator %chin% "Kommunal sygepleje"
]


DT <- DT[
  k_assignment %chin% c('control', 'treatment', 'counter_factual')
][
  ,
  k_assignment := data.table::fcase(
    default = 'Sygdomsgruppe',
    k_assignment %chin% 'control', 'Sammenligningsgruppe',
    k_assignment %chin% 'counter_factual', 'Kontrafaktisk sygdomsgruppe'
  )
  ,
]


plotly::plot_ly(
  data = DT,
  x = ~k_year,
  y = ~v_qty,
  color = ~k_assignment
)
