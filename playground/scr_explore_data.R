rm(list = ls()); invisible(gc());

devtools::load_all()

DT <- extract_data(
  DB_connection = DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = "inst/extdata/db.sqlite"
  )
)

unique(DT$k_sector)

DT[
  k_disease %chin% c("adhd", "alkoholmisbrug") & k_sector %chin% "Praksissektor"
][
  ,
  .(
    v_qty = mean(v_qty, na.rm = TRUE),
    v_cost = mean(v_qty, na.rm = TRUE)
  )
  ,
]
