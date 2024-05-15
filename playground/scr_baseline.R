# script: scr_baseline
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-15
# objective: Generate Baseline table
# script start;

rm(list = ls()); invisible(gc()); devtools::load_all()


DT <- extract_data(
  DB_connection = DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = "inst/extdata/db.sqlite"
  ),
  table = "model1_baseline",
  k_disease = c("astma", "psoriasis"),
  c_type    = "Incident"
)


# DT <- data.table::fread(
#   "inst/extdata/adhd_characteristica.txt"
# )
#
# data.table::setkey(
#   DT,
#   k_allocator
# )
#
# unique(DT$k_allocator)
#
# DT[
#   c_socioeconomic %chin% "Udenfor"
#   ,
#   .(
#     v_characteristica = sum(v_weights * v_characteristica, na.rm = TRUE) / sum(v_weights, na.rm = TRUE)
#   )
#   ,
#   by = .(
#     k_allocator
#   )
# ]


# script end;
