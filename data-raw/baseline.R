# script: Baseline
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-15
# objective: Baseline table prepartion. All data goes into the
# sql server
# script start;

rm(list = ls()); invisible(gc()); devtools::load_all();

# 1) load data; ####
# NOTE: This step has to be replaced by acual data
# at a later point.
# DT <- data.table::data.table(
#   data.table::CJ(
#     k_disease       = model1_parameters$k_disease,
#     c_type          = model1_parameters$c_type,
#     c_age           = model1_parameters$c_age,
#     c_socioeconomic = model1_parameters$c_socioeconomic,
#     c_education     = model1_parameters$c_education,
#     c_gender        = model1_parameters$c_gender,
#     k_allocator     = c(
#       "Aktiv",
#       "Alder",
#       "Faglært",
#       "Inaktiv",
#       "Kvinde",
#       "Mand",
#       "Udenfor",
#       "Ufaglært",
#       "Videregående Uddannelse"
#       )
#   )
# )
#
# DT[
#   ,
#   `:=`(
#     v_obs = runif(
#       n = .N,
#       min = 10000,
#       max = 20000
#     ),
#     v_weights = runif(
#       n = .N,
#       min = 0,
#       max = 1
#     ),
#     v_characteristics = rnorm(
#       n    = .N,
#       mean = 100,
#       sd   = 1
#     )
#   )
#   ,
#   by = .(
#     c_type,
#     k_disease
#   )
# ]
DT <- data.table::fread(
  "inst/extdata/DT_characteristics.csv"
)

data.table::setnames(
  DT,
  old = "v_characteristica",
  new = "v_characteristics"
)


DT[
  ,
  c_group := data.table::fcase(
    default = as.character(span(bsicons::bs_icon("people"), "Alder")),
    grepl(
      pattern = "ufaglært|faglært|videregående uddannelse",
      ignore.case = TRUE,
      x = k_allocator
    ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
    grepl(
      pattern = "mand|kvinde",
      ignore.case = TRUE,
      x = k_allocator
    ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
    grepl(
      pattern = "aktiv|inaktiv|udenfor",
      ignore.case = TRUE,
      x = k_allocator
    ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
  )
  ,
]



# 2) Store in SQL
DB_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = 'inst/extdata/db.sqlite',
)

DBI::dbWriteTable(
  conn      = DB_connection,
  value     = DT[k_treatment == 1],
  name      = "model1_baseline",
  overwrite = TRUE
)

DBI::dbWriteTable(
  conn      = DB_connection,
  value     = DT[k_treatment == 0],
  name      = "model1_baseline_population",
  overwrite = TRUE
)

# script end;
