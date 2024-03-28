# script: scr_Examples
# date: 2024-03-28
# author: Serkan Korkmaz, serkor1@duck.com
# objective: How to use the data-wrangling
# functions
# script start;

# 1) Define recipe
# object
get_recipe <- recipe(
  treatment = list(
    k_disease = "andre lidelser_alkohol misbrug",
    c_gender  = "mand"
  ),
  control = list(
    k_disease =  "andre lidelser_leddegigt",
    c_gender = "mand"
  )
)

# 2) Extract data
# from database
DT <- extract_data(
  DB_connection = DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = "inst/extdata/db"
  ),
  k_disease = c("andre lidelser_alkohol misbrug", "andre lidelser_leddegigt"),
  c_gender  = c("mand")
)


#  If nothing else is supplied
#  it will return everything
#  from the chosen diseases

# 3.1) Prepare the
# data by adding
# treatment and control indicators
# by list
DT <- prepare_data(
  DT = DT,
  recipe = get_recipe
)



# 3.2) Aggregate the
# data
DT <- aggregate_data(
  DT = DT,
  calc = expression(
    .(
      v_qty = sum(
        v_qty * v_weights, na.rm = TRUE
      )/sum(v_weights, na.rm = TRUE),

      v_cost = sum(
        v_cost * v_weights, na.rm = TRUE
      )/sum(v_weights, na.rm = TRUE)
    )
  ),
  by = c(
    "k_year",
    "k_sector",
    "k_disease",
    "k_assignment",
    "k_allocator",
    "c_type"
  )
)

# 3.3) Add effects
# to the data
DT <- effect_data(
  DT =DT,
  effect = data.table::data.table(
    effect = c(
      runif(
        n = 5
      )
    ),
    k_year = -2:5
  )
)



# script end;
