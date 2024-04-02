# script: scr_tables
# date: 2024-04-01
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate Tables
# script start;

rm(list = ls()); gc();
devtools::load_all()


# 1) create data.table
# (not necessary)
DT <- head(data.table::as.data.table(
  mtcars
),n = 10)

DT[
  ,
  group := rep(c(1,2), each = 5)
  ,
]

# 2) The headers can be created
# as a table
add_header(
  column_names = names(DT),
  where =  list(
    `Group Header` = c("mpg", "cyl"),
    `Group 2 Header` = c("disp", "hp")
  )
)


# 3) These can be passed into the
# into the table function
generate_table(
  DT = DT,
  header = list(
    `Group Header` = c("disp", "hp")
  )
)



