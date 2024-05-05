# script: connect_database
# date: 2024-03-29
# author: Serkan Korkmaz, serkor1@duck.com
# objective: This script connects to
# the database
# script start;

DB_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "inst/extdata/db.sqlite"
)

# script end;






