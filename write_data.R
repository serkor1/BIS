# script: write_date
# date: 2024-03-01
# author: Serkan Korkmaz, serkor1@duck.com
# objective: This script
# writes the data into the SQL-database
# script start;

rm(list = ls())

cli::cli_inform(
    message = c(
       "x" = "This script only works as long as you have an existing database"
    )
)


developer_mode <- FALSE


# 1) Extract data from
# the database
DB_connection <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = data.table::fifelse(
        developer_mode,
        yes = 'input/db_dev',
        no = 'input/db'
    )
)

# 1) fetch distinct parameters
# from database
get_results <- DBI::dbSendQuery(
    conn = DB_connection,
    statement = "SELECT * FROM model1"
)

# 1.1) fetch results from
# database
DT <- data.table::as.data.table(
    DBI::dbFetch(
        res = get_results
    )
)

# 1.2) Clear results and
# prepare for next query
DBI::dbClearResult(get_results)


data.table::setkey(
    DT,
    k_year
)

DT <- unique(DT[
    k_year == 0
][
    ,
    .(
        k_disease,
        c_type,
        c_population,
        c_gender,
        c_age,
        c_socioeconomic,
        c_education,
        v_obs
    )
    ,
])



DT <- melt(
    data = DT,
    id.vars = c("k_disease", "c_type", "c_population", "v_obs"),
    measure.vars = c('c_education', "c_gender", 'c_age', 'c_socioeconomic'),
    value.name = "c_characteristic",
    
    variable.factor = FALSE
)[
    ,
    variable := NULL
    ,
]

DBI::dbWriteTable(
    conn = DB_connection,
    value = DT,
    name = "population_count"
)






# script end;