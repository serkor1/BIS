# script: scr_efficiency
# date: 2024-03-18
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Test the efficieny of various
# functions
# script start;

# garbage collection
rm(list = ls()); invisible(gc())

options(warn = -1)
# libraries for
# shiny development
suppressPackageStartupMessages(
    {
        library(shiny)
        library(bslib)
        library(shinyWidgets)
        library(data.table)
        library(workbookR)
        library(DBI)
    }
)

# source; 
invisible(
    lapply(
        X = list.files(
            path = 'R',
            full.names = TRUE,
            recursive = TRUE
        ),
        FUN = source
    )
)

# parameters;
DB_connection <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = data.table::fifelse(
        FALSE,
        yes = 'input/db_dev',
        no = 'input/db'
    )
)



# 4) Extract global
# parameters for the model

# 1) fetch distinct parameters
# from database
get_results <- DBI::dbSendQuery(
    conn = DB_connection,
    statement = "SELECT * FROM parameters"
)

# 1.1) fetch results from
# database
parameters <- data.table::as.data.table(
    DBI::dbFetch(
        res = get_results
    )
)

# 1.2) Clear results and
# prepare for next query
DBI::dbClearResult(get_results)


# 2) extract parameters;

# 2.1) get diseases
diseases <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'disease',
    as_list = TRUE
)



names(diseases) <- stringr::str_to_sentence(
    names(diseases) 
)


# 2.2) get characteristics
gender <-  input_parameters(
    DT = parameters[model == 1],
    variable_ = 'gender'
)

education <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'education'
)

socioeconomics <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'socioeconomic'
)


age <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'age'
)

allocator <- input_parameters(
    DT = parameters[model == 1],
    variable_ = 'allocator'
)



get_results <- dbSendQuery(
    conn = DB_connection,
    "
  SELECT * FROM sector
  "
)


# NOTE: Needs some work here
sector <- as.data.table(
    dbFetch(
        res = get_results
    )
)

units <- as.data.table(
    dbFetch(
        dbSendQuery(
            conn = DB_connection,
            "
      SELECT * FROM units
      "
        )
    )
)


type <- "incident"
k_sector <- 'lægemiddelforbrug'
k_allocator <- 'det offentliges betaling'
k_disease <- 'cancer_tarm'
c_gender <- c("kvinde", "mand")
c_age    <- c("65+ år","18-49 år")
c_socioeconomic <- c("aktiv", "inaktiv", "udenfor")
c_education <- c("faglært", "ufaglært", "videregående uddannelse")

recipe_object <- recipe(
    treatment = list(
        k_disease       = "cancer_bryst",
        c_gender        = c_gender,
        c_education     = c_education,
        c_socioeconomic = c_socioeconomic,
        c_age           = c_age
    ),
    control = list(
        k_disease       = 'cancer_tarm',
        c_gender        = c_gender,
        c_education     = c_education,
        c_socioeconomic = c_socioeconomic,
        c_age           = c_age
    )
)

# foo:
# 
# The foo function is the current setup
# that is embedded in the Application
foo <- function() {
    
    DT <- extract_data(
        DB_connection = DB_connection,
        table         = "model1",
        c_type        = type,
        k_sector      = k_sector,
        k_allocator   = k_allocator
    )
    
    prepare_data(
        recipe = recipe_object,
        DT = DT
    )
    
    
    
}


# bar: 
# 
# The bar function is where everything
# has been reversed
bar <- function() {
    DT <- extract_data(
        DB_connection = DB_connection,
        table         = "model1",
        c_type        = type,
        k_disease       = c('cancer_bryst', "cancer_tarm")
    )
    
    DT <- prepare_data(
        recipe = recipe(
            treatment = list(
                k_disease = "cancer_bryst",
                k_sector = 'lægemiddelforbrug',
                k_allocator = 'det offentliges betaling',
                c_gender        = c_gender,
                c_education     = c_education,
                c_socioeconomic = c_socioeconomic,
                c_age           = c_age
            ),
            control = list(
                k_disease = "cancer_tarm",
                k_sector = 'lægemiddelforbrug',
                k_allocator = 'det offentliges betaling',
                c_gender        = c_gender,
                c_education     = c_education,
                c_socioeconomic = c_socioeconomic,
                c_age           = c_age
            )
        ),
        DT = DT
    )
}

data.table::fsetequal(
    bar(),
    foo()
)



microbenchmark::microbenchmark(
    bar(),
    foo()
)




# script end;