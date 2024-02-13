# script: scr_SyntheticData
# date: 2023-06-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: This data represents the data
# in its exact format in which it is fed into
# the model.
# script start;

generate_data <- function(
    n = 10
) {
  
  #' function information
  #' 
  #' The data generates disease groups
  #' with allocators for for specific treatment
  #' types.
  #' 
  #' The data is split between incidence and prevalent
  #' patients, with specific characteristics and counts
  #' of patients.
  
  # model 1; #####
  DT <- data.table::data.table(
    
    data.table::CJ(
      k_year      = -2:5,
      
      k_sector   = paste(
        # was: sector
        'sektor',
        1:n
      ),
      
      # TODO: Maybe this has to be called śomething
      # else
      k_disease  = paste(
        # was: disease
        'sygdom',
        1:(n + 10)
      ),
      
      k_allocator = paste(
        'outcome',
        1:(n + 20)
      ),
      
      c_population = c('general', 'study'),
      
      c_gender    = c(
        'mand',
        'kvinde'
      ),
      
      c_education = c(
        'lang',
        'mellemlang',
        'kort'
      ),
      
      c_socioeconomic = c(
        'aktiv',
        'inaktiv',
        'udenfor'
      ),
      
      c_type      = c(
        'prævalent',
        'incident'
      )
    )
  )
  
  DT[
    ,
    `:=`(
      v_qty = round(abs(rt(
        n = .N,
        df = 10
      ) * 20
      )),
      v_cost = round(abs(
        rt(
          n = .N,
          df = 10
        )
      ) * 1000
      ),
      v_weights = round(
        runif(
        .N
      ),
      digits = 2
      ),
      v_obs = round(
        runif(
        .N,
        min = 10000,
        max = 100000
      ),
      digits = 0
      )
    )
    ,
  ][
    ,
    v_avg_cost := round(
      v_cost/v_qty,
      digits = 2
    )
    ,
  ]
  
  
  # add id; 
  # to detect labels
  DT[
    ,
    `:=`(
      id = .GRP
    )
    ,
    by = .(
      k_disease
    )
  ]
  
  # NOTE: We were using IDs before
  # this might break stuff
  
  # add labels
  DT[
    ,
    `:=`(
      c_unit = paste(
        k_allocator,
        'specifik enhed'
      ),
      k_allocator = paste(
        k_sector,
        k_allocator
      )
    )
    
    ,
  ]
  
  
  # store;
  DT_ <- data.table::copy(
    DT
  )
  
  # model 2: ####
  
  DT <- data.table::data.table(
    data.table::CJ(
      # NOTE: Service is the old allocator
      k_sector = paste('agegroup', 1:10),
      c_education = c('kort', 'mellemlang', 'lang'),
      c_unit    = 'model 2 specifik enhed'
    )
  )
  
  DT[
    ,
    `:=`(
      v_qty = round(abs(
        rt(
          n = .N,
          df = 20
        )
      )) * 10,
      v_weight = round(
        runif(
        .N
      ),
      digits = 0
      ),
      n_obs = round(
        runif(
        .N,
        min = 1000,
        max = 23000
      ),
      digits = 0
    )
    )
    ,
  ]
  
  
  
  
  
  return(
    list(
      model_1 = DT_,
      model_2 = DT
    )
  )
  
  
}



# script end;