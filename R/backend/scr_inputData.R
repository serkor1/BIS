# script: scr_inputData
# date: 2023-06-15
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate a class of functions that 
# prepares the data for the model
# script start;

# grinders; #####
# 
# 
# The grinder class functions reduces
# the size of the DT by identifying
# treatment and control groups and
# filtering them as such.

.prepare_model1 <- function(
    recipe,
    DT,
    name_class = 'model1'
) {
  
  
  # local variables extracted
  # from the recipe objec
  treatment_char <- recipe$treatment$char
  treatment_disease <- recipe$treatment$disease
  
  control_char <- recipe$control$char
  control_disease <- recipe$control$disease
  
  
  # 1) Filter by incidence
  # DT_ <- DT[
  #   type %chin% recipe$incidence
  # ]
  
  DT_ <- copy(DT)
  # 2) add indicators of 
  # treatement and control
  
  treatment_cols <- c(
    'k_disease',
    names(treatment_char)
  )
  
  control_cols <- c(
    'k_disease',
    names(control_char)
  )
  
  
  treatment_values <- c(
    treatment_disease,
    unlist(treatment_char)
  )
  
  control_values <- c(
    control_disease,
      unlist(control_char)
    
  )
  
  # BUG: If the treatment group set
  # is smaller than control - then
  # treatment doesnt exist.
  # Needs a fix
  if (length(treatment_values) < length(control_values)) {
    
    
    
    DT_[
      DT_[
        ,
        Reduce(
          `&`,
          lapply(
            .SD,
            `%chin%`,
            control_values
          )
        ),
        .SDcols = control_cols
      ],
      k_assignment := 'control'
    ]
    
    DT_[
      DT_[
        ,
        Reduce(
          `&`,
          lapply(
            .SD,
            `%chin%`,
            treatment_values
          )
        ),
        .SDcols = treatment_cols
      ],
      k_assignment := 'treatment'
    ]
    
  } else {
    
    
    DT_[
      DT_[
        ,
        Reduce(
          `&`,
          lapply(
            .SD,
            `%chin%`,
            treatment_values
          )
        ),
        .SDcols = treatment_cols
      ],
      k_assignment := 'treatment'
    ]
    
    DT_[
      DT_[
        ,
        Reduce(
          `&`,
          lapply(
            .SD,
            `%chin%`,
            control_values
          )
        ),
        .SDcols = control_cols
      ],
      k_assignment := 'control'
    ]
    
  }
  
  
  
  # filter data according
  # to treatment and control
  DT_ <- DT_[
    !is.na(k_assignment)
  ]
 
  
  return(
    DT_[]
  )
  
}


prepare <- function(
  recipe,
  DT,
  name_class = 'model1'
) {
  
  #' function information
  #' 
  #' This function filters and adds
  #' treatment and control indicators on 
  #' the data.
  #' 
  #' 
  #' @param recipe A recipe object passed.
  #' @param DT with a specific class of either
  #' model1 or model2
  
  cli::cli_inform(
    message = c('i' = 'Preparing data')
  )
  
  
  class(DT) <- c(
    class(DT), name_class
  )
  
  
  
  if (!(inherits(DT, 'model1') | inherits(DT, 'model2'))) {
    
    stop(
      call. = FALSE,
      'DT has no relevant class!'
    )
    
  }
  
  
  if (inherits(DT, 'model1')) {
    
    DT <- .prepare_model1(
      recipe = recipe,
      DT     = DT
    )
    
  } else {
    
    
    
  }
  
  # Add class of prepared to the
  # data so future metadata can be extracted
  class(DT) <- c(class(DT), 'prepared')
  
  return(
    DT
  )
  
}


# cook data; ######
# 
# The cook data functions aggregates
# the DT so it is ready for futher 
# analysis as according to the sliders, 
# and patient counters
.cook_model1 <- function(DT) {
  
  
  
  
  
  # Aggregate the data
  DT_ <- DT[
    ,
    .(
      v_qty = sum(
        v_qty * v_weights, na.rm = TRUE
      )/sum(v_weights, na.rm = TRUE),
      
      v_cost = sum(
        v_cost * v_weights, na.rm = TRUE
      )/sum(v_weights, na.rm = TRUE)
    )
    ,
    by = .(
      k_year,
      k_sector,
      k_disease,
      k_assignment,
      k_allocator,
      c_type
    )
  ]
  
  # DT_[
  #   ,
  #   v_avg_cost := v_cost / v_qty
  #   ,
  # ]
  
  # Genereate Meta data
  # ie. population size
  DT_meta <- DT[
    ,
    .(
      v_obs = sum(
        v_obs,
        na.rm = TRUE
      )
    )
    ,
    by = .(
      k_disease,
      k_assignment
    )
  ]
  
  
  return(
    list(
      DT = DT_,
      DT_meta = DT_meta
    )
    
    )
  
}



cook <- function(
    DT
    ) {
  
  
  cli::cli_inform(
    message = c('i' = 'Cooking data')
  )
  
  if (!inherits(DT, 'prepared')){
    
    stop(
      call. = FALSE,
      'The data has not been prepared.'
    )
    
  }
  
  
  
  
  if (inherits(DT, 'model1')) {
    
    DT_ <- .cook_model1(
      DT
    )
    
  } else {
    
    
    
  }
  
  return(
    DT_
  )
  
}


# flavor data;
# 
# The flavored DT 
# is the cooked DT with all the relevant effects


.flavor_model1 <- function(
    DT,
    effect = rep(x = 0.5, 5)
) {
  
  
  # 1) take difference;
  # of the actual values
  DT_ <- DT[
    ,
    lapply(
      .SD,
      function(x) {
        
        abs(x[k_assignment %chin% 'treatment'] - x[k_assignment %chin% 'control'])
        
      }
    )
    ,
    by = .(
      k_year,
      k_sector,
      k_allocator,
      c_type
    ),
    .SDcols = c(
      'v_qty',
      'v_cost'
      # ,
      # 'v_avg_cost'
    )
  ]
  
  
  # 2) add difference
  # indicator
  DT_diff <- copy(
    DT_
  )
  
  DT_diff[
    ,
    `:=`(
      k_assignment = 'actual_difference',
      k_disease = 'actual_difference'
    )
    ,
  ]
  
  
  # 3) add effect values
  # and remove from the difference
  DT_effect <- data.table(
    k_year = -2:5,
    effect = c(0,0,0, effect)
  )
  
  
  
  DT_ <- merge(
    DT_,
    DT_effect,
    all.x = TRUE,
    by = c('k_year')
  )
  
  
  
  # DT_[
  #   ,
  #   effect := c(
  #     rep(0,3), effect
  #   )
  #   ,
  #   by = .(
  #     c_type,
  #     k_sector,
  #     k_allocator
  #   )
  # ]
  
  DT_[
    ,
    (c(
      'v_qty',
      'v_cost'
      # ,
      # 'v_avg_cost'
    )) := lapply(
      .SD,
      function(x) {
        
        x * effect
        
      }
    ) 
    ,
    .SDcols = c(
      'v_qty',
      'v_cost'
      # ,
      # 'v_avg_cost'
    )
  ]
  
  
  
  
  # 3) rbind with treatment
  # values to extract the 
  # counter factuals value
  # of the disease.
  DT_ <- rbind(
    DT[k_assignment %chin% 'treatment'],
    DT_,
    fill = TRUE
  )
  
  DT_ <- DT_[
    k_year > 0
    ,
    lapply(
      .SD,
      function(x) {
        
        x[k_assignment %chin% 'treatment'] - x[is.na(k_assignment)]
        
        
        
      }
    )
    ,
    by = .(
      k_year,
      k_sector,
      k_allocator,
      c_type
    ),
    .SDcols = c(
      'v_qty',
      'v_cost'
      # ,
      # 'v_avg_cost'
    )
  ]
  
  DT_ <- rbind(
    DT,
    DT_,
    fill = TRUE
  )
  
  
  # 4) add indicators
  # for plotting and table implementation
  DT_[
    is.na(k_disease),
    k_disease := unique(
      DT[k_assignment %chin% 'treatment']$k_disease
    )
  ][
    is.na(
      k_assignment
    ),
    k_assignment := 'counter_factual'
  ]
  
  
  DT_ <- rbind(
    DT_diff,
    DT_,
    fill = TRUE
  )
  
  
  # 5) coutnerfactual 
  # difference
  DT_diff <- copy(
    DT_[
      k_year > 0
      ,
      lapply(
        .SD,
        function(x) {
          
         # x[k_assignment %chin% 'treatment'] - x[k_assignment %chin% 'counter_factual']
          
          x[k_assignment %chin% 'counter_factual'] - x[k_assignment %chin% 'control']
        }
      )
      ,
      by = .(
        k_year,
        k_sector,
        k_allocator,
        c_type
      ),
      .SDcols = c(
        'v_qty',
        'v_cost'
        # ,
        # 'v_avg_cost'
      )
    ]
  )
  
  DT_diff[
    ,
    `:=`(
      k_assignment = 'counter_factual_difference',
      k_disease = 'counter_factual_difference'
    )
    ,
  ]
  
  DT_ <- rbind(
    DT_diff,
    DT_,
    fill = TRUE
  )
  
  
  # 5) Round values
  DT_[
    ,
    `:=`(
      v_qty = round(v_qty, 2),
      v_cost = round(v_cost, 2)
      # ,
      # v_avg_cost = round(v_avg_cost, 2)
    )
    ,
  ]
  
  
  
  
  return(
    DT_
  )
  
}



flavor <- function(DT, effect = rep(x = 0.5, 5)) {
  
  # extract the data 
  # from the list
  DT_ <- DT$DT
  
  cli::cli_inform(
    message = c('i' = 'Flavoring data')
  )
  
  
  # NOTE: maybe its
  if (inherits(DT_, 'model1')) {
    
    DT_ <- .flavor_model1(
      DT = DT_,
      effect = effect
    )
    
  } else {
    
    
  }
  
  
  
  return(
    DT_[]
  )
  
}


# script end;