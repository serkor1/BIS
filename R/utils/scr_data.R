# script: scr_data
# date: 2024-03-01
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create high level functions
# to ease the data process
# script start;

# 1) recipe object
recipe <- function(
        treatment = list(),
        control   = list()
) {
    
    # NOTE: Has to be named
    list(
        treatment = treatment,
        control = control
    )
    
    
}


# 2) Filter function
# 
# This function filters the
# data and is primarily used 
# inside other functions
subset_data <- function(
        DT,
        ...){
    
    # Capture the arguments passed through ...
    args <- list(...)
    column_names <- ...names()
    
    expressions <- mapply(function(column, value) {
        
        if (is.character(value)) {
            # Assuming you want to check if a column's value is in a vector of character values
            sprintf("%s %%chin%% c('%s')", column, paste(value, collapse = "','"))
        } else {
            # For numerical or other types, checking for equality
            sprintf("%s %%in%% c(%s)", column, paste(value, collapse = ","))
        }
    }, column_names, args, SIMPLIFY = FALSE)
    
    # Combine all expressions into a single character string (if needed)
    combined_expression <- paste(expressions, collapse = " & ")
    
    #eval(,envir = parent.env())
    
    DT[eval(parse(text = combined_expression))]
}


# 3) Split and prepare data;
# 
# This function uses the recipe
# to split the data between treatment
# and control.
# 
# 
# Returns a data.table
prepare_data <- function(
        DT,
        recipe) {
    
    
    
    recipe_names <- names(recipe)
    
    
    DT_list <- lapply(recipe, function(x) {
        
        do.call(subset_data, c(list(DT = DT), x))
        
    })
    
    names(DT_list) = recipe_names
    
    lapply(
        recipe_names,
        function(x) {
            
            DT_list[[x]][,k_assignment := x,]
        }
    )
    
    
    data.table::rbindlist(
        DT_list
    )
    
}


# 4) aggregate the data
# this function aggregates
# the data based on user choices

aggregate_data <- function(
        DT,
        calc,
        by = NULL) {

    
    # this function aggregates the data
    # and adds differences to the data
    # so its ready for action
    DT <- DT[
        ,
        eval(
            calc
        )
        ,
        by = c(
            if (is.null(by)) names(DT) else by
        )
       # by = c(by)
    ]
    
    DT[
        k_assignment %chin% 'control',
        c(names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]) := lapply(
            .SD,
            function(x) {
                
                -x
                
            }
        ) 
        ,
        .SDcols = names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]
    ]
    
    DT_ <- DT[
        ,
        lapply(
            .SD,
            function(x) {
                
                sum(x)
                
            }
        ) 
        ,
        .SDcols = names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))],
        by = c(
            names(DT)[!grepl(pattern = "k_assignment|k_disease|v_",x = names(DT))]
        ) 
    ][
        ,
        k_assignment := "difference"
        ,
    ]
    
    DT <- rbind(
        DT,
        DT_,
        fill = TRUE
    )
    
    DT[
        k_assignment %chin% 'control',
        c(names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]) := lapply(
            .SD,
            function(x) {
                
                -1*x
                
            }
        ) 
        ,
        .SDcols = names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]
    ]
    
    
    
    
    DT[]
    
}



# 5) add effects
effect_data <- function(
        DT,
        index = "k_year",
        effect = data.table(
            k_year = 0,
            effect = 0.5
        )) {
    
    # 0) Merge the data
    DT <- merge(
        DT,
        effect
    )
   
    
    # Take a copy
    # of the DT
    DT_ <- copy(
        DT
    )
    
    
    DT_[
        k_assignment %chin% 'difference',
        c(names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]) := lapply(
            .SD,
            function(x) {
                
                -x
                
            }
        ) 
        ,
        .SDcols = names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))]
    ]
    
    DT_ <- DT_[
        k_assignment %chin% c("treatment", "difference")
        ,
        lapply(
            .SD,
            function(x) {
                
                sum(x)
                
            }
        ) 
        ,
        .SDcols = names(DT)[grep(pattern = "v_", ignore.case = TRUE, x = names(DT))],
        by = c(
            names(DT)[!grepl(pattern = "k_assignment|k_disease|v_",x = names(DT))]
        ) 
    ][
        ,
        k_assignment := "counter_factual"
        ,
    ]
    
    
    
   rbind(
       DT,
       DT_[k_assignment %chin% "counter_factual"],
       fill = TRUE
   )
    
    
    
    
    # DT <- rbind(
    #     fill = TRUE,
    #     DT,
    #     DT_[
    #         ,
    #         lapply(
    #             .SD,
    #             function(x) {
    #                 
    #                 abs(x[k_assignment %chin% 'treatment'] - x[k_assignment %chin% 'difference'] * effect)
    #                 
    #             }
    #         )
    #         ,
    #         by = c(
    #             index, "effect"
    #         ),
    #         .SDcols = names(DT)[grep(pattern = "v_", x = names(DT),ignore.case = TRUE)]
    #     ][
    #         ,
    #         k_assignment := "counter_factual"
    #         ,
    #         
    #     ]
    # )
    # 
    # 
    # data.table::setkey(
    #     DT,
    #     k_year
    # )
    # 
    # DT[]
    
}


# script end;

