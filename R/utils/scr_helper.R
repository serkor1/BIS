# script: helper functions
# author: Serkan Korkmaz
# date: 2023-06-14
# objective: These functions are non-essential functions
# to reduce code repetition
# script start; ####


#' .panel_ui <- function(
#'     condition,
#'     ns = NULL,
#'     first_panel = ...,
#'     second_panel = ...
#'     ) {
#'   
#'   #' function information
#'   #' 
#'   #' @param condition a string on the form 'input.foo'
#'   #' or 'output.foo'. It only takes TRUE or FALSE values.
#'   #' @param first_panel a UI element in the
#'   #' main panel, ie. model 1
#'   #' @param second_panel a UI element in the secondary
#'   #' åanel, ie. model. 2
#'   #' 
#'   #' Each panel element can be wrapped in a tagList
#'   #' to add more elements to the conditional
#'   #' panel
#'   
#'   # error handlers;
#'   if (missing(first_panel) | missing(second_panel)) {
#'     
#'     stop(
#'       call. = FALSE,
#'       'No panel elements provided.'
#'     )
#'     
#'   }
#'   
#'   # Has to be wrapped in a tagList
#'   # otherwise it will not show
#'   tagList(
#'     conditionalPanel(
#'       condition = paste0(condition),ns = ns,
#'       {
#'         
#'         first_panel
#'         
#'       }
#'     ),
#'     conditionalPanel(
#'       condition = paste0('!', condition), ns = ns,
#'       {
#'         
#'         second_panel
#'         
#'       }
#'     )
#'   )
#' }


#' # recipe objects
#' .recipe_object <- function(
#'   disease_treatment,
#'   disease_control,
#'   char_treatment = NULL,
#'   char_control = NULL
#' ) {
#'   
#'   #' function information
#'   #' 
#'   #' @param incidence a character vector of length one
#'   #' 
#'   #' @param disease_ a character vector of length 1
#'   #' indicating the type disease to be examined
#'   #' 
#'   #' @param char_ a character vector with demographical
#'   #' characteristics. Can be NULL. HAS TO BE NAMED LIIST
#'   
#'   # # error control;
#'   # if (!inherits(incidence, 'character')) {
#'   #   
#'   #   cli::cli_abort(
#'   #     message = c(
#'   #       '!' = '{.var incidence} has to be class {.var character}',
#'   #       'i' = 'You supplied class {.cli {class(incidence)}}'
#'   #     )
#'   #   )
#'   #   
#'   # }
#'   
#'   
#'   list(
#'     treatment = list(
#'       disease = disease_treatment,
#'       char    = char_treatment
#'     ),
#'     control   = list(
#'       disease = disease_control,
#'       char    = char_control
#'     )
#'   )
#'   
#'   
#'   
#' }

# 
# .color_gradient <- function(color, alpha = 0.5, percent, invert = FALSE) {
#   
#   # get color code;
#   color_code    <- alpha(color,alpha = alpha)
#   
#   
#   if (invert) {
#     
#     percent <- paste0(100-percent, '%')
#     
#   } else {
#     
#     percent <- paste0(percent, "%")
#   }
#   
#   
#   # background color;
#   regular <- paste0(
#     'background: ',
#     alpha('white'),
#     ';'
#   )
#   
#   # -moz-linear gradient
#   moz <-  paste0(
#     'background: -moz-linear-gradient(90deg, ', color_code, ' ' ,percent, ');'
#   )
#   
#   webkit <-  paste0(
#     'background: -webkit-linear-gradient(90deg, ', color_code, ' ' ,percent, ');'
#   )
#   
#   
#   linear_gradient <- paste0(
#     'background: linear-gradient(90deg, ', color_code, ' ' ,percent, ');'
#   )
#   
#   css <- paste(
#     c(
#       regular,
#       moz,
#       webkit,
#       linear_gradient
#     ),
#     collapse = ' '
#   )
#   
#   return(
#     css
#   )
# }



# # generate headers; 
# 
# container_foo <- function(colnames) {
#   
#   cli::cli_warn(
#     message = c(
#      '!' =  'Container foo is running',
#      'i' = 'When the format is finalized, the container should be hardcoded'
#     )
#   )
#   
#   # The function should take the amount of colnames
#   # with splits, and get the names
#   colnames <- colnames[grepl(
#     pattern = '//',
#     x = colnames,
#     ignore.case = TRUE 
#   )]
#   
#   # initialise data
#   container <- data.table()
#   
#   # add labels;
#   container[
#     ,
#     c('header', 'subheader') := tstrsplit(
#       colnames, '//'
#     )
#     ,
#   ]
#   
#   # calcuilate colspoans
#   container[
#     ,
#     colspan := .N
#     ,
#     by = .(
#       header
#     )
#   ]
#   
#   # calculate repetitions
#   container[
#     ,
#     rep := uniqueN(
#       header
#     )
#     ,
#   ]
#   
#   container <- list(
#     header    = split(
#       unique(
#         container[
#           ,
#           .(
#             header,
#             colspan
#           )
#           ,
#         ]
#       )
#       ,
#       by = 'header'),
#     subheader = unique(container[,.(subheader, rep),])
#   )
#   
#   
#   container_ <-  htmltools::withTags(
#     table(
#       class = 'display',
#       thead(
#         # Top columns
#         tr(
#           th(rowspan = 2, 'Tid'),
#           # The length of which the
#           # top column should cover
#           # The th colspan is the span of the
#           # variable
#           # ie. mpg_Automatic mpg_Manual and
#           # the name is the mpg
#           # the number of th's is the
#           # nummber of variables
#           lapply(
#             container$header,
#             function(x) {
#               
#               th(colspan = x$colspan, x$header)
#               
#             }
#           )
#         ),
#         tr(
#           
#           # The name of the varibles that
#           # the to columns should cover and how many.
#           # ie. automatic and manual, with the length
#           # being the number of variables
#           lapply(
#             rep(
#               container$subheader$subheader, unique(container$subheader$rep)
#             ),
#             th
#           )
#         )
#         
#       )
#     )
#   )
#   
#   
#   
#   
#   return(
#     container_
#   )
# }

# load input parameters;

input_parameters <- function(
    DT,
    variable_ = NULL,
    as_list = FALSE
) {
  
  # NOTE: names is what is visible
  # to the end-user.
  # the remaining is backend
  
  
  # this function
  # loads the input parameters
  # while preserving the values
  # and adds labels
  # 
  #' @param label the label values of the
  #' inputs shown to the user
  #' @param values the actual values fed
  #' into the model
  #' @param as_list logical. If the input values should be
  #' returned as a named and ordered list
  DT_ <- DT[
    grepl(
      pattern = variable_,
      x = tolower(variable)
    )
  ]
  
  
  
  if (as_list) {
    
    
    DT_ <- DT_[
      ,
      group := tstrsplit(x = value, '_')[[1]]
      ,
    ]
    
    
    parameters <- DT_$value
    names(parameters) <- DT_$label
    
    parameters <- split(
      DT_,
      f = DT_$group
    )
    
    
    parameters <- lapply(
      parameters,
      function(x) {
        
        parameters <- x$value
        
        
        names(parameters) <- do.call(rbind,strsplit(x$label, split = '_'))[,2]
        
        return(parameters)
      }
    )
    
    
    
  } else {
    
    
    parameters <- DT_$value
    names(parameters) <- DT_$label
    
    
  }
  
  
  return(parameters)
  
}



# generate_styles <- function(
#     styles = c('background: blue')
# ) {
#   
#   
#   paste(
#     styles,
#     collapse = '; '
#   )
#   
#   
#   
# }

# end of script; ####