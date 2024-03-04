# script: ui_model1
# date: 2023-06-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate the Userinterface for model 1
# in a shiny module
# script start;


.ui_model1_sidebar <- function(
    input,
    output,
    id) {
  
  # 1) set the namespace
  ns <- NS(id)
  
  # 2) wrap
  # everything in tagList
  shiny::tagList(
    
    # 2.1) Sector-choices
    shinyWidgets::pickerInput(
      inputId  = ns('sector'),
      label    = 'Sektor',
      # TODO: This wasnt necessary before.
      # Something changed.
      choices  = unique(sector_model1$k_sector),
      multiple = FALSE,
      options  = list(
        `live_search` = TRUE
      )
    ),
    
    shinyWidgets::pickerInput(
      inputId  = ns('subsector'),
      label    = 'Outcomes',
      multiple = FALSE,
      choices  = NULL,
      selected = '',
      width    = '100%'
      
    ),
    
    shinyWidgets::pickerInput(
      inputId = ns('type'),
      label = 'Patienttype',
      choices = c(
        'Incident' = 'incident',
        'Prævalent'= 'prævalent'),
      multiple = FALSE,
      selected = 'Incident',
      width = '100%'
    )
    
    
  )
  
}








# .ui_model1_body <- function(
#     input,
#     output,
#     id
# ) {
#   
#   # set namespace
#   ns <- NS(id)
#   
#   parameter_ui <- tagList(
#     segment(
#       class = 'attached',
#       shiny.semantic::steps(
#         id = "steps",
#         class = 'attached four',
#         steps_list = list(
#           single_step(
#             id = "step_1",
#             step_class = 'active',
#             title = "Vælg sektor og patienttype",
#             description = "Sundhedssektor, specifik outcome og patienttype",
#             icon_class = "hospital"
#           ),
#           single_step(
#             id = "step_2",
#             title = "Vælg sygdomsgruppe",
#             description = "Sygdommen og sygdomsbærerens karakteristika",
#             icon_class = "stethoscope",
#             step_class = 'active'
#           ),
#           single_step(
#             id = "step_3",
#             title = "Vælg sammenligningsgruppe",
#             description = "Sammenligningsgruppen og grruppens karakteristika",
#             icon_class = "blind",
#             step_class = 'active'
#           ),
#           single_step(
#             id = "step_3",
#             title = "Vælg forventet effekt",
#             description = "Den forventede årlige effekt i procent",
#             icon_class = "sliders horizontal",
#             step_class = 'active'
#           )
#         )
#       )
#     ),
#     segment(
#       class = 'attached',
#       grid(
#         grid_template = grid_template(
#           list(
#             areas = cbind(
#               'first',
#               'second',
#               'third',
#               'fourth'
#             ),
#             rows_height = 'auto',
#             cols_width  = c('1fr', '1fr', '1fr', '1fr')
#           )
#         ),
#         first = tagList(
#           segment(
#             class = 'basic',
#             shiny::selectInput(
#               inputId = ns('sector'),
#               label = 'Sektor',
#               choices = sector_model1$k_sector,
#               multiple = FALSE,
#               selected = '',
#               width = '100%'
#             ),
#             
#             shiny::selectInput(
#               inputId = ns('subsector'),
#               label = 'Outcomes',
#               multiple = FALSE,
#               choices = NULL,
#               selected = '',
#               width = '100%'
#               
#             ),
#             
#             shiny::selectInput(
#               inputId = ns('type'),
#               label = 'Patienttype',
#               choices = c(
#                 'Incident' = 'incident',
#                 'Prævalent'= 'prævalent'),
#               multiple = FALSE,
#               selected = 'Incident',
#               width = '100%'
#             )
#           )
#           
#         ),
#         
#         
#         
#         second = tagList(
#           segment(
#             class = 'basic',
#             selectors(
#               postfix = 'treatment',
#               ns = ns)
#           )
#           
#         ),
#         third = tagList(
#           segment(
#             class = 'basic',
#             selectors(
#               postfix = 'control',
#               ns = ns,
#               add_population = TRUE
#               )
#           )
#           
#         ),
#         fourth =tagList(
#           segment(
#             class = 'basic',
            # lapply(
            #   1:5,
            #   function(x) {
            #     shiny::sliderInput(
            #       inputId = ns(paste0('effect_', x)),
            #       label = NULL,
            #       width = '100%',
            #       value = 0,
            #       min = 0,
            #       max = 100
            #     )
#                 
#                 
#                 
#               }
#             )
#           )
#           
#         )
#       )
#     ),
#     
#     segment(
#       class = 'attached',
#       steps(
#         'steps',
#         class = 'attached two',
#         steps_list = list(
#           single_step(
#             step_class = 'active',
#             id = shiny.semantic:::generate_random_id(prefix = 'step'),
#             title = textOutput(
#               ns("treatment_size"),
#               inline = TRUE
#             ),
#             description = 'Personer i den valgte sygdomsgruppe',
#             icon_class = 'users'
#           ),
#           single_step(
#             step_class = 'active',
#             id = shiny.semantic:::generate_random_id(prefix = 'step'),
#             title = textOutput(ns("control_size"), inline = TRUE),
#             description = 'Personer i den valgte sammenligningsgruppe',
#             icon_class = 'users'
#           )
#           
#         )
#         
#       )
#     )
#     
#       
#       
#   )
#   
#   
  # output_ui <- segment(
  #   class = 'attached',
  #   style = 'height: 100%',
  #   tabset(
  #     tabs = list(
  #       list(
  #         menu = span(tags$i(class = 'ambulance icon'), 'Forbrug'),
  #         content = plotlyOutput(
  #             inline = FALSE,
  #             outputId = ns('qty_plot'),
  #             height = '100%'
  #           )
  # 
  #       ),
  #       list(
  #         menu = span(tags$i(class="money bill alternate icon"), 'Omkostninger'),
  #         content = plotlyOutput(
  #             inline = FALSE,
  #             outputId = ns('cost_plot'),
  #             height = '50%'
  #           )
  # 
  #       )
  #     )
  #   )
  # )
#   
#     
#     
#   return(
#     list(
#       output_ui    = output_ui,
#       parameter_ui = parameter_ui
#     )
#     
#   )
# }
# 
# 
# 
# 
# # UI wrappper
# #
# # this function collects
# # all UI elements in a single wrapper
# model1UI <- function(
#     input,
#     output,
#     id
# ) {
#   
#   .ui_model1_body(input, output, id)
#   
# }



# script end;