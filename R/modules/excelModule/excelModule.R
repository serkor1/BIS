# script: excelModule
# author: Serkan Korkmaz
# date: 2023-08-17
# objective: Create UI for the excelModal
# which is located in the UI.
# needs to migrate existing choices.
# script start; ####



export_module_ui <- function() {

  
  # 2) Allocators
  shiny::tagList(
    
    shiny::markdown(
      "
      
      Dine valgte `sygdomsgrupper` vil blive eksporteret til `Excel` sammen med
      de ekstra valg du foretager dig her.
      
      > **Bemærk:** Downloadtiden kan være lang hvis du vælger mange parametre.
      
      "
    ),
    
    
    shinyWidgets::pickerInput(
      inputId = 'placeholder_sector',
      label = 'Outcome',
      multiple = TRUE,
      width = '100%',
      choices = allocator,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
      
    ),
    
    shinyWidgets::pickerInput(
      inputId = 'placeholder_type',
      label = 'Patienttype',
      multiple = TRUE,
      width = '100%',
      choices = c(
        'Incident' = 'incident',
        'Prævalent'= 'prevalent'),
      options = list(
        `actions-box` = TRUE
      )
    ),
    
    shinyWidgets::pickerInput(
      inputId = 'placeholder_color',
      label = 'Farvetema',
      width = '100%',
      multiple = FALSE,
      choices = c(
        'Rød' = 'Reds',
        'Grå' = 'Greys',
        'Blå' = 'Blues'
      ),
      options = list(
        `actions-box` = TRUE
      )
    )
    
    
    
  )
  
  
  
  
}




# # TODO: This modal has to
# # be moved to the UI side. There is no other choice.
# 
# 
# .model1_excel_ui <- function(
#     id,
#     input,
#     output
# ) {
#   
#   
#   # 1) generate namespace
#   ns <- NS(id)
#   
#   # 2) Generate UI
#   list(
#     segment(
#       class = paste('basic'),
#       
#       shiny::selectInput(
        # inputId = ns('placeholder_sector'),
        # label = 'Outcome',
        # multiple = TRUE,
        # width = '100%',
        # choices = allocator_model1
#       ),
#       
#       
#       
#       
#       
#       animated_button(
#         inputid = ns('select_all'),
#         label = 'Vælg alle',
#         hidden_content = tags$i(class = 'plus icon')
#       ),
#       
#       animated_button(
#         inputid = ns('deselect_all'),
#         label = 'Fravælg alle',
#         hidden_content = tags$i(class = 'minus icon')
#       ),
#       
#       
#       
      # shiny::selectInput(
      #   inputId = ns('placeholder_type'),
      #   label = 'Patienttype',
      #   multiple = TRUE,
      #   width = '100%',
      #   choices = c(
      #     'Incident' = 'incident',
      #     'Prævalent'= 'prevalent')
      # ),
      # 
      # shiny::selectInput(
      #   inputId = ns('placeholder_color'),
      #   label = 'Farvetema',
      #   width = '100%',
      #   multiple = FALSE,
      #   choices = c(
      #     'Rød' = 'Reds',
      #     'Grå' = 'Greys',
      #     'Blå' = 'Blues'
      #   )
      # )
#     )
#   )
#   
#   
#   
#   
# }
# 
# 
# .model2_excel_ui <- function(
#     id,
#     input,
#     output
# ) {
#   
#   
#   # 1) generate namespace
#   ns <- NS(id)
#   
#   # 2) Generate UI
#   segment(
#     class = paste('basic'),
#     
#     shiny.semantic::selectInput(
#       inputId = ns('placeholder_sector'),
#       label = 'Outcome',
#       multiple = TRUE,
#       choices = c('allocator 1', 'allocator 2')
#     ),
#     
#     
#     shiny.semantic::selectInput(
#       inputId = ns('placeholder_type'),
#       label = 'Patienttype',
#       multiple = TRUE,
#       choices = c(
#         'Incident' = 'incident',
#         'Prævalent'= 'prevalent')
#     ),
#     
#     shiny.semantic::selectInput(
#       inputId = ns('placeholder_color'),
#       label = 'Farvetema',
#       multiple = FALSE,
#       choices = c(
#         'Rød' = 'Reds',
#         'Grå' = 'Greys',
#         'Blå' = 'Blues'
#       )
#     )
#   )
#   
#   
# }

# end of script; ####