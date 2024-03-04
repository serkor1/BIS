# script: scr_selectize
# date: 2023-06-15
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Some of the parameter search 
# are common across model1, and are therefore
# programmemd seperately
# script start;

selectors <- function(
    group){
  
  if (group == "control"){
    
    diseases <- c(
      diseases,
      list(
        Matching = list(
          "Befolkningen" = "general"
        )
      )
    )
    
  }
  
  shiny::tagList(
    # 1) The first element
    # is the diseases
    shinyWidgets::pickerInput(
      inputId  = paste0("disease_", group),
      label    = "Sygdomme",
      multiple = FALSE,
      choices = diseases,
      selected = sample(
        diseases,
        1
      ),
        options = list(
          `live-search` = TRUE
          )
      
    ),
    
    shinyWidgets::pickerInput(
      inputId  = paste0("char_gender_", group),
      label    = "Køn",
      multiple = TRUE,
      choices = gender,
      selected = sample(
        gender,
        1
      ),
      options = list(
        `live-search` = TRUE
      )
      
    ),
    
    shinyWidgets::pickerInput(
      inputId  = paste0("char_age_", group),
      label    = "Alder",
      multiple = TRUE,
      choices = age,
      selected = sample(
        age,
        1
      ),
      options = list(
        `live-search` = TRUE
      )
      
    ),
    
    shinyWidgets::pickerInput(
      inputId  = paste0("char_education_", group),
      label    = "Uddannelse",
      multiple = TRUE,
      choices = education,
      selected = sample(
        education,
        1
      ),
      options = list(
        `live-search` = TRUE
      )
      
    ),
    
    shinyWidgets::pickerInput(
      inputId  = paste0("char_socioeconomic_", group),
      label    = "Arbejdsmarkedsstatus",
      multiple = TRUE,
      choices = socioeconomics,
      selected = sample(
        socioeconomics,
        1
      ),
      options = list(
        `live-search` = TRUE
      )
      
    ),
    
    
  )
  
}








# selectors <- function(
#     postfix = NULL,
#     ns = ns,
#     add_population = FALSE
# ) {
#   
#   #ns <- NS(id)
#   
#   width = '100%'
# 
#   if (add_population) {
#     
#     diseases_model1 <- c(diseases_model1, list( 
#       Matching = list(
#         'Befolkningen' = 'general'
#         )
#       )
#     )
#     
#   }
#   
#   tagList(
#     
#     shiny::selectInput(
#       inputId = ns(paste0('disease_', postfix)),
#       label = 'Sygdomme',
#       choices = diseases_model1,
#       multiple = FALSE,
#       selected = sample(
#         diseases_model1,
#         1),
#       width = width
#     ),
#     
#     shiny::selectInput(
#       inputId = ns(paste0('char_gender_', postfix)),
#       label = 'Køn',
#       choices = gender_model1,
#       selected = sample(gender_model1, size = sample(1:length(gender_model1),1)),
#       multiple = TRUE,
#       width = width
#     ),
#   
#     
#     shiny::selectInput(
#       inputId = ns(paste0('char_age_', postfix)),
#       label = 'Aldersgruppe',
#       choices = age_model1,
#       selected = sample(age_model1, size = sample(1:length(age_model1), 1)),
#       multiple = TRUE,
#       width = width
#     ),
#     
#     shiny::selectInput(
#       inputId = ns(paste0('char_education_', postfix)),
#       label = 'Uddannelse',
#       choices = education_model1,
#       selected = sample(education_model1, size = sample(1:length(education_model1), 1)),
#       multiple = TRUE,
#       width = width
#     ),
#     
#     shiny::selectInput(
#       inputId = ns(paste0('char_socioeconomic_', postfix)),
#       label = 'Arbejdsmarkedstilknytning',
#       choices = socioeconomics_model1,
#       selected = sample(socioeconomics_model1, size = sample(1:length(socioeconomics_model1), 1)),
#       multiple = TRUE,
#       width = width
#     )
#     
#     
#     
#   )
#   
#   
# }



# script end;