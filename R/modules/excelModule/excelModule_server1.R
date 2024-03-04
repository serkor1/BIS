# script: ex
# date: 2024-03-01
# author: Serkan Korkmaz, serkor1@duck.com
# objective: 
# script start;


excel_server <- function(
    input,
    output,
    session) {
  
  # 1) Generate recipe 
  # object based on user-input
  get_recipe <- shiny::reactive(
    {
      recipe(
        treatment = list(
          k_disease       = input$disease_treatment,
          c_gender        = input$char_gender_treatment,
          c_education     = input$char_education_treatment,
          c_socioeconomic = input$char_socioeconomic_treatment,
          c_age           = input$char_age_treatment
        ),
        
        control = list(
          k_disease       = input$disease_control,
          c_gender        = input$char_gender_control,
          c_education     = input$char_education_control,
          c_socioeconomic = input$char_socioeconomic_control,
          c_age           = input$char_age_control
        )
      )
    }
  )
  
  # 2) Extract data from
  # SQL database 
  DT <- shiny::reactive(
    {
      
      
      extract_data(
        DB_connection = DB_connection,
        table         = "model1",
        k_disease     = c(input$disease_treatment, input$disease_control)
      )
    }
  )
  
  
  DT_general <- reactive(
    {
      
      
      extract_data(
        DB_connection = DB_connection,
        table         = "general_population",
        k_disease     = input$disease_treatment,
        k_sector      = input$sector,
        k_allocator   = input$subsector
      )
      
      # 
      # # 1) generate query string;
      # query <- paste(
      #   'SELECT * FROM general_population WHERE', paste(
      #     c(
      #       paste0('k_disease = "',  input$disease_treatment, '"'),
      #       paste0('k_sector = "', input$sector, '"'),
      #       paste0('k_allocator = "', input$subsector, '"')
      #     ),
      #     
      #     collapse = ' AND '
      #   )
      # )
      # 
      # # 1.1) verbose the the query;
      # # TODO: might be that this should
      # # be moved toi the logger.
      # cli::cli_inform(
      #   message = c(
      #     '!' = 'Query',
      #     'i' = query
      #   )
      # )
      # 
      # # 2) send the query
      # # to the db
      # get_results <- dbSendQuery(
      #   conn = DB_connection,
      #   statement = query
      # )
      # 
      # # 3) Get the results
      # # and store as data.table
      # DT <- as.data.table(
      #   dbFetch(
      #     res = get_results,
      #     n = -1
      #   )
      # )
      # 
      # 
      # DT[
      #   ,
      #   k_disease := gsub(
      #     pattern = input$disease_treatment,
      #     replacement = 'general',
      #     x = k_disease
      #   )
      #   ,
      # ]
      # 
      # # 4) clear results;
      # dbClearResult(get_results)
      # 
      # return(
      #   DT
      # )
      
      
      
      
    }
  )
  
  
  
  
  prepared_data <- shiny::reactive(
    {
      
      
      
      if (grepl(x = input$disease_control, pattern = 'general')) {
        
        
        DT <- rbind(
          DT(),
          DT_general()
        )
        
        
      } else {
        
        
        DT <- DT()
        
        
      }
      
      
      
      prepare_data(
        DT = DT,
        recipe = get_recipe()
      )
    }
  )
  
  cooked_data <-shiny::reactive(
    {
      
      
      
      aggregate_data(
        DT = prepared_data(),
        calc = expression(
          .(
            v_qty = sum(
              v_qty * v_weights, na.rm = TRUE
            )/sum(v_weights, na.rm = TRUE),
            
            v_cost = sum(
              v_cost * v_weights, na.rm = TRUE
            )/sum(v_weights, na.rm = TRUE)
          )
        ),
        by = c(
          "k_year",
          "k_sector",
          "k_disease",
          "k_assignment",
          "k_allocator",
          "c_type"
        )
      )
    }
  )
  
  flavored_data <- shiny::reactive(
    {
      
      effect_data(
        DT = cooked_data(),
        effect = data.table::data.table(
          effect = c(
            input$effect_1/100,
            input$effect_2/100,
            input$effect_3/100,
            input$effect_4/100,
            input$effect_5/100
          ),
          k_year = -2:5
        )
      )
    }
  )
  
  
  
  
  
  final_data <- shiny::reactive(
    {
      
      flavored_data()[
        k_assignment %chin% c('control', 'treatment', 'counter_factual')
      ][
        ,
        k_assignment := fcase(
          default = 'Sygdomsgruppe',
          k_assignment %chin% 'control', 'Sammenligningsgruppe',
          k_assignment %chin% 'counter_factual', 'Kontrafaktisk sygdomsgruppe'
        )
        ,
      ]
      
    }
  )
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('workbook.xlsx', sep="")
    },
    content = function(file) {
      
      # 1) start download indicator
      # after user clicks downlaod
      showNotification(
        ui = shiny::span(bsicons::bs_icon("download"), "Downloader..."),
        action = NULL,
        duration = NULL,
        closeButton = FALSE,
        id = "download_indicator",
        type = c("default"),
        session = getDefaultReactiveDomain()
      )
      
      
      # 2) store data
      # in DT
      DT_ <- final_data()
      
      # 3) add danish
      # labels
      DT_[
        ,
        k_year := paste('Tid', k_year)
        ,
      ]
      
      # 4) prepare workbook 
      # data
      workbook_data <- workbookR::as_workbook_data(
        DT = DT_,
        by = list(
          row = 'k_allocator',
          column  = c(
            'k_year',
            'k_assignment'
          )
        ),
        
        structure = list(
          sheet = 'c_type',
          row    = 'k_sector'
        )
        
      )
      
      # 5) generate workbook 
      # data send to user
      generate_workbook(
        file = file,
        list = workbook_data,
        theme = list(
          template = 'default',
          color = input$placeholder_color
        )
      )
      
      
      # 6) Close notification
      removeNotification("download_indicator", session = getDefaultReactiveDomain())
      
      
      
      
    }
  )
  
}



# script end;



