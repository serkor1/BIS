excel_server <- function(
    input,
    output,
    session) {
  
  
  
  
  # prepare data; ####
  # 
  # 
  # 1) Load the data;
  # by disease
  DT <- reactive(
    {
      
      # 1) generate query string;
      # based on chosen diseases
      query <- paste(
        'SELECT * FROM model1 WHERE', paste(
          c(
            paste0(
              'k_disease IN (',
              paste0("'",c(input$disease_treatment, input$disease_control),"'", collapse = ','),
              ')'
            )
          ),
          
          collapse = ' AND '
        )
      )
      
      # 1.1) verbose the the query;
      # TODO: might be that this should
      # be moved toi the logger.
      cli::cli_inform(
        message = c(
          '!' = 'Query',
          'i' = query
        )
      )
      
      # 2) send the query
      # to the db
      get_results <- dbSendQuery(
        conn = DB_connection,
        statement = query
      )
      
      # 3) Get the results
      # and store as data.table
      DT <- as.data.table(
        dbFetch(
          res = get_results,
          n = -1
        )
      )
      
      # 4) clear results;
      dbClearResult(get_results)
      
      return(
        DT
      )
    }
  )
  
  observe(
    {
      message(head(DT()))
    }
  )
  
  # 2) create recipe
  # object
  recipe_object <- reactive(
    {
      # NOTE: we probably
      # need an additional argument
      # here for the sectors
      .recipe_object(
        disease_treatment = input$disease_treatment,
        disease_control = input$disease_control,
        char_treatment = list(
          c_gender = input$char_gender_treatment,
          c_education = input$char_education_treatment,
          c_socioeconomic = input$char_socioeconomic_treatment,
          c_age           = input$char_age_treatment
        ),
        char_control = list(
          c_gender = input$char_gender_control,
          c_education = input$char_education_control,
          c_socioeconomic = input$char_socioeconomic_control,
          c_age           = input$char_age_control
        )
      )
    }
  )


  DT_general <- reactive(
    {

      # 1) generate query string;
      query <- paste(
        'SELECT * FROM general_population WHERE', paste(
          c(
            paste0('k_disease = "',  input$disease_treatment, '"'),
            paste0('k_sector = "', input$sector, '"'),
            paste0('k_allocator = "', input$subsector, '"')
          ),

          collapse = ' AND '
        )
      )

      # 1.1) verbose the the query;
      # TODO: might be that this should
      # be moved toi the logger.
      cli::cli_inform(
        message = c(
          '!' = 'Query',
          'i' = query
        )
      )

      # 2) send the query
      # to the db
      get_results <- dbSendQuery(
        conn = DB_connection,
        statement = query
      )

      # 3) Get the results
      # and store as data.table
      DT <- as.data.table(
        dbFetch(
          res = get_results,
          n = -1
        )
      )


      DT[
        ,
        k_disease := gsub(
          pattern = input$disease_treatment,
          replacement = 'general',
          x = k_disease
        )
        ,
      ]

      # 4) clear results;
      dbClearResult(get_results)

      return(
        DT
      )




    }
  )








  prepared_data <- reactive(
    {

      if (grepl(x = input$disease_control, pattern = 'general')) {



        DT <- rbind(
          DT(),
          DT_general()
        )


      } else {


        DT <- DT()


      }



      prepare(
        DT = DT,
        recipe = recipe_object()
      )
    }
  )




  cooked_data <- reactive(
    {



      cook(
        DT = prepared_data()
      )
    }
  )




  flavored_data <- reactive(
    {



      flavor(
        DT = cooked_data(),
        effect = c(
          input$effect_1/100,
          input$effect_2/100,
          input$effect_3/100,
          input$effect_4/100,
          input$effect_5/100
        )
      )
    }
  )

  final_data <- reactive(
    {
      flavored_data()

    }
  )






  output$downloadData <- downloadHandler(
    filename = function() {
      paste('workbook.xlsx', sep="")
    },
    content = function(file) {



      DT_ <- final_data()


      DT_[
        ,
        k_year := paste('Tid', k_year)
        ,
      ]


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

      generate_workbook(
        file = file,
        list = workbook_data,
        theme = list(
          template = 'default',
          color = input$placeholder_color
        )
      )




    }
  )
  
  
  
  
  
  
  
  
}
