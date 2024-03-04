# script: server_model1
# date: 2023-06-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create a seperated server
# for model 1
# script start;

# server; #####
server_model1 <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      
      
      observe(
        {
          
          # Extract chosen sector
          # values;
          value <- input$sector
          
          # Update the available
          # values
          # shiny::updateSelectInput(
          #   session = session,
          #   inputId = 'subsector',
            # choices = sector_model1[
            #   k_sector %chin% value
            # ]$k_allocator
          # )
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "subsector",
            choices = sector_model1[
              k_sector %chin% value
            ]$k_allocator
          )
        }
      )


      # DT <- reactive(
      #   {
      # 
      #     # 1) generate query string;
      #     query <- paste(
      #       'SELECT * FROM model1 WHERE', paste(
      #         c(
      #           paste0('c_type = "', input$type, '"'),
      #           paste0('k_sector = "', input$sector, '"'),
      #           paste0('k_allocator = "', input$subsector, '"')
      #         ),
      # 
      #         collapse = ' AND '
      #       )
      #     )
      # 
      #     # 1.1) verbose the the query;
      #     # TODO: might be that this should
      #     # be moved toi the logger.
      #     cli::cli_inform(
      #       message = c(
      #         '!' = 'Query',
      #         'i' = query
      #       )
      #     )
      # 
      #     # 2) send the query
      #      # to the db
      #     get_results <- dbSendQuery(
      #       conn = DB_connection,
      #       statement = query
      #     )
      # 
      #     # 3) Get the results
      #     # and store as data.table
      #     DT <- as.data.table(
      #       dbFetch(
      #         res = get_results,
      #         n = -1
      #       )
      #     )
      # 
      #     # 4) clear results;
      #     dbClearResult(get_results)
      # 
      #     return(
      #       DT
      #     )
      #   }
      # )
      # 
      # 
      # 
      # 
      # 
      # 
      # # 1) Show graphs by default.
      # output$show_graph <- reactive(
      #   {
      #     TRUE
      #   }
      # )
      # 
      # observeEvent(
      #   input$table_ui,
      #   {
      #     output$show_graph <- reactive(
      #       {
      #         FALSE
      #       }
      #     )
      #   }
      # )
      # 
      # observeEvent(
      #   input$graph_ui,
      #   {
      #     output$show_graph <- reactive(
      #       {
      #         TRUE
      #       }
      #     )
      #   }
      # )
      # 
      # outputOptions(
      #   x = output,
      #   name = "show_graph",
      #   suspendWhenHidden = FALSE
      # )
      # 
      # 
      # 
      # 
      # 
      # 
      # 
      # # create recipe object;
      # recipe_object <- reactive(
      #   {
      #     .recipe_object(
      #       disease_treatment = input$disease_treatment,
      #       disease_control = input$disease_control,
      #       char_treatment = list(
      #         c_gender = input$char_gender_treatment,
      #         c_education = input$char_education_treatment,
      #         c_socioeconomic = input$char_socioeconomic_treatment,
      #         c_age           = input$char_age_treatment
      #       ),
      #       char_control = list(
      #         c_gender = input$char_gender_control,
      #         c_education = input$char_education_control,
      #         c_socioeconomic = input$char_socioeconomic_control,
      #         c_age           = input$char_age_control
      #       )
      #     )
      #   }
      # )
      # 
      # 
      # 
      # DT_general <- reactive(
      #   {
      # 
      #     # 1) generate query string;
      #     query <- paste(
      #       'SELECT * FROM general_population WHERE', paste(
      #         c(
      #           paste0('k_disease = "',  input$disease_treatment, '"'),
      #           paste0('k_sector = "', input$sector, '"'),
      #           paste0('k_allocator = "', input$subsector, '"')
      #         ),
      # 
      #         collapse = ' AND '
      #       )
      #     )
      # 
      #     # 1.1) verbose the the query;
      #     # TODO: might be that this should
      #     # be moved toi the logger.
      #     cli::cli_inform(
      #       message = c(
      #         '!' = 'Query',
      #         'i' = query
      #       )
      #     )
      # 
      #     # 2) send the query
      #     # to the db
      #     get_results <- dbSendQuery(
      #       conn = DB_connection,
      #       statement = query
      #     )
      # 
      #     # 3) Get the results
      #     # and store as data.table
      #     DT <- as.data.table(
      #       dbFetch(
      #         res = get_results,
      #         n = -1
      #       )
      #     )
      # 
      # 
      #     DT[
      #       ,
      #       k_disease := gsub(
      #         pattern = input$disease_treatment,
      #         replacement = 'general',
      #         x = k_disease
      #       )
      #       ,
      #     ]
      # 
      #     # 4) clear results;
      #     dbClearResult(get_results)
      # 
      #     return(
      #       DT
      #     )
      # 
      # 
      # 
      # 
      #   }
      # )
      # 
      # 
      # 
      # 
      # 
      # 
      # prepared_data <- reactive(
      #   {
      # 
      #     if (grepl(x = input$disease_control, pattern = 'general')) {
      # 
      # 
      #       DT <- rbind(
      #         DT(),
      #         DT_general()
      #       )
      # 
      # 
      #     } else {
      # 
      # 
      #       DT <- DT()
      # 
      # 
      #     }
      # 
      # 
      # 
      #     prepare(
      #       DT = DT,
      #       recipe = recipe_object()
      #     )
      #   }
      # )
      # 
      # cooked_data <- reactive(
      #   {
      # 
      # 
      # 
      #     cook(
      #       DT = prepared_data()
      #     )
      #   }
      # )
      # 
      # flavored_data <- reactive(
      #   {
      # 
      # 
      # 
      #     flavor(
      #       DT = cooked_data(),
      #       effect = c(
      #         input$effect_1/100,
      #         input$effect_2/100,
      #         input$effect_3/100,
      #         input$effect_4/100,
      #         input$effect_5/100
      #       )
      #     )
      #   }
      # )
      # 
      # final_data <- reactive(
      #   {
      #     flavored_data()
      # 
      #   }
      # )
      # 
      # 
      # output$control_size <- renderText(
      #   {
      #     paste(
      #       formatC(
      #         x = cooked_data()$DT_meta[k_assignment %chin% 'control']$v_obs,
      #         digits = 0,
      #         format = 'd',
      #         big.mark = '.',
      #         decimal.mark = ',')
      #     )
      #   }
      # )
      # 
      # output$treatment_size <- renderText(
      #   {
      #     paste(
      #       formatC(
      #         x = cooked_data()$DT_meta[k_assignment %chin% 'treatment']$v_obs,
      #         digits = 0,
      #         format = 'd',
      #         big.mark = '.',
      #         decimal.mark = ',')
      #     )
      #   }
      # )
      # 
      # 
      # 
      # 
      # 
      # 
      # 
      # # prepare data for plotting;
      # plotting_data <- reactive(
      #   {
      # 
      #     # 1) extract the relevant
      #     # measures
      #     DT_ <-  final_data()[
      #       k_assignment %chin% c('control', 'treatment', 'counter_factual')
      #     ]
      # 
      #     # # 2) rename variable
      #     DT_[
      #       ,
      #       k_assignment := fcase(
      #         default = 'Sygdomsgruppe',
      #         k_assignment %chin% 'control', 'Sammenligningsgruppe',
      #         k_assignment %chin% 'counter_factual', 'Kontrafaktisk sygdomsgruppe'
      #       )
      #       ,
      #     ]
      # 
      #   }
      # )
      # 
      # 
      # 
      # 
      # output$cost_plot <- renderPlotly(
      #   {
      #     plot_ly(
      #       plotting_data(),
      #       x = ~k_year,
      #       y = ~v_cost,
      #       color = ~k_assignment,
      #       colors = plot_color,
      #       type = 'scatter',
      #       # fill = 'tozeroy',
      #       mode = 'lines+markers',
      #       line = list(shape = 'spline', smoothing = 1.3)
      #     ) %>% layout(
      #       title = 'Omkostninger',
      #       legend = list(orientation = 'h'),
      #       xaxis = list(
      #         range=c(-2,5),
      #         title = 'Tid'
      #       ),
      #       yaxis = list(
      #         title = units[k_allocator %chin% input$subsector]$c_unit
      #       )
      # 
      #       )
      # 
      # 
      # 
      #   }
      # )
      # 
      # output$qty_plot <- renderPlotly(
      #   {
      # 
      #     plot_ly(
      #       plotting_data(),
      #       x = ~k_year,
      #       y = ~v_qty,
      #       # fill = 'tozeroy',
      #       color = ~k_assignment,
      #       colors = plot_color,
      #       type = 'scatter',
      #       mode = 'lines+markers',
      #       line = list(shape = 'spline', smoothing = 1.3)
      # 
      #     ) %>% layout(
      #       title = 'Forbrug',
      #       legend = list(orientation = 'h'),
      #       xaxis = list(
      #         range=c(-2,5),
      #         title = 'Tid'
      #       ),
      #       yaxis = list(
      #         title = units[k_allocator %chin% input$subsector]$c_unit
      #       )
      #       )
      # 
      # 
      # 
      # 
      #   }
      # )





    }
  )
}

# script end;