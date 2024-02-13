# script: scr_server
# date: 2024-02-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: The server-side
# of the model.
# script start;

server <- function(
        input,
        output,
        session
){
    
    shiny::observeEvent(
        eventExpr = input$export,
        {
            # 1) ShowModal
            shiny::showModal(

                # 2) Contents of the Modal
                shiny::modalDialog(
                    title = "Getrekt",
                    size = "xl",fade = TRUE,
                    easyClose = FALSE,
                    export_module_ui(),
                    downloadButton(
                        "downloadData",
                        "Eksporter"
                    )

                )
            )
            
            # 3) export server
            excel_server(
                input = input,
                output = output,
                session = session
            )
        }
    )
    
    
    
    
    # observe the chosen
    # sector and update 
    # the available values 
    # accordingly
    shiny::observe(
        {
            
            # Extract chosen sector
            # values;
            value <- input$sector
            
            shinyWidgets::updatePickerInput(
                session = session,
                inputId = "subsector",
                choices = sector[
                    k_sector %chin% value
                ]$k_allocator
            )
        }
    )
    
    # Extract base data
    DT <- shiny::reactive(
        {
            
            # 1) generate query string;
            query <- paste(
                'SELECT * FROM model1 WHERE', paste(
                    c(
                        paste0('c_type = "', input$type, '"'),
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
            get_results <- DBI::dbSendQuery(
                conn = DB_connection,
                statement = query
            )
            
            # 3) Get the results
            # and store as data.table
            DT <- data.table::as.data.table(
                DBI::dbFetch(
                    res = get_results,
                    n = -1
                )
            )
            
            # 4) clear results;
            DBI::dbClearResult(get_results)
            
            return(
                DT
            )
        }
    )
    
    
    recipe_object <- shiny::reactive(
        {
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
    
    
    
    DT_general <- shiny::reactive(
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
            get_results <- DBI::dbSendQuery(
                conn = DB_connection,
                statement = query
            )
            
            # 3) Get the results
            # and store as data.table
            DT <- data.table::as.data.table(
                DBI::dbFetch(
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
            DBI::dbClearResult(get_results)
            
            return(
                DT
            )
            
            
            
            
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
            
            
            
            prepare(
                DT = DT,
                recipe = recipe_object()
            )
        }
    )
    
    cooked_data <- shiny::reactive(
        {
            
            
            
            cook(
                DT = prepared_data()
            )
        }
    )
    
    flavored_data <- shiny::reactive(
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
    
    final_data <- shiny::reactive(
        {
            flavored_data()
            
        }
    )
    
    
    plotting_data <- shiny::reactive(
        {
            
            # 1) extract the relevant
            # measures
            DT_ <-  final_data()[
                k_assignment %chin% c('control', 'treatment', 'counter_factual')
            ]
            
            # # 2) rename variable
            DT_[
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
    
    
    
    
    output$cost_plot <-  plotly::renderPlotly(
        {
            
            plotly::layout(
                
                # cost-plot:
                p = plotly::plot_ly(
                    plotting_data(),
                    x = ~k_year,
                    y = ~v_cost,
                    color = ~k_assignment,
                    colors = plot_color,
                    type = 'scatter',
                    # fill = 'tozeroy',
                    mode = 'lines+markers',
                    line = list(shape = 'spline', smoothing = 1.3)
                ),
                
                # layout-elements
                title = 'Omkostninger',
                legend = list(orientation = 'h'),
                xaxis = list(
                    range=c(-2,5),
                    title = 'Tid'
                ),
                yaxis = list(
                    title = units[k_allocator %chin% input$subsector]$c_unit
                )
                
            )
            

        }
    )
    
    output$qty_plot <-  plotly::renderPlotly(
        {
            
            plotly::layout(
                # Quantity plot:
                p = plotly::plot_ly(
                    plotting_data(),
                    x = ~k_year,
                    y = ~v_qty,
                    # fill = 'tozeroy',
                    color = ~k_assignment,
                    colors = plot_color,
                    type = 'scatter',
                    mode = 'lines+markers',
                    line = list(shape = 'spline', smoothing = 1.3)
                    
                ),
                
                # Layout Elements
                title = 'Forbrug',
                legend = list(orientation = 'h'),
                xaxis = list(
                    range=c(-2,5),
                    title = 'Tid'
                ),
                yaxis = list(
                    title = units[k_allocator %chin% input$subsector]$c_unit
                )
            )
            
            
            
            
            
        }
    )
    
    
    
    
    
    
    
    
    
}

# script end;


