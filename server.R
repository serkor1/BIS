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
    
    
    output$baselineTable <- renderDT({
        # Sample data frame
        # DT <- data.frame(
        #     `Variable Name` = c("Age", "Gender", "BMI", "Smoking Status"),
        #     `Group 1` = c("30 ± 5", "50% Male", "25 ± 3", "20% Smokers"),
        #     `Group 2` = c("31 ± 4", "45% Male", "26 ± 2", "18% Smokers"),
        #     check.names = FALSE
        # )
        
        DT <- dcast(
            data = extract_data(
                DB_connection = DB_connection,
                table         = "population_count",
                k_disease     = list(input$disease_treatment, input$disease_control),
                c_type        = list(input$type)
            ),
            formula = c_characteristic ~ k_disease,
            value.var = "v_obs",
            fun.aggregate = sum,na.rm = TRUE
        )
        
        # Render the data table with custom styling
        datatable(
            DT, 
            options = list(autoWidth = TRUE, paging = FALSE, fillContainer = TRUE), 
            class = 'cell-border stripe', 
            rownames = FALSE,
            selection = "none",
            caption = "Caption"
        )
    })
    
    reactivePlotTheme <- reactive({
        if(input$app_theme == "light") {
            lightModeTheme()
        } else {
            darkModeTheme()
        }
    })
    
    
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
            
            extract_data(
                DB_connection = DB_connection,
                table         = "model1",
                c_type        = input$type,
                k_sector      = input$sector,
                k_allocator   = input$subsector
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
            
            
            extract_data(
                DB_connection = DB_connection,
                table         = "general_population",
                k_disease     = input$disease_treatment,
                k_sector      = input$sector,
                k_allocator   = input$subsector
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
            
            
            
            theme <- reactivePlotTheme()
            
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
                
                # Layout Elements
                title = 'Omkostninger',
                legend = theme$legend,
                xaxis = theme$xaxis,
                yaxis = theme$yaxis,
                plot_bgcolor = 'rgb(0,0,0,0)',#reactivePlotTheme()$plot_bgcolor,
                paper_bgcolor ='rgb(0,0,0,0)', #reactivePlotTheme()$paper_bgcolor,
                font         = theme$font
                
                # ,
                # plot_bgcolor='rgba(0,0,0,0)', # Transparent plot background
                # paper_bgcolor='rgba(0,0,0,0)'
                
            )
            
            
        }
    )
    
    output$qty_plot <-  plotly::renderPlotly(
        {
            
            
            theme <- reactivePlotTheme()
            
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
                legend = theme$legend,
                xaxis = theme$xaxis,
                yaxis = theme$yaxis,
                plot_bgcolor = 'rgb(0,0,0,0)',#reactivePlotTheme()$plot_bgcolor,
                paper_bgcolor ='rgb(0,0,0,0)', #reactivePlotTheme()$paper_bgcolor,
                font         = theme$font
            )
            
            
            
            
            
        }
    )
    
    
    
    
    
    
    
    
    
}

# script end;


