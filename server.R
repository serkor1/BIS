# script: scr_server
# date: 2024-02-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: The server-side
# of the model.
# script start;

server <- function(
        input,
        output,
        session){
    
    { 
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
                    c_type        = input$type,
                    k_sector      = input$sector,
                    k_allocator   = input$subsector
                )
            }
        )
        
        # 3) Extract general
        # population
        # 
        # NOTE: At this stage I have no clue
        # what this does
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
    }
    
    
    {
        
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
        
        reactivePlotTheme <- reactive({
            if(input$app_theme == "light") {
                lightModeTheme()
            } else {
                darkModeTheme()
            }
        })
        
    }
    
    
    shiny::observeEvent(
        eventExpr = input$export,
        {
            # 1) ShowModal
            shiny::showModal(
                
                # 2) Contents of the Modal
                shiny::modalDialog(
                    title = shiny::span(bsicons::bs_icon("table"), "Eksportér til Excel"),
                    size = "m",
                    fade = TRUE,
                    easyClose = FALSE,
                    footer = div(
                        modalButton('Luk',icon = shiny::icon("close")),
                        downloadButton(
                            "downloadData",
                            "Download"
                        )
                        
                    ),
                    export_module_ui()
                    
                        
                       
                    
                    
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
    
    
    
    
    
    output$baselineTable <- DT::renderDT({
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
        DT::datatable(
            DT, 
            options = list(autoWidth = TRUE, paging = FALSE, fillContainer = TRUE, searching = FALSE, info = FALSE, ordering = FALSE), 
            class = 'cell-border stripe', 
            rownames = FALSE,width = "100%",
            selection = "none",
            caption = NULL
        )
    })
    
    
    
    output$cost_plot <-  plotly::renderPlotly(
        {
            
            
            
            theme <- reactivePlotTheme()
            
            plotly::layout(
                
                # cost-plot:
                p = plotly::plot_ly(
                    final_data(),
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
                plot_bgcolor = 'rgb(0,0,0,0)',
                paper_bgcolor ='rgb(0,0,0,0)', 
                font         = theme$font
                
                
            )
            
            
        }
    )
    
    output$qty_plot <-  plotly::renderPlotly(
        {
            
            
            theme <- reactivePlotTheme()
            
            plotly::layout(
                # Quantity plot:
                p = plotly::plot_ly(
                    final_data(),
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
                plot_bgcolor = 'rgb(0,0,0,0)',
                paper_bgcolor ='rgb(0,0,0,0)', 
                font         = theme$font
            )
            
            
            
            
            
        }
    )
    
}

# script end;


