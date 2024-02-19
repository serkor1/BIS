# script: User-interface
# date: 2024-02-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create a user interface
# script start;

bslib::page_navbar(
    title = "Beregner til Investeringer i Sundhed",
    id = "main_page",
    bslib::nav_item(bslib::input_dark_mode(mode = "light")),
    
    
    
    lang = "en",
    window_title = "BIS",
    fillable_mobile = TRUE,
    sidebar = bslib::sidebar(
    position = "left",
    width = "360px",
    open = "always",
    title = "Parametre",
    
    
    # Sidebar elements;
    
    # 1) Sector and patient choices
    # 
    # TODO: Wrap these in a function
    # at a later point
    shinyWidgets::pickerInput(
        inputId  = ('sector'),
        label    = 'Sektor',
        # TODO: This wasnt necessary before.
        # Something changed.
        choices  = unique(sector$k_sector),
        multiple = FALSE,
        options  = list(
            `live_search` = TRUE
        )
    ),
    
    shinyWidgets::pickerInput(
        inputId  = ('subsector'),
        label    = 'Outcomes',
        multiple = FALSE,
        choices  = NULL,
        selected = '',
        width    = '100%'
        
    ),
    
    shinyWidgets::pickerInput(
        inputId = ('type'),
        label = 'Patienttype',
        choices = c(
            'Incident' = 'incident',
            'Prævalent'= 'prævalent'),
        multiple = FALSE,
        selected = 'Incident',
        width = '100%'
    ),
    
    # 2) Selecting patient
    # groups:
    # 
    # Treatment vs Control
    shinyWidgets::radioGroupButtons(
        inputId = "pt_group",
        label   = "Gruppe",
        choiceNames = c(
            "Sygdomsgruppe",
            "Sammenligningsgruppe"
        ),
        choiceValues = c(
            "treatment",
            "control"
        ),
        justified = FALSE,
        width = "100%",size = "normal",direction = "vertical"
    ),
    
    shiny::conditionalPanel(
        condition = "input.pt_group == 'treatment'",{
            selectors(
                group = "treatment"
            )
            
        }
    ),
    
    shiny::conditionalPanel(
        condition = "input.pt_group == 'control'",{
            selectors(
                group = "control"
            )
        }
    ),
    
    shinyWidgets::actionBttn(
        inputId = "export",
        label = "Eksportér",
        style = "simple",
        block = TRUE,
        icon = shiny::icon("download"),
        size = "md",color = "primary"
    )


    ),
    theme = bslib::bs_theme(
        version =  5,
        preset = "flatly"
        ),
    
    # Model 1
    bslib::nav_panel(
        title = "Målgruppemodellen",
        icon = shiny::icon("github"),
        bslib::card(
            bslib::layout_columns(
                col_widths = c(3,3,3,3),
                
                bslib::value_box(
                    title = "Lægemiddelforbrug",
                    value = "Det offentliges betaling",
                    showcase = bsicons::bs_icon("bank2")
                    # ,
                    # theme = "primary"
                ),
                bslib::value_box(
                    title = "Patienttype",
                    value = "Incident",
                    showcase = bsicons::bs_icon("bank2"),
                    theme = "teal"
                ),
                bslib::value_box(
                    title = "Sygomdomsgruppe",
                    value = "N = 23.340",
                    showcase = bsicons::bs_icon("bank2"),
                    theme = "primary"
                ),
                bslib::value_box(
                    title = "Sammenligningsgruppe",
                    value = "N = 43.342",
                    showcase = bsicons::bs_icon("bank2"),
                    theme = "teal"
                )
            )
        ),
        
        
        bslib::card(
            bslib::card_header(
                shiny::span(
                    
                    dropdownButton(
                        
                        tags$h3("Vælg Effekter"),
                        
                        lapply(
                            1:5,
                            function(x) {
                                shiny::sliderInput(
                                    inputId = paste0('effect_', x),
                                    label = NULL,
                                    width = '100%',
                                    value = 0,
                                    min = 0,
                                    max = 100
                                )
                            }
                            
                        ),
                        
                        circle = FALSE,
                        size = "default",
                        #status = "info",
                        icon = icon("gear"), 
                        width = "300px",
                        label = "Effekter",
                        tooltip = tooltipOptions(title = "Klik for at vælge effektstørrelse!")
                    )
                    
                    
                    
                   
                )
                
                
            ),
            
            bslib::layout_columns(
                col_widths = c(6,6),
                plotly::plotlyOutput(
                    inline = FALSE,
                    outputId = 'qty_plot',
                    height = '100%'
                ),
                plotly::plotlyOutput(
                    inline = FALSE,
                    outputId = 'cost_plot',
                    height = '100%'
                )
            ),
            bslib::card_footer("footer"),

            full_screen = TRUE
        )
        
        
       
        
    ),
    collapsible = FALSE
    
    
    
    
)


# script end;