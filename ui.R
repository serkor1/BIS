# script: User-interface
# date: 2024-02-13
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create a user interface
# script start;

bslib::page_navbar(
    shiny::tags$style(HTML("
    .btn-transparent {
      background-color: transparent !important;
      border-color: transparent !important;
    }
  ")),
    
    shiny::tags$script('
    document.addEventListener("DOMContentLoaded", function() {
      var button = document.getElementById("openLinkBtn");
      button.addEventListener("click", function() {
        window.open("https://github.com/serkor1/BIS", "_blank");
      });
    });
  '),
    
    title = "Beregner til Investeringer i Sundhed",
    id = "main_page",
    bslib::nav_spacer(),
  
  
  bslib::nav_item(
      shinyWidgets::actionBttn(
          inputId = "openLinkBtn",label = NULL,icon = bsicons::bs_icon("github"), style = "simple", color = "default",class = "btn-transparent")
      ),
    bslib::nav_item(bslib::input_dark_mode(mode = "light",id = "app_theme")),
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
    
    bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1,1),
        card(
            title = span(shiny::icon("cog"), "Ouput"),
            header = NULL,
            body = NULL,
            footer = NULL
        ),
        
        
        card(
            title = span(shiny::icon("cog"), "Ouput"),
            header = shiny::div(
                shinyWidgets::dropdownButton(
                
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
                label = "Vælg effekter",
                tooltip = tooltipOptions(title = "Klik for at vælge effektstørrelse!")
            )
                ),
            
            footer = "Footer",
            body = bslib::layout_columns(
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
            )
        )
    ),
    
    

    collapsible = FALSE
    
    
    
    
)


# script end;