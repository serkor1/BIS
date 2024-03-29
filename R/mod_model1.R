#' model1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model1_ui <- function(id){
  ns <- NS(id)

  # main
  body <- tagList(
    uiOutput(ns("body"))
  )

  sidebar <- tagList(
    h1("sidebar")
  )


  return(
    list(
      body = body,
      sidebar = sidebar
    )

  )

}

#' model1 Server Functions
#'
#' @noRd
mod_model1_server <- function(id, theme){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Setup initial UI
    landing <-bslib::layout_columns(
      col_widths = c(4,4,4),row_heights = c(1,1),
      div(class="dim-overlay",
          div(class="card-container",
              div(
                class = "d-flex justify-content-center align-items-center",
                style = "height: 100%;",  # This ensures the div takes the full viewport height
                bslib::layout_columns(
                  options_card(
                    header = "Sygdomsgruppe",
                    footer = NULL,
                    picker_input(
                      inputid = ns("treatment_disease"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = model1_parameters$k_disease,
                      size = 10
                    )
                  ),
                  options_card(
                    header = "Sammenligningsgruppe",
                    footer = NULL,
                    picker_input(
                      inputid = ns("control_disease"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = model1_parameters$k_disease,
                      size = 10
                    )

                  ),
                  options_card(
                    header = "Sektor",
                    footer = NULL,
                    picker_input(
                      inputid = ns("k_sector"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = model1_parameters$k_sector,
                      size = 10
                    )

                  ),

                  clickable_card(
                    inputid = ns("start"),
                    outputval = "temp",
                    bslib::card_header("Start"),
                    bslib::card_body("Something")
                  )


                )
              )
          )

      )
    )



    output$body <- shiny::renderUI(
      landing
    )

    observeEvent(
      input$restart,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,

      output$body <- shiny::renderUI(
        landing
      )
    )

    observeEvent(
      input$start,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        output$body <- shiny::renderUI(
          bslib::layout_columns(
            height = "100%",
            col_widths = c(12),
            row_heights = c(1,1),
            bslib::layout_columns(
              col_widths = c(6,6),
              card(
                title = span(bsicons::bs_icon(name = "person"),"Demografi"),
                header = shiny::div(
                  div(style = "display: inline-block; vertical-align: top; margin-right: 1px;", # Add margin for some space between elements
                      shinyWidgets::switchInput(
                        label    = "Incidens",
                        inputId  = ns("c_type"),
                        width = "100%",
                        value = TRUE,
                        onLabel = bsicons::bs_icon("check",size = "2em")
                      )
                  ),
                  # shiny::tags$button(
                  #   id = ns("restart"),
                  #   bsicons::bs_icon("arrow-counterclockwise",size = "2em"),
                  #   style = "
                  #   background-color: transparent;
                  #   border: none;
                  #   padding: 0;
                  #   display: inline-block;
                  #   vertical-align: top;
                  #   height:1em;
                  #   width:1em;
                  #   vertical-align:-0.125em;
                  #   "
                  # )
                  # ,
                  # actionButton(
                  #   inputId = ns("restart"),
                  #   label = bsicons::bs_icon("arrow-counterclockwise",size = "2em"), # The icon
                  #   style = "
                  #   color: currentColor;
                  #   background-color: transparent;
                  #   border: none;
                  #   padding: 0;
                  #   display: inline-block;
                  #   vertical-align: top;
                  #   height:1em;
                  #   width:1em;
                  #   vertical-align:-0.125em;
                  #   "
                  # )
                  # ,
                  shinyWidgets::actionBttn(
                    inputId = ns("restart"),
                    label = NULL,
                    style = "simple",
                    color = "default",
                    class = "btn-transparent",
                    icon = bsicons::bs_icon(
                      "arrow-counterclockwise"
                    )
                  )

                  # div(
                  #   style = "display: inline-block; vertical-align: top;",
                  #   id = ns("restart"),
                  #   class = "btn-transparent",
                  #   bsicons::bs_icon(name = "person")
                      # shinyWidgets::actionBttn(
                      #   inputId = ns("restart"),
                      #   label = NULL,
                      #   style = "simple",
                      #   color = "default",
                      #   class = "btn-transparent",
                      #   icon = bsicons::bs_icon(
                      #     "arrow-counterclockwise"
                      #     )
                      #   )
                  # )

                ),
                body = bslib::layout_columns(
                  col_widths = c(6,6),
                  bslib::card(
                    bslib::card_header("Valgt Sygdomsgruppe"),


                    lapply(
                      names(subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")),
                      function(name){
                        picker_input(
                          inputid = ns(paste0(
                            "treatment_",name
                          )),
                          choices = model1_parameters[[name]],
                          label = name,
                          multiple = TRUE,
                          selected = NULL,
                          search = TRUE,
                          placeholder_text = "Alle valgt"
                        )

                      }

                    )



                  ),
                  bslib::card(
                    bslib::card_header("Valgt Sammenligningsgruppe"),
                    lapply(
                      names(subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")),
                      function(name){
                        picker_input(
                          inputid = ns(paste0(
                            "control_",name
                          )),
                          choices = model1_parameters[[name]],
                          label = name,
                          multiple = TRUE,
                          selected = NULL,
                          search = TRUE,
                          placeholder_text = "Alle valgt"
                        )

                      }

                    )





                  )
                )
              ),
              card(
                title = span(bsicons::bs_icon("table"), "Baselinetabel"),
                body =  DT::dataTableOutput(
                  ns("table2")
                )
              )
            ),

            create_tabs(
              X = model1_parameters$sector[k_sector %chin% c(input$k_sector)]$k_allocator,
              fn = function(x){


                tagList(
                  bslib::layout_columns(
                    col_widths = c(6,6),
                    plotly::plotlyOutput(
                      outputId = ns(
                        paste0(
                          "qty_output",x
                        )
                      )
                    ),
                    plotly::plotlyOutput(
                      outputId = ns(
                        paste0(
                          "cost_output",x
                        )
                      )
                    )
                  )
                )



              },
              header = bslib::nav_menu(

                title = "Links",
                bslib::nav_spacer(),
                bslib::nav_item("link_shiny"),
                bslib::nav_item("link_posit")
              )

            )
            # ,
            #
            # card(
            #   title = span(bsicons::bs_icon("bar-chart-steps"), "Resultater"),
            #   body =  DT::dataTableOutput(
            #     ns("table3")
            #   )
            # )
          )

        )
      }
    )



    # 1) Generate recipe
    # object based on user-input
    get_recipe <- shiny::reactive(
      {
        # construct
        # recipe
        recipe_list <- recipe(
          treatment = list(
            k_disease       = input$treatment_disease,
            c_gender        = input$treatment_c_gender,
            c_education     = input$treatment_c_education,
            c_socioeconomic = input$treatment_c_socioeconomic,
            c_age           = input$treatment_c_age
          ),

          control = list(
            k_disease       = input$control_disease,
            c_gender        = input$control_c_gender,
            c_education     = input$control_c_education,
            c_socioeconomic = input$control_c_socioeconomic,
            c_age           = input$control_c_age
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
          k_sector      = input$k_sector,
          k_disease     = c(
            input$treatment_disease,
            input$control_disease)
        )
      }
    )


    prepared_data <- reactive({

      shiny::req(input$treatment_disease)
      shiny::req(input$control_disease)

      prepare_data(
        DT = DT(),
        recipe = get_recipe()
      )



    })


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
            "c_type",
            "c_unit"
          )
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
            "c_type",
            "c_unit"
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
              runif(
                n = 5
              )
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
          k_assignment := data.table::fcase(
            default = 'Sygdomsgruppe',
            k_assignment %chin% 'control', 'Sammenligningsgruppe',
            k_assignment %chin% 'counter_factual', 'Kontrafaktisk sygdomsgruppe'
          )
          ,
        ]



      }
    )


    num_tables <- reactive({
      shiny::req(input$k_sector)

      seq_along(
        unique(cooked_data()$k_allocator)
      )
    })


    reactivePlotTheme <- reactive({

      if(theme() == "light") {

        lightModeTheme()

      } else {

        darkModeTheme()
      }
    })

    # observe(
    #   lapply(
    #     num_tables(),
    #      function(i) {
    #
    #
    #        message(paste("rendering", i))
    #
    #
    #        output[[paste0("cost_output",i)]] <- plotly::renderPlotly(
    #          {
    #
    #            DT <- final_data()[c_type %chin% data.table::fifelse(
    #              test = input$c_type,
    #              yes  = "Incident",
    #              no   = "Prævalent"
    #            )][
    #              k_allocator %chin% unique(flavored_data()$k_allocator)[i]
    #            ]
    #
    #            theme <- reactivePlotTheme()
    #
    #            plotly::layout(
    #
    #              # cost-plot:
    #              p = plotly::plot_ly(
    #                DT,
    #                x = ~k_year,
    #                y = ~v_cost,
    #                color = ~k_assignment,
    #                type = 'scatter',
    #                # fill = 'tozeroy',
    #                mode = 'lines+markers',
    #                line = list(shape = 'spline', smoothing = 1.3)
    #              ),
    #
    #              # Layout Elements
    #              title = 'Omkostninger',
    #              legend = theme$legend,
    #              xaxis = theme$xaxis,
    #              yaxis = theme$yaxis,
    #              plot_bgcolor = 'rgb(0,0,0,0)',
    #              paper_bgcolor ='rgb(0,0,0,0)',
    #              font         = theme$font
    #
    #
    #            )
    #
    #          }
    #        )
    #
    #        output[[paste0("qty_output",i)]] <- plotly::renderPlotly(
    #          {
    #
    #            DT <- final_data()[c_type %chin% data.table::fifelse(
    #              test = input$c_type,
    #              yes  = "Incident",
    #              no   = "Prævalent"
    #            )][
    #              k_allocator %chin% unique(flavored_data()$k_allocator)[i]
    #            ]
    #
    #            theme <- reactivePlotTheme()
    #
    #            plotly::layout(
    #
    #              # cost-plot:
    #              p = plotly::plot_ly(
    #                DT,
    #                x = ~k_year,
    #                y = ~v_qty,
    #                color = ~k_assignment,
    #                type = 'scatter',
    #                # fill = 'tozeroy',
    #                mode = 'lines+markers',
    #                line = list(shape = 'spline', smoothing = 1.3)
    #              ),
    #
    #              # Layout Elements
    #              title = 'Omkostninger',
    #              legend = theme$legend,
    #              xaxis = theme$xaxis,
    #              yaxis = theme$yaxis,
    #              plot_bgcolor = 'rgb(0,0,0,0)',
    #              paper_bgcolor ='rgb(0,0,0,0)',
    #              font         = theme$font
    #
    #
    #            )
    #
    #          }
    #        )
    #
    #     })
    # )



    # Function to generate plotly plots based on the given y-axis variable
    generate_plotly_output <- function(
    data,
    y_var,
    title) {

      theme <- reactivePlotTheme() # Assuming this doesn't change often, call it outside if it does




      plotly::layout(
        plotly::plot_ly(
          data,
          x = ~k_year,
          y = as.formula(paste0("~", y_var)),
          color = ~k_assignment,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(shape = 'spline', smoothing = 1.3)
        ),
        title = paste0(
          unique(data$k_allocator), ": ", unique(data$c_unit)
        ),
        legend = theme$legend,
        xaxis = theme$xaxis,
        yaxis = theme$yaxis,
        plot_bgcolor = 'rgb(0,0,0,0)',
        paper_bgcolor ='rgb(0,0,0,0)',
        font = theme$font,
        xaxis = list(
          range = c(-2,5)
        ),
        margin = list(l = 10, r = 10, b = 15, t = 25)
      #   legend = list(
      #     orientation = 'h', x = 0, y = 1,yref="container", title = list(text = "Indicators:"),
      #   # font    = list(
      #   #   color= if (dark)'#848e9c' else NULL
      #   # ),
      #   annotations = list(
      #     list(
      #       text = paste(
      #         '<b>Ticker:</b>', 'name',
      #         '<b>Interval:</b>', 'interval'
      #       ),
      #       x = 0,
      #       xref = 'paper',
      #       y = 1,
      #       yref = 'paper',
      #       xanchor = 'left',
      #       yanchor = 'bottom',
      #       showarrow = FALSE,
      #       font = list(
      #         size =24
      #       )
      #     ),
      #     list(
      #       text = paste('<b>Market:</b>', 'market'),
      #       x = 1,
      #       xref = 'paper',
      #       y = 1,
      #       yref = 'paper',
      #       xanchor = 'right',
      #       yanchor = 'bottom',
      #       showarrow = FALSE,
      #       font = list(
      #         size = 24
      #       )
      #     )
      #   )
      #   # title = list(
      #   #   text=paste(
      #   #     '<b>Ticker:</b>',
      #   #     name,
      #   #     '<b>Interval:</b>',
      #   #     interval
      #   #   ),
      #   #   x = 0,
      #   #   xref = 'paper',
      #   #   yref = "container"
      #   #   # ,
      #   #   # y = 1,
      #   #   # x = 0,
      #   #
      #   #   # yref =  'paper'
      #   # )
      # )
      )

    }


    observe({
      req(input$c_type)
      lapply(num_tables(), function(i) {
        message(paste("rendering", i))

        DT <- final_data()[c_type %chin% data.table::fifelse(
          test = input$c_type,
          yes  = "Incident",
          no   = "Prævalent"
        )][k_allocator %chin% unique(flavored_data()$k_allocator)[i]]

        output[[paste0("cost_output", i)]] <- plotly::renderPlotly(generate_plotly_output(DT, "v_cost", "Omkostninger"))
        output[[paste0("qty_output", i)]] <- plotly::renderPlotly(generate_plotly_output(DT, "v_qty", "Antal"))
      })
    })








  })
}

## To be copied in the UI
# mod_model1_ui("model1_1")

## To be copied in the server
# mod_model1_server("model1_1")
