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
      col_widths = c(12),
      row_heights = c(1,1),
      div(class="dim-overlay",
          div(class="card-container",
              div(
                class = "d-flex justify-content-center align-items-center",
                style = "height: 100%;",  # This ensures the div takes the full viewport height

                bslib::layout_columns(
                  col_widths = c(12),
                  row_heights = c(1,1),
                  bslib::layout_columns(
                    row_heights = c(1,1),
                    col_widths = c(4,4,4),
                    options_card(
                      header = "Sygdomsgruppe",
                      footer = NULL,
                      width  = "320px",
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
                      width  = "320px",
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
                      width  = "320px",
                      picker_input(
                        inputid = ns("k_sector"),
                        label = NULL,
                        multiple = FALSE,
                        search = TRUE,
                        choices = model1_parameters$k_sector,
                        size = 10
                      )

                    ),






                  ),
                  shinyWidgets::actionBttn(
                    block = TRUE,
                    inputId =  ns("start"),
                    label = "Start",
                    style = "float",no_outline = TRUE,
                    color = "default",
                    icon = icon("bars")
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
      {

        # 0) Upon restart
        # the choices should be
        # updated with earlier
        # picked values
        shinyWidgets::updatePickerInput(
          inputId = "treatment_disease",
          selected = input$treatment_disease
        )

        shinyWidgets::updatePickerInput(
          inputId = "control_disease",
          selected = input$control_disease
        )

        shinyWidgets::updatePickerInput(
          inputId = "k_sector",
          selected = input$k_sector
        )




        output$body <- shiny::renderUI(
          landing
        )




      }




    )



    observeEvent(
      input$start,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        # 0) define parameters
        #
        # prefix is the prefix of the
        # input id
        #
        # the pattern is name of the input
        # ids
        prefixes <- c("treatment_", "control_")

        patterns <- names(
          subset_list(
            list = model1_parameters,
            pattern = "age|educ|gender|socio"
          )
        )

        # 1) Update all choices
        # by iterating over all inputids
        # for both treatment and control
        #
        #
        # NOTE: This updates ALL values
        # regardless of wether it has changed.
        # This might be suboptimal
        lapply(
          X   = patterns,
          FUN = function(name) {

            lapply(
              X = prefixes,
              FUN = function(prefix) {
                shinyWidgets::updatePickerInput(
                  inputId = paste0(prefix, name),
                  selected = input[[paste0(prefix, name)]]
                )
              }
            )

          }
        )

        # 2) Update the c_type
        # switcher input
        shinyWidgets::updateSwitchInput(
          inputId = "c_type",
          value   = input$c_type
        )

      }
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
                        width = "100%",size = "mini",
                        value = TRUE,
                        onLabel = bsicons::bs_icon("check",size = "1.5rem")
                      )
                  ),
                  shinyWidgets::actionBttn(
                    inputId = ns("restart"),
                    label = NULL,
                    size = "sm",
                    style = "simple",
                    color = "primary",
                    #class = "btn-transparent",
                    icon = bsicons::bs_icon(
                      "arrow-counterclockwise"
                      #,size = "1.5rem"
                    )
                  )

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
              bslib::nav_menu(

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
      )

    }


    observe({
      req(input$c_type)
      lapply(num_tables(), function(i) {

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
