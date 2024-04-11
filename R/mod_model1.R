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
    bslib::layout_columns(
      height = "100%",
      col_widths = c(12),
      row_heights = c(1,1),
      # Upper part of Application;
      bslib::layout_columns(
        col_widths = c(6,6),


        # Left card start;
        card(
          header = list(
            title = shiny::span(bsicons::bs_icon(name = "person"), "Demografi"),
            content = list(

              picker_input(
                inputid = ns("k_sector"),
                label = NULL,
                multiple = FALSE,
                search = TRUE,
                choices = model1_parameters$k_sector,
                size = 10
              ),
              shinyWidgets::switchInput(
                label    = "Incidens",
                onStatus = "primary",
                inputId  = ns("c_type"),
                width = "100%",
                size = "small",
                value = TRUE,
                onLabel = bsicons::bs_icon("check",size = "1.5rem")
              ),
              shinyWidgets::actionBttn(
                inputId = ns("restart"),
                label = NULL,
                size = "m",
                style = "simple",
                color = "primary",
                #class = "btn-transparent",
                icon = bsicons::bs_icon(
                  "arrow-counterclockwise"
                  #,size = "1.5rem"
                )
              )

            )
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(6,6),
              bslib::card(
                bslib::card_header(

                  shiny::p(
                    bsicons::bs_icon("virus"),
                    shiny::strong("Diagnose:"),
                    "input$treatment_disease"

                  )
                ),

                bslib::card_body(

                  style = "gap: 25px !important;",
                  lapply(
                    names(
                      subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")
                    ),
                    function(name){
                      picker_input(
                        inputid = ns(paste0(
                          "treatment_",name
                        )),
                        choices = model1_parameters[[name]],
                        label = HTML(data.table::fcase(
                          default = as.character(span(bsicons::bs_icon("people"), "Alder")),
                          grepl(
                            pattern = "educ",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
                          grepl(
                            pattern = "gender",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
                          grepl(
                            pattern = "socio",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
                        )),
                        multiple = TRUE,
                        selected = NULL,
                        search = TRUE,
                        placeholder_text = "Alle valgt"
                      )

                    }

                  )
                )





              ),

              bslib::card(
                bslib::card_header(
                  shiny::p(
                    bsicons::bs_icon("virus2"),
                    shiny::strong("Diagnose:"),
                    "input$control_disease"

                  )
                ),
                bslib::card_body(
                  style = "gap: 25px !important;",
                  lapply(
                    names(subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")),
                    function(name){
                      picker_input(
                        inputid = ns(paste0(
                          "control_",name
                        )),
                        choices = model1_parameters[[name]],
                        label = HTML(data.table::fcase(
                          default = as.character(span(bsicons::bs_icon("people"), "Alder")),
                          grepl(
                            pattern = "educ",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
                          grepl(
                            pattern = "gender",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
                          grepl(
                            pattern = "socio",
                            ignore.case = TRUE,
                            x = name
                          ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
                        )),
                        multiple = TRUE,
                        selected = NULL,
                        search = TRUE,
                        placeholder_text = "Alle valgt"
                      )

                    }

                  )
                )






              )
            )
          )

          # Left card end
        ),

        card(
          header = list(
            title = span(bsicons::bs_icon("table"), "Baselinetabel")
          ),
          bslib::card_body(

            DT::dataTableOutput(
              ns("baseline")
            )
          )
        )
      ),

      uiOutput(
        ns("test")
      )



    )



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
mod_model1_server <- function(id, theme, init){
  moduleServer( id, function(input, output, session){
    ns <- session$ns







    observeEvent(
      input$start,
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      {
        output$test <- shiny::renderUI(
          {



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
              bslib::nav_item(
                bslib::popover(
                  options = list(
                    popoverMaxWidth = "400px"
                  ),
                  span(bsicons::bs_icon("gear"), "Effekter")
                  ,
                  lapply(
                    1:5,
                    function(x) {
                      shiny::sliderInput(
                        inputId = ns(paste0('effect_', x)),
                        label = paste("Tid", x),
                        width = "300px",
                        value = 0,
                        min = 0,
                        max = 100
                      )
                    }

                  ),
                  title = "Effekter"

                )
              ),
              height = "100%"




            )

          }
        )
      }
    )


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
                    col_widths = c(6,6),
                    options_card(
                      header = span(bsicons::bs_icon("virus"),"Sygdomsgruppe"),
                      footer = NULL,
                      width  = "20vw",
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
                      header = span(bsicons::bs_icon("virus2"),"Sammenligningsgruppe"),
                      footer = NULL,
                      width  = "20vw",
                      picker_input(
                        inputid = ns("control_disease"),
                        label = NULL,
                        multiple = FALSE,
                        search = TRUE,
                        choices = model1_parameters$k_disease,
                        size = 10
                      )

                    )
                  ),
                  shinyWidgets::actionBttn(
                    block = TRUE,
                    inputId =  ns("start"),
                    label = "Start",
                    style = "float",
                    no_outline = TRUE,
                    color = "default",
                    icon = icon("bars")
                  )
                )

              )
          )

      )
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



    showModal(
      landing
    )




    # this is start button
    # within the second part
    observeEvent(
      input$start,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        removeModal()

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
      input$restart,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {


        showModal(
          landing
        )



      }
    )




    # observeEvent(
    #   input$start,
    #   ignoreInit = TRUE,
    #   ignoreNULL = TRUE,
    #   {
    #
    #
    #
    #
    #
    #     output$body <- shiny::renderUI(
    #       bslib::layout_columns(
    #         height = "100%",
    #         col_widths = c(12),
    #         row_heights = c(1,1),
    #         bslib::layout_columns(
    #           col_widths = c(6,6),
    #           card(
    #             title = span(bsicons::bs_icon(name = "person"),"Demografi"),
    #             header = shiny::div(
    #               div(style = "display: inline-block; vertical-align: top; margin-right: 1px;", # Add margin for some space between elements
    #                   shinyWidgets::switchInput(
    #                     label    = "Incidens",
    #                     onStatus = "primary",
    #                     inputId  = ns("c_type"),
    #                     width = "100%",size = "mini",
    #                     value = TRUE,
    #                     onLabel = bsicons::bs_icon("check",size = "1.5rem")
    #                   )
    #               ),
    #               shinyWidgets::actionBttn(
    #                 inputId = ns("restart"),
    #                 label = NULL,
    #                 size = "sm",
    #                 style = "simple",
    #                 color = "primary",
    #                 #class = "btn-transparent",
    #                 icon = bsicons::bs_icon(
    #                   "arrow-counterclockwise"
    #                   #,size = "1.5rem"
    #                 )
    #               )
    #
    #             ),
    #             body = bslib::layout_columns(
    #               col_widths = c(6,6),
    #               bslib::card(
    #                 bslib::card_header(
    #
    #                   shiny::p(
    #                     bsicons::bs_icon("virus"),
    #                     shiny::strong("Diagnose:"),
    #                     input$treatment_disease
    #
    #                   )
    #                 ),
    #
    #
    #                 lapply(
    #                   names(subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")),
    #                   function(name){
    #                     picker_input(
    #                       inputid = ns(paste0(
    #                         "treatment_",name
    #                       )),
    #                       choices = model1_parameters[[name]],
    #                       label = HTML(data.table::fcase(
    #                         default = as.character(span(bsicons::bs_icon("people"), "Alder")),
    #                         grepl(
    #                           pattern = "educ",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
    #                         grepl(
    #                           pattern = "gender",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
    #                         grepl(
    #                           pattern = "socio",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
    #                       )),
    #                       multiple = TRUE,
    #                       selected = NULL,
    #                       search = TRUE,
    #                       placeholder_text = "Alle valgt"
    #                     )
    #
    #                   }
    #
    #                 )
    #
    #
    #
    #               ),
    #               bslib::card(
    #                 bslib::card_header(
    #                   shiny::p(
    #                     bsicons::bs_icon("virus2"),
    #                     shiny::strong("Diagnose:"),
    #                     input$control_disease
    #
    #                   )
    #                 ),
    #                 lapply(
    #                   names(subset_list(list = model1_parameters, pattern = "age|educ|gender|socio")),
    #                   function(name){
    #                     picker_input(
    #                       inputid = ns(paste0(
    #                         "control_",name
    #                       )),
    #                       choices = model1_parameters[[name]],
    #                       label = HTML(data.table::fcase(
    #                         default = as.character(span(bsicons::bs_icon("people"), "Alder")),
    #                         grepl(
    #                           pattern = "educ",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
    #                         grepl(
    #                           pattern = "gender",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
    #                         grepl(
    #                           pattern = "socio",
    #                           ignore.case = TRUE,
    #                           x = name
    #                         ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
    #                       )),
    #                       multiple = TRUE,
    #                       selected = NULL,
    #                       search = TRUE,
    #                       placeholder_text = "Alle valgt"
    #                     )
    #
    #                   }
    #
    #                 )
    #
    #
    #
    #
    #
    #               )
    #             )
    #           ),
    #           card(
    #             title = span(bsicons::bs_icon("table"), "Baselinetabel"),
    #             body =  DT::dataTableOutput(
    #               ns("baseline")
    #             )
    #           )
    #         )
    #         # ,
    #         #
    #         # card(
    #         #   title = span(bsicons::bs_icon("bar-chart-steps"), "Resultater"),
    #         #   body =  DT::dataTableOutput(
    #         #     ns("table3")
    #         #   )
    #         # )
    #       )
    #
    #     )
    #   }
    # )



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

        req(input$start)


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
              0,
              0,
              0,
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
        title = list(
          text = paste0(
          unique(data$k_allocator), ": ", unique(data$c_unit)
        ),
        x = 1,
        xref = "paper",
        xanchor = "right"

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



    output$baseline <- DT::renderDT({


      req(input$start)


      # treatment = list(
      #   k_disease       = input$treatment_disease,
      #   c_gender        = input$treatment_c_gender,
      #   c_education     = input$treatment_c_education,
      #   c_socioeconomic = input$treatment_c_socioeconomic,
      #   c_age           = input$treatment_c_age
      # ),
      #
      # control = list(
      #   k_disease       = input$control_disease,
      #   c_gender        = input$control_c_gender,
      #   c_education     = input$control_c_education,
      #   c_socioeconomic = input$control_c_socioeconomic,
      #   c_age           = input$control_c_age
      # )

      recipe_list <- recipe(
        treatment = list(
          k_disease  = input$treatment_disease
        ),
        control = list(
          k_disease = input$control_disease
        )
      )


      DT <- extract_data(
        DB_connection = DBI::dbConnect(
          drv = RSQLite::SQLite(),
          dbname = "inst/extdata/db"
        ),
        table = "baseline",
        k_disease = c(
          input$treatment_disease,
          input$control_disease
        )
      )



      # This filters all the
      # data. We need a new function
      # that sets to 0 if not chosen.
      #
      #
      # NOTE: If empty, everything should
      # be displayed.

      DT <- prepare_data(
        DT = DT,
        recipe = recipe_list
      )




      DT[
        ,
        group := data.table::fcase(
          default = as.character(span(bsicons::bs_icon("people"), "Alder")),
          grepl(
            pattern = "educ",
            ignore.case = TRUE,
            x = k_variable
          ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
          grepl(
            pattern = "gender",
            ignore.case = TRUE,
            x = k_variable
          ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
          grepl(
            pattern = "socio",
            ignore.case = TRUE,
            x = k_variable
          ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
        )
        ,
      ]

      DT <-  data.table::dcast(
        data = DT[c_type == "Incident"],
        formula = c_variable + group ~ k_assignment,
        value.var = "v_obs"
      )

      data.table::setorder(
        DT,
        -group
      )

      data.table::setcolorder(
        DT,
        c('c_variable', colnames(DT)[grepl(pattern = "valgt", ignore.case = TRUE, x = colnames(DT))])
      )

      data.table::setnames(
        x = DT,
        old = "c_variable",
        new = "Karakteristika"
      )
      generate_table(
        DT = DT,
        header = NULL
      )
    })












  })
}

## To be copied in the UI
# mod_model1_ui("model1_1")

## To be copied in the server
# mod_model1_server("model1_1")
