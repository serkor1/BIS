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
              bslib::popover(
                id = ns("choice_popover"),
                trigger = span(bsicons::bs_icon("gear"), "Menu"),
                options = list(
                  popoverMaxWidth = "400px"
                ),
                title = span(bsicons::bs_icon("gear"), "Menu"),

                bslib::layout_columns(
                  col_widths = 12,

                  # chosing sector
                  # for each outcome
                  picker_input(
                    inputid  = ns("k_sector"),
                    label    = "Udfaldsmål",
                    multiple = FALSE,
                    search   = FALSE,
                    choices  = model1_parameters$k_sector,
                    size     = 10
                  ),




                  shinyWidgets::radioGroupButtons(
                    inputId = ns("c_type"),
                    label = "Patienttype",
                    justified = TRUE,
                    choices = model1_parameters$c_type,
                    status = "primary"
                  ),




                  # Add restart and
                  # export buttons side-by-side
                  bslib::layout_columns(
                    col_widths = c(6,6),

                    shinyWidgets::downloadBttn(
                      outputId = ns("downloader"),
                      label = "Eksporter",
                      size = "s",
                      style = "simple",
                      color = "primary"

                    ),

                    shinyWidgets::actionBttn(
                      inputId = ns("restart"),
                      label = "Genstart",
                      size = "s",
                      style = "simple",
                      color = "primary",
                      #class = "btn-transparent",
                      icon = bsicons::bs_icon(
                        "arrow-counterclockwise"
                        #,size = "1.5rem"
                      )
                    )
                  )

                )



              )









            )
          ),

          # Demographics conent:
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(6,6),

              # treatment-group parameters
              card(
                header = list(
                  title = shiny::uiOutput(
                    outputId = ns("treatment_disease_label")
                  ),
                  content = list(
                    tooltip(
                      "Den valgte gruppes karakteristika"
                    )
                  )
                ),

                # content
                bslib::card_body(
                  style = "gap: 5px !important;",
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

              # control group parameters
              card(
                header = list(
                  title = shiny::uiOutput(
                    outputId = ns("control_disease_label")
                  ),
                  content = list(
                    tooltip(
                      "Sammenligningsgruppens karakteristika"
                    )
                  )),
                bslib::card_body(
                  height = "auto",
                  style = "gap: 5px !important;",
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
                  span(bsicons::bs_icon("sliders"), "Effekter")
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
                  title = span(bsicons::bs_icon("sliders"), "Effekter")

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
                        choices = c("Befolkningen", model1_parameters$k_disease),
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
        shinyWidgets::updateRadioGroupButtons(
          inputId = "c_type",
          selected = input$c_type
        )





      }
    )





    observeEvent(
      input$restart,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {


        bslib::toggle_popover(show = FALSE,id = "choice_popover")


        showModal(
          landing
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
            k_disease       = data.table::fifelse(
              test = grepl(
                x = input$control_disease,
                pattern = "befolkning",
                ignore.case = TRUE
              ),
              yes = paste0("pop_",input$treatment_disease),
              no  = input$control_disease),
            c_gender        = input$control_c_gender,
            c_education     = input$control_c_education,
            c_socioeconomic = input$control_c_socioeconomic,
            c_age           = input$control_c_age
          )
        )


      }
    )


    output$control_disease_label <- shiny::renderUI(
      {

        shiny::p(
          bsicons::bs_icon("virus2"),
          shiny::strong("Sammenligningsgruppe:"),
          input$control_disease

        )
      }
    )


    output$treatment_disease_label <- shiny::renderUI(
      {

        shiny::p(
          bsicons::bs_icon("virus"),
          shiny::strong("Valg gruppe:"),
          input$treatment_disease

        )
      }
    )




    # 2) Extract data from
    # SQL database
    DT <- shiny::reactive(
      {
        req(input$start)

        treatment_disease <- input$treatment_disease
        control_disease   <- input$control_disease

        # extract data;
        DT_ <- extract_data(
          DB_connection = DB_connection,
          table         = "model1",
          k_disease     = c(
            treatment_disease,
            control_disease
          )
        )

        # NOTE: if general population
        # is chosen then it wont return anything
        # for the control
        # it is in a different table
        if (grepl(pattern = "befolkning", x = control_disease,ignore.case = TRUE)) {


          DT_pop <- extract_data(
            DB_connection = DB_connection,
            table         = "population",
            k_disease     = c(
              treatment_disease
            )
          )

          DT_pop[
            ,
            k_disease := paste0("pop_", k_disease)
            ,
          ]

          DT_ <- rbind(
            DT_,
            DT_pop
          )

        }

        return(
          DT_
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


    output$downloader <- shiny::downloadHandler(
      filename = function() {
        paste('workbook.xlsx', sep="")
      },
      content = function(file) {

        # 1) start download indicator
        # after user clicks downlaod
        showNotification(
          ui = shiny::span(bsicons::bs_icon("download"), "Downloader..."),
          action = NULL,
          duration = NULL,
          closeButton = FALSE,
          id = "download_indicator",
          type = c("default"),
          session = getDefaultReactiveDomain()
        )


        # 2) prepare data for export
        #
        #
        # 2.1) store as DT while filtering for
        # c_type
        DT <- as_table(
          flavored_data()[
            c_type %chin% input$c_type
          ],
          treatment_disease = input$treatment_disease,
          control_disease   = input$control_disease,
          effect_data       = data.table::data.table(
            effect = c(
              NA,
              NA,
              NA,
              input$effect_1/100,
              input$effect_2/100,
              input$effect_3/100,
              input$effect_4/100,
              input$effect_5/100

            ),
            k_year = -2:5
          )
        )

        # # 2.2) merge effect data
        # # to the data
        # DT <- merge(
        #   x = DT,
        # y = data.table::data.table(
        #   effect = c(
        #     NA,
        #     NA,
        #     NA,
        #     input$effect_1/100,
        #     input$effect_2/100,
        #     input$effect_3/100,
        #     input$effect_4/100,
        #     input$effect_5/100
        #
        #   ),
        #   k_year = -2:5
        # ),
        #   by = "k_year",
        #   all.x = TRUE
        # )

        # # # 2.3) set names
        # # # of the data
        # data.table::setnames(
        #   x = DT,
        #   skip_absent = TRUE,
        #   old = c(
        #     "k_year",
        #     "k_allocator",
        #     "c_type",
        #     "effect"
        #     ),
        #   new = c(
        #     "Tid",
        #     "Byrdemål",
        #     "Patienttype",
        #     "Effekt (%)")
        # )




        wb <- create_workbook(
          DT = DT,
          f  = expression(DT$k_sector)
        )


        openxlsx::saveWorkbook(
          wb = wb,
          file = file,
          overwrite = TRUE
        )




        # 6) Close notification
        removeNotification("download_indicator", session = getDefaultReactiveDomain())




      }
    )





    final_data <- shiny::reactive(
      {

        flavored_data()[
          k_assignment %chin% c('control', 'treatment', 'counter_factual') &
            k_sector  %chin% input$k_sector,
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
        unique(final_data()$k_allocator)
      )
    })


    plotting_data <- reactive(
      {
        #req(input$c_type)
        lapply(
          num_tables(),
          function(i) {
            final_data()[c_type %chin% input$c_type][k_allocator %chin% unique(final_data()$k_allocator)[i]]
          }
        )
      }
    )



    observe({
      data_list <- plotting_data()  # Capture the list of data tables

      lapply(
        seq_along(data_list),
        function(i) {

          DT <- data_list[[i]]

          output[[paste0("cost_output", i)]] <- plotly::renderPlotly(
            {
              layout(
                plot = plot(
                  data = DT,
                  x = setNames("k_year", "Tid"),
                  y = setNames(
                    "v_cost",
                    paste("Pris pr.",unique(DT$c_unit))
                  ),
                  color = ~k_assignment,
                  type = 'scatter',
                  mode = 'lines+markers',
                  line = list(shape = 'spline', smoothing = 1.3)
                ),
                title = "Omkostninger",
                dark = as.logical(
                  theme() == "dark"
                )
              )
            }
          )

          output[[paste0("qty_output", i)]] <- plotly::renderPlotly(
            {
              layout(
                plot = plot(
                  data = DT,
                  x = setNames("k_year", "Tid"),
                  y = setNames(
                    "v_qty",
                    paste("Antal", unique(DT$c_unit), "pr. person")
                  ),
                  color = ~k_assignment,
                  type = 'scatter',
                  mode = 'lines+markers',
                  line = list(shape = 'spline', smoothing = 1.3)
                ),
                title = "Forbrug",
                dark = as.logical(
                  theme() == "dark"
                )
              )


            }
          )

        }
      )
    }
    )

    output$baseline <- DT::renderDT({

      # 1) extract data
      DT <- prepare_data(
        extract_data(
          DB_connection = DBI::dbConnect(
            drv = RSQLite::SQLite(),
            dbname = "inst/extdata/db.sqlite"
          ),
          table = "model1_baseline",
          k_disease = c(input$treatment_disease, input$control_disease),
          c_type    = input$c_type
        ),
        recipe = get_recipe()
      )

      DT <- DT[
        ,
        .(
          v_characteristics = sum(v_weights * v_characteristics, na.rm = TRUE) / sum(v_weights, na.rm = TRUE)
        )
        ,
        by = .(
          k_allocator,
          k_assignment
        )
      ]

      DT[
        ,
        group := data.table::fcase(
          default = as.character(span(bsicons::bs_icon("people"), "Alder")),
          grepl(
            pattern = "ufaglært|faglært|videregående uddannelse",
            ignore.case = TRUE,
            x = k_allocator
          ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
          grepl(
            pattern = "mand|kvinde",
            ignore.case = TRUE,
            x = k_allocator
          ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
          grepl(
            pattern = "aktiv|inaktiv|udenfor",
            ignore.case = TRUE,
            x = k_allocator
          ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
        )
        ,
      ]

      DT <- data.table::dcast(
        data = DT,
        formula = k_allocator + group ~ k_assignment,
        value.var = "v_characteristics"
      )

      data.table::setnames(
        DT,
        old = c("k_allocator", "control", "treatment"),
        new = c("Karakteristika", "Valgt Gruppe", "Sammenligningsgruppe"),
        skip_absent = TRUE
      )

      data.table::setorder(
        DT,
        -group
      )


      generate_table(
        header = NULL,
        DT = DT
      )



      # generate_table(
      #   DT = mtcars,
      #   header = NULL
      # )


    })

    # output$baseline <- DT::renderDT({
    #
    #
    #   req(input$start)
    #
    #
    #   # treatment = list(
    #   #   k_disease       = input$treatment_disease,
    #   #   c_gender        = input$treatment_c_gender,
    #   #   c_education     = input$treatment_c_education,
    #   #   c_socioeconomic = input$treatment_c_socioeconomic,
    #   #   c_age           = input$treatment_c_age
    #   # ),
    #   #
    #   # control = list(
    #   #   k_disease       = input$control_disease,
    #   #   c_gender        = input$control_c_gender,
    #   #   c_education     = input$control_c_education,
    #   #   c_socioeconomic = input$control_c_socioeconomic,
    #   #   c_age           = input$control_c_age
    #   # )
    #
    #   recipe_list <- recipe(
    #     treatment = list(
    #       k_disease  = input$treatment_disease
    #     ),
    #     control = list(
    #       k_disease = input$control_disease
    #     )
    #   )
    #
    #
    #   DT <- extract_data(
    #     DB_connection = DBI::dbConnect(
    #       drv = RSQLite::SQLite(),
    #       dbname = "inst/extdata/db.sqlite"
    #     ),
    #     table = "baseline",
    #     k_disease = c(
    #       input$treatment_disease,
    #       input$control_disease
    #     )
    #   )
    #
    #
    #
    #   # This filters all the
    #   # data. We need a new function
    #   # that sets to 0 if not chosen.
    #   #
    #   #
    #   # NOTE: If empty, everything should
    #   # be displayed.
    #
    #   DT <- prepare_data(
    #     DT = DT,
    #     recipe = recipe_list
    #   )
    #
    #
    #
    #
    #   DT[
    #     ,
    # group := data.table::fcase(
    #   default = as.character(span(bsicons::bs_icon("people"), "Alder")),
    #   grepl(
    #     pattern = "educ",
    #     ignore.case = TRUE,
    #     x = k_variable
    #   ), as.character(span(bsicons::bs_icon(name = "book"),"Uddannelse")),
    #   grepl(
    #     pattern = "gender",
    #     ignore.case = TRUE,
    #     x = k_variable
    #   ), as.character(span(bsicons::bs_icon(name= "gender-ambiguous"), "Køn")),
    #   grepl(
    #     pattern = "socio",
    #     ignore.case = TRUE,
    #     x = k_variable
    #   ), as.character(span(bsicons::bs_icon(name= "building"), "Arbejdsmarkedstatus"))
    # )
    #     ,
    #   ]
    #
    #   DT <-  data.table::dcast(
    #     data = DT[c_type == input$c_type],
    #     formula = c_variable + group ~ k_assignment,
    #     value.var = "v_obs"
    #   )
    #
    # data.table::setorder(
    #   DT,
    #   -group
    # )
    #
    #   data.table::setcolorder(
    #     DT,
    #     c('c_variable', colnames(DT)[grepl(pattern = "valgt", ignore.case = TRUE, x = colnames(DT))])
    #   )
    #
    #   data.table::setnames(
    #     x = DT,
    #     old = c("c_variable", "control", "treatment"),
    #     new = c("Karakteristika", input$control_disease, input$treatment_disease)
    #   )
    #   generate_table(
    #     DT = DT,
    #     header = NULL
    #   )
    # })



  })
}

## To be copied in the UI
# mod_model1_ui("model1_1")

## To be copied in the server
# mod_model1_server("model1_1")
