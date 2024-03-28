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
mod_model1_server <- function(id){
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
                    picker_input(
                      inputid = ns("randomid1"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = LETTERS,size = 5
                    )
                  ),
                  options_card(
                    header = "Sammenligningsgruppe",
                    picker_input(
                      inputid = ns("randomid2"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = LETTERS,size = 5
                    )

                  ),
                  options_card(
                    header = "Sektor",
                    picker_input(
                      inputid = ns("randomid3"),
                      label = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      choices = LETTERS,size = 5
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
                  shinyWidgets::actionBttn(
                    inputId = ns("restart"),
                    label = NULL,

                    color = "success",
                    style = "simple",
                    icon = bsicons::bs_icon("arrow-counterclockwise")
                  )
                ),
                body = bslib::layout_columns(
                  col_widths = c(6,6),
                  bslib::card(
                    bslib::card_header("Valgt Sygdomsgruppe"),
                    lapply(
                      1:4,
                      function(i){
                        shinyWidgets::pickerInput(
                          inputId = paste(i),
                          label = paste0("choice",i),
                          choices = model1_parameters,multiple = TRUE,width = "100%"
                        )

                      }

                    )



                  ),
                  bslib::card(
                    bslib::card_header("Valgt Sammenligningsgruppe"),
                    lapply(
                      1:4,
                      function(i){
                        shinyWidgets::pickerInput(
                          inputId = paste(i * 100),multiple = TRUE,width = "100%",
                          label = paste0("choice",i),
                          choices = sample(LETTERS,size = 3)
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

            bslib::navset_card_tab(
              bslib::nav_panel(title = "One", p("First tab content.")),
              bslib::nav_panel(title = "Two", p("Second tab content.")),
              bslib::nav_panel(title = "Three", p("Third tab content")),
              bslib::nav_spacer(),
              bslib::nav_menu(
                title = "Links",
                bslib::nav_item("link_shiny"),
                bslib::nav_item("link_posit")
              ),
              full_screen = TRUE,
              sidebar = NULL
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



    output$table1 <- DT::renderDT({
      DT::datatable(
        DT,
        options = list(
          autoWidth = TRUE,
          responsive = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        class = "display compact" # Use compact styling
      )
    })

    output$table2 <- DT::renderDataTable(
      DT
    )

    output$table3 <- DT::renderDataTable(
      DT
    )

  })
}

## To be copied in the UI
# mod_model1_ui("model1_1")

## To be copied in the server
# mod_model1_server("model1_1")
