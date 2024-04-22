#' model2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model2_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_columns(
      col_widths = 12,
      row_heights = c("auto", "auto"),
      min_height = "100%",
      bslib::layout_columns(
        col_widths = c(4,8),

        # 1) Parameter-card
        card(

          header = list(
            title = span(bsicons::bs_icon(name = "gear"), "Parametre"),
            content =list(
              shiny::downloadButton(
                outputId = ns("downloader"),
                label = "Eksporter",
              ),
              bslib::popover(
                options = "400px",
                span(bsicons::bs_icon("gear"), "Sygedage"),
                shiny::sliderInput(
                  inputId = ns("effect"),
                  label   = "Sygedage",
                  width   = "300px",
                  value   = 1,
                  min     = 1,
                  max     = 28
                )
              )
            )
          ),

          bslib::layout_columns(
            col_widths = 12,
            row_heights = "auto",

            shiny::div(
              options_card(
                header = NULL,
                footer = NULL,
                width = "100%",
                picker_input(
                  inputid = ns("k_sector"),
                  label   = "Aldersgruppe",
                  choices = model2_parameters$k_sector,
                  multiple = TRUE,
                  selected = model2_parameters$k_sector,
                  search = TRUE,
                  placeholder_text = "Intet valgt"

                )
              ),
              options_card(
                header = NULL,
                footer = NULL,
                width = "100%",
                picker_input(
                  inputid = ns("k_education"),
                  label   = "Udannelsesniveau",
                  choices = model2_parameters$k_education,
                  multiple = TRUE,
                  selected = model2_parameters$k_education,
                  search = FALSE,
                  placeholder_text = "Intet valgt"

                )
              ),
              options_card(
                header = NULL,
                footer = NULL,
                width = "100%",
                picker_input(
                  inputid = ns("k_allocator"),
                  label   = "Hvem tager sygedagen?",
                  choices = model2_parameters$k_allocator,
                  multiple = TRUE,
                  selected = model2_parameters$k_allocator,
                  search = FALSE,
                  placeholder_text = "Intet valgt"

                )
              )
            )





          )
        ),

        # 2) table-card
        # for the tabular output
        card(
          header = list(
            title = span(bsicons::bs_icon("table"), "Resultater"),
            content = list(
              tooltip(
                msg = c(
                  "Viser resultaterne i Tableform. Samme som forneden"
                )
              )
            )
          ),
          DT::dataTableOutput(
            outputId = ns("table")
          )

        )
      ),


      # 3) plot-card
      # for the plotly output
      # 2) table-card
      # for the tabular output
      card(
        header = list(
          title = span(bsicons::bs_icon("table"), "Resultater"),
          content = list(
            tooltip(
              msg = c(
                "Viser resultaterne i Tableform. Samme som forneden"
              )
            )
          )
        ),
        plotly::plotlyOutput(
          outputId = ns("plot")
        )
      )

    )


  )
}

#' model2 Server Functions
#'
#' @noRd
mod_model2_server <- function(id, theme){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # 1) Extract base
    # data
    DT <- reactive(
      {

        extract_data(
          DB_connection = DB_connection,
          table         = "model2"
        )

      }
    )


    # 1) Aggregate the
    # data
    DT_aggregate <- reactive(
      {

        # 1) filter data
        # accordingly
        DT <- DT()[
          k_allocator %chin% input$k_allocator &
            k_education %chin% input$k_education &
            k_sector %chin% input$k_sector
        ]

        DT <- DT[
          ,
          .(
            v_cost = round(sum(
              v_cost,
              na.rm = TRUE
            )/sum(v_weight, na.rm = TRUE)
          ) * input$effect)
          ,
          by = .(
            k_sector,
            k_allocator
          )
        ]

       DT[
         ,
         k_allocator := data.table::fcase(
           default = "Delt",
           k_allocator %chin% 'low', "Lavest Uddannede",
           k_allocator %chin% "high", "Højest Uddannede"
         )
         ,
       ]


        return(
          DT
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


        wb <- create_workbook(
          DT = DT_aggregate()
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


    output$table <- DT::renderDataTable(
      {

        table_DT <- data.table::dcast(
          data = DT_aggregate(),
          formula = k_allocator ~ k_sector,
          value.var = 'v_cost'
        )

        data.table::setnames(
          table_DT,
          old = "k_allocator",
          new = "Uddannelse"
        )


        table_DT[
          ,
          group:= "Hvem tager sygedagene?"
          ,
        ]
        generate_table(
          DT = table_DT,
          header = NULL
        )



      }
    )


    reactivePlotTheme <- reactive({

      if(theme() == "light") {

        lightModeTheme()

      } else {

        darkModeTheme()
      }
    })


    output$plot <- plotly::renderPlotly(
      {

        theme <- reactivePlotTheme()


        plotly::layout(
          p = plotly::layout(
            p = plotly::plot_ly(
              data  = DT_aggregate(),
              y     = ~factor(
                k_sector,
                levels = c('0-2 år', '3-6 år', '7-11 år', '12-17 år')
              ),
              x     = ~v_cost,
              color = ~k_allocator,
              type = "bar",
              orientation = "h",
              alpha = 0.7,
              marker = list(
                line = list(
                  color = 'black',
                  width = 1.5))),
            legend = theme$legend,
            xaxis = theme$xaxis,
            yaxis = theme$yaxis,
            plot_bgcolor = 'rgb(0,0,0,0)',
            paper_bgcolor ='rgb(0,0,0,0)',
            font = theme$font



          ),
          yaxis = list(
            title = "Aldersgruppe"
          ),
          xaxis = list(
            title = "Ugentlig produktionstab pr. forældre (kr.)"
          )
        )







      }
    )



  })
}

## To be copied in the UI
# mod_model2_ui("model2_1")

## To be copied in the server
# mod_model2_server("model2_1")
