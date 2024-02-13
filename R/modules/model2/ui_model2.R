# script: ui_model2
# date: 2023-07-03
# author: Serkan Korkmaz, serkor1@duck.com
# objective: The UI for the secoond model;
# script start;

# ui body; ####
.ui_model2_body <- function(
    input,
    output,
    id
) {


  # namespace;
  ns <- NS(
    id
  )

  tagList(
    column(
      width = 12,

      # Top steps attached
      # to the parameter grid
      tagList(
        shiny.semantic::steps(
          id = 'steps',
          class = 'attached four',
          steps_list = list(
            
            # first step;
            single_step(
              id = "step_1",
              step_class = 'active',
              title = "Vælg sektor",
              description = "It's night?",
              icon_class = "hospital"
            ),
            
            # second step;
            single_step(
              id = "step_2",
              title = "Vælg sygdomsgruppe",
              description = "Order some food",
              icon_class = "stethoscope",
              step_class = 'active'
            ),
            
            # third step;
            single_step(
              id = "step_3",
              title = "Vælg sammenligningsgruppe",
              description = "Feed the Kiwi",
              icon_class = "blind",
              step_class = 'active'
            ),
            
            # fourth step;
            single_step(
              id = "step_3",
              title = "Vælg forventet effekt",
              description = "Feed the Kiwi",
              icon_class = "sliders horizontal",
              step_class = 'active'
            )

          )
        )
      ),

      # add parameter choices
      segment(
        class = paste(
          segment_color,
          'secondary attached'
        ),
        grid(
          grid_template = grid_template(
            default = list(
              areas = rbind(
                c('first','second','third', 'fourth')
              ),
              rows_height = c('auto', 'auto', 'auto', 'auto'),
              cols_width  = c('1fr', '1fr', '1fr', '1fr')
            )
          ),
          first = segment(
            class = 'basic',
            shiny::selectInput(
              inputId = ns('assignment_model2'),
              label = 'Aldersgruppe',
              choices = assignment_model2,
              multiple = FALSE,
              selected = '',
              width = '100%'
            )
          ),
          second = segment(
            class = 'basic',
            shiny::selectInput(
              inputId = ns('education_model2'),
              label = 'Uddannelse',
              choices = education_model2,
              multiple = FALSE,
              selected = '',
              width = '100%'
            )
          ),
          third = segment(
            class = 'basic',
            shiny::selectInput(
              inputId = ns('allocator'),
              label = 'Hvem tager sygedagene?',
              choices = assignment_model2,
              multiple = FALSE,
              selected = '',
              width = '100%'
            )
          ),
          fourth = segment(
            class = 'basic',
            shiny::sliderInput(
              inputId = ns('sickdays'),
              label = NULL,
              width = '100%',
              value = 0,
              min = 0,
              max = 14
            )
          )
        )
      ),

      # bottom steps attached to
      # porameter segment
      steps(
        id = 'steps',
        class = 'attached two',
        steps_list = list(
          single_step(
            step_class = 'active',
            id = "step_1",
            title = "Step 1",
            description = "It's night?",
            icon_class = "moon"
          ),
          single_step(
            step_class = 'active',
            id = "step_2",
            title = "Step 2",
            description = "Order some food",
            icon_class = "bug"
          )
        )
      )

    ),


    # output area
    tags$div(
      class = 'ui segments',
      segment(
        class = segment_color,
        # Add plotly
        plotlyOutput(
          width = '100%',
          outputId = ns(
            'plot_model2'
          )
        )

      ),

      tagList(
        segment(
          class = segment_color,
          # Add plotly
          semantic_DTOutput(
            outputId = ns(
              'table'
            )
          )

        ),
        steps(
          id = 'steps',
          class = 'attached',
          steps_list = list(

            single_step(
              step_class = 'active link',
              id = 'exccel_holder',
              title = 'Download',
              description = 'Eksporter data til Excel',
              icon_class = 'download'
            )

          )
        )
      )






      )


  )


}

# ui wrapper
model2UI <- function(
    input,
    output,
    id
) {

  list(
    body = .ui_model2_body(
      input,
      output,
      id
    )
  )

}

# script end;