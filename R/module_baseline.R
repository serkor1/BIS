# script: module_baseline
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-22
# objective: This is the baselinetable module
# for model 1 and model 2
# script start;

module_baseline_model1 <- function(
    id
) {
  shiny::moduleServer(id, function(input, output, session){

    # 0) set namespace
    ns <- session$ns

    # 1) determine recipe
    # NOTE: should be moved outside to a different
    # server at a later point
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
            k_disease = data.table::fifelse(
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

    # 2) Extract the data
    # and from the SQL-server
    sql_data <- shiny::reactive(
      x = {

        DT <- extract_data(
          DB_connection = DB_connection,
          table         = "model1_baseline",
          k_disease     = c(
            input$treatment_disease,
            input$control_disease
          ),
          c_type        = input$c_type
        )

        # NOTE: if general population
        # is chosen then it wont return anything
        # for the control
        # it is in a different table
        if (grepl(pattern = "befolkning", x = input$control_disease,ignore.case = TRUE)) {


          DT_pop <- extract_data(
            DB_connection = DB_connection,
            table         = "model1_baseline_population",
            k_disease     = c(
              input$treatment_disease
            ),
            c_type        = input$c_type
          )

          DT_pop[
            ,
            k_disease := paste0("pop_", k_disease)
            ,
          ]

          DT <- rbind(
            DT,
            DT_pop
          )

        }

        DT


      }
    )

    output$baseline <- DT::renderDT({

      req(input$start)

      # 1) extract data
      DT <- prepare_data(
        DT = sql_data(),
        recipe = get_recipe()
      )

      # DT <- aggregate_data(
      #   DT = DT,
      #   calc = expression(
      #     .(
      #       v_characteristics = round(
      #         weighted.mean(
      #           x     = v_characteristics,
      #           w     = v_weights,
      #           na.rm = TRUE
      #         ),
      #         digits = 2
      #       ),
      #       v_obs = sum(
      #         unique(
      #           v_obs
      #         ),
      #         na.rm = TRUE
      #       )
      #     )
      #   ),
      #   by =  c(
      #     "k_assignment",
      #     "k_allocator",
      #     "c_group"
      #   )
      #
      # )
      #
      # DT[
      #   !(k_allocator %chin% "Alder")
      #   ,
      #   v_characteristics := paste0(
      #     v_characteristics * 100
      #   )
      #   ,
      # ][
      #   ,
      #   v_characteristics := data.table::fifelse(
      #     k_allocator %chin% "Alder",
      #     paste0(
      #       v_characteristics," (N=", v_obs,")"
      #     ),paste0(
      #       v_characteristics,"% (N=", v_obs,")"
      #     )
      #   )
      #   ,
      # ]



      total_obs <- aggregate_data(
        unique(DT[,.(v_obs = sum((v_obs))), by = .(k_allocator, k_assignment)])
        ,
        calc = expression(
          .(
            c_group           = as.character(shiny::span(bsicons::bs_icon(name = "person-raised-hand"), "Antal")),
            k_allocator       = "Total",
            v_characteristics = unique(v_obs)
          )
        ),
        by = c(
          "k_assignment"
        )
      )

      grouped_obs <- data.table::rbindlist(
        lapply(
          grep("^c", colnames(DT), value = TRUE),
          function(x) {

            aggregate_data(
              DT,
              calc = expression(
                .(
                  v_obs = sum(unique(v_obs))
                )
              ),
              by = c(
                "k_assignment",
                "c_group",
                "k_allocator" = x
              )
            )

          }
        )
      )

      grouped_vals <- aggregate_data(
        DT = DT,
        calc = expression(
          .(
            v_characteristics = round(
              weighted.mean(
                x     = v_characteristics,
                w     = v_weights,
                na.rm = TRUE
              ),
              digits = 2
            )
          )
        ),
        by =  c(
          "k_assignment",
          "k_allocator",
          "c_group"
        )

      )


      DT_ <- merge(
        grouped_vals,
        grouped_obs,
        by = c("k_assignment", "k_allocator", "c_group"),
        all.x = TRUE
      )


      DT_[
        !(k_allocator %chin% "Alder")
        ,
        v_characteristics := paste0(
          v_characteristics * 100
        )
        ,
      ][
        ,
        v_characteristics := data.table::fifelse(
          k_allocator %chin% "Alder",
          formatC(
            v_characteristics,
            digits = 2
          ),
          paste0(
            v_characteristics,"% (N = ", data.table::fifelse(is.na(v_obs),"0", as.character(v_obs)), ")"
          )
        )
        ,
      ][
        ,
        `:=`(
          v_obs = NULL
        )
        ,
      ]

      DT <- data.table::rbindlist(
        list(
          total_obs,
          DT_
        ),
        fill = TRUE
      )

      DT <- data.table::dcast(
        data = DT,
        formula = k_allocator + c_group ~ k_assignment,
        value.var = "v_characteristics"
      )





      data.table::setnames(
        DT,
        old = c("k_allocator", "treatment", "control", "difference"),
        new = c("Karakteristika", "Valgt Gruppe", "Sammenligningsgruppe", "Forskel"),
        skip_absent = TRUE
      )

      data.table::setcolorder(
        x = DT,
        neworder = c("Karakteristika", "Valgt Gruppe", "Sammenligningsgruppe", "Forskel")
      )

      data.table::setorder(
        DT,
        -c_group
      )

      generate_table(
        header = NULL,
        DT = DT
      )


    }
    )


  }
  )
}

# script end;
