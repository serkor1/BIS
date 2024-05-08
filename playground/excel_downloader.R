# script: Exceldownloader
# date: 2024-04-22
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create an excel downloader
# script start;

# gc()
rm(list = ls()); invisible(gc()); devtools::load_all();


# 1) extract all data
DT <- extract_data(
  DB_connection = DB_connection,
  table = "model1",
  k_disease = c(model1_parameters$k_disease[1], model1_parameters$k_disease[2])
)


DT <- prepare_data(
  DT = DT,
  recipe = recipe(
    treatment = list(
      k_disease = model1_parameters$k_disease[1]
    ),
    control = list(
      k_disease = model1_parameters$k_disease[2]
    )
  )
)


DT <- aggregate_data(
  DT = DT,
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

DT_list <- split(
  DT,
  DT$k_sector
)

DT_list <- lapply(
  X = DT_list,
  function(DT) {

    effect_data(
      DT = DT,
      effect = data.table::data.table(
        effect = c(
          0,
          0,
          0,
          50/100,
          50/100,
          50/100,
          50/100,
          50/100

        ),
        k_year = -2:5
      )
    )

  }
)



as_table(
  DT = DT_list[[1]][c_type %chin% 'Incident']
)




# script end;
