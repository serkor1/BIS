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


wb <- openxlsx::createWorkbook(
  title = "Title"
)

sheet_name <- names(DT_list)


lapply(
  sheet_name,
  function(name) {

    openxlsx::addWorksheet(
      wb = wb,
      sheetName = name
    )

    openxlsx::writeDataTable(
      wb = wb,
      x = DT_list[[name]],
      sheet = name
    )

  }
)


openxlsx::saveWorkbook(
  wb = wb,
  file = "playground/wb.xlsx"
)





create_workbook <- function(
    DT,
    f = NULL) {


  if (!is.null(f)) {

    DT_list <- split(
      x = DT,
      f = f
    )

  } else {

    DT_list <- list(
      DT
    )

    names(DT_list) <- "data"

  }


  # 1) create workbook
  # locally
  wb <- openxlsx::createWorkbook()

  # 2) write data
  # while createing sheets
  invisible({
    lapply(
      X = names(DT_list),
      FUN = function(name) {

        # 2.1) add worksheet
        # to the data
        openxlsx::addWorksheet(
          wb = wb,
          sheetName = name
        )

        # 2.2) write as datatable
        # to the work sheet
        openxlsx::writeDataTable(
          wb = wb,
          x = DT_list[[name]],
          sheet = name
        )

      }
    )
  })


  wb


}







# script end;
