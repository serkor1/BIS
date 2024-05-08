# script: export
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-08
# objective:
# script start;

as_table_model1 <- function(
    DT
) {

  # 0) sort data by
  # year, sector, and allocator
  DT_list <- lapply(
    X = c("v_qty", "v_cost"),
    FUN = function(value.var) {

      # 1) dcast data
      DT <- data.table::dcast(
        data = DT,
        formula = k_year + k_sector + k_allocator + c_type ~ k_assignment,
        value.var = value.var
      )

      data.table::setnames(
        x = DT,
        old = c("control", "counter_factual", "difference", "treatment"),
        new = paste(
          c("control", "counter_factual", "difference", "treatment"),
          data.table::fifelse(
            test = value.var == "v_qty",
            yes  = "(Forbrug)",
            no   = "(Omkostninger)"
          )
        )
      )


    }
  )

  DT <- do.call(
    merge,
    DT_list
  )

  data.table::setkey(
    x = DT,
    k_sector,
    k_allocator,
    k_year
  )

  DT



}

as_table <- function(
    DT,
    model = 1
) {

  # 0) assert the
  # passed DT class
  assert(
    inherits(DT,"data.table"),
    error_message = c(
      "x" = sprintf(
        fmt = "{.val DT} has to be class {.cls data.table}, got {.cls %s}",
        class (DT)
      )
    )
  )

  DT <- get(
    x = paste0("as_table_model", model)
  )(
    DT = DT
  )


  idx <- which(sapply(
    DT,
    inherits,
    "numeric"
  ))


  DT[
    ,
    (idx) := lapply(
      .SD,
      round,
      digits = 2
    ),
    .SDcols = idx
    ,
  ][]

}


# script end;
