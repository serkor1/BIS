# script: export
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-08
# objective:
# script start;

as_table_model1 <- function(
    DT,
    treatment_disease,
    control_disease,
    effect_data
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

      data.table::setcolorder(
        x = DT,
        neworder = c(
          "k_year", "k_sector", "k_allocator", "c_type",
          "treatment",
          "control",
          "difference",
          "counter_factual"
        )
      )

      data.table::setnames(
        x = DT,
        old = c("control", "counter_factual", "difference", "treatment"),
        new = paste(
          c(
            paste0("Sammenligningsgruppe: ", control_disease),
            "Kontrafaktisk [Valgt Gruppe]",
            "Forskel",
            paste0("Valgt Gruppe: ", treatment_disease)

            ),
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




  DT <- merge(
          x = DT,
          y = effect_data,
          by = "k_year",
          all.x = TRUE
        )

  data.table::setkey(
    x = DT,
    k_sector,
    k_allocator,
    k_year
  )

  data.table::setnames(
    x = DT,
    skip_absent = TRUE,
    old = c(
      "k_year",
      "k_allocator",
      "c_type",
      "effect"
    ),
    new = c(
      "Tid",
      "Byrdemål",
      "Patienttype",
      "Effekt (%)")
  )



  DT



}

as_table <- function(
    DT,
    treatment_disease = NULL,
    control_disease = NULL,
    model = 1,
    effect_data = NULL) {

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
    DT = DT,
    treatment_disease = treatment_disease,
    control_disease = control_disease,
    effect_data = effect_data
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
