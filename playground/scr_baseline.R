# script: scr_baseline
# author: Serkan Korkmaz, serkor1@duck.com
# date: 2024-05-15
# objective: Generate Baseline table
# script start;

rm(list = ls()); invisible(gc()); devtools::load_all()

DT <- data.table::fread(
  "inst/extdata/adhd_characteristica.txt"
)

DT[
  c_socioeconomic %chin% "Udenfor"
  ,
  .(
    v_characteristica = sum(v_weights * v_characteristica, na.rm = TRUE) / sum(v_weights, na.rm = TRUE)
  )
  ,
  by = .(
    k_allocator
  )
]


# script end;
