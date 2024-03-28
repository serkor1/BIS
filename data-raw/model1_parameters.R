## code to prepare `model1_parameters` dataset goes here


test_DT <- data.table::data.table(
  variable = rep(LETTERS[1:3],3),
  value    = rep(LETTERS[1:3],3),
  label    = rep(LETTERS[1:3],3)
)


model1_parameters <- input_parameters(
  DT = test_DT,as_list = TRUE
)


usethis::use_data(
  model1_parameters,
  overwrite = TRUE
  )
