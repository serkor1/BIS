rm(list = ls()); gc(); devtools::load_all()


layout(
  plot(
    data = mtcars,
    x = setNames(
      object = "hp",
      nm = "Horsepower"
    ),
    y = setNames(
      object = "mpg",
      nm = "Miles per Gallon"
    ),
    color = as.formula(
      paste0("~", "as.factor(am)")
    )
  ),
  title = "long ass title",
  dark = FALSE
)

