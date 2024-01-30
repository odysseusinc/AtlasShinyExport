df <- data.frame(
  name = c("earth", "mars", "venus"),
  value = c(30, 40, 30)
)

df |>
  e_charts() |>
  e_treemap() |>
  e_on(query = ".", handler = "function(params) {alert(params.name)}")
