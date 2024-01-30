library(dplyr)
library(echarts4r)
df <- tibble(
  name = c("earth", "mars", "venus"),
  value = c(30, 40, 30),
  itemStyle = tibble(color = c(NA, "red", "blue")),
)

df |>
  e_charts() |>
  e_treemap(animation = F) %>% 
  e_graphic_g("elements" = list(onclick = "function(params) {
  // Print name in console
  console.log(params.name);"))


series: [
  {
    roam: 'move',
    nodeClick: false,
    ...
  }
]