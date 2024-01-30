library(echarts4r)
library(dplyr)

df <- tibble(
  x = factor(5:1),
  y = rev(c(1000, 555, 99, 10, 0)),
  pad = (max(y) - y)/2
)

df |> 
  e_charts(x) |> 
  e_bar(pad, stack = "grp", legend = F, color = 'rgba(0,0,0,0)') %>% # used for offset
  e_bar(y, stack = "grp", legend = F) %>% 
  e_flip_coords() %>% 
  e_x_axis(show = F) %>% 
  e_y_axis(show = F) %>% 
  e_hide_grid_lines() 
  

p <- df |>
  e_charts(x) |>
  e_bar(y) 

p # normal
e_flip_coords(p) # flip


mtcars |>
  tibble::rownames_to_column("model") |> 
  mutate(total = mpg + qsec) |>
  arrange(desc(total)) |>
  e_charts(model) |>
  e_bar(mpg, stack = "grp") |>
  e_bar(qsec, stack = "grp")
