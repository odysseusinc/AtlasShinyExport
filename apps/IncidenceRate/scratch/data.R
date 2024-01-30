
# create data for one
x <- jsonlite::read_json("data/SYNPUF_110k_by_event.json")
# 2 <- jsonlite::read_json("data/SYNPUF_110k_by_person.json")


summary_table <- dplyr::tibble(total_index_events = x$summary$baseCount,
                               final_index_events = x$summary$finalCount,
                               percent_included = x$summary$percentMatched)

# 612861 total events
# inclusion_table <- 
tidyr::unnest_wider(dplyr::tibble(value = x$inclusionRuleStats), col = value) %>% 
  gt::gt() %>% 
  gt::fmt_number(countSatisfying, decimals = 0)
  

tidyTreemapData <- function(treemapData) {

  # initialize vectors to hold output
  name <- character()
  size <- numeric()

  # define recursive function that extracts values
  recurse <- function(lst) {
    for (item in lst) {
      if (is.list(item) && "size" %in% names(item)) {
        # the elements we want to extract have names "name" and "size"
        stopifnot("name" %in% names(item)) 
        name <<- c(name, item$name)
        size <<- c(size, item$size)
      } else if (is.list(item)) {
        recurse(item)
      } else {
        next
      }
    }
  }
  
  # call the recursive function to populate the vectors
  recurse(treemapData)
  
  # return a dataframe with the counts of each partition
  return(dplyr::tibble(name, value = size))
}

df <- tidyTreemapData(treemapData)


df

library(dplyr)
library(echarts4r)
# df <- tibble(
#   name = c("earth", "mars", "venus"),
#   value = c(30, 40, 30),
#   itemStyle = tibble(color = c(NA, "red", "blue")),
# )



df |>
  e_charts() |>
  e_treemap(animation = F) %>% 
  e_graphic_g("elements" = list(onclick = "function(params) {console.log(params.name);}",
                                onmouseover = "function(params) {console.log('adsfaf');}"))




