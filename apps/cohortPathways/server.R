server <- function(input, output, session) {
  output$firstTable <- renderReactable(
    reactable(
      app_data[[4]],
      sortable = TRUE,
      showSortable = FALSE,
      highlight = TRUE,
      searchable = TRUE
    )
  )
}