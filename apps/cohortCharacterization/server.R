library(shiny)
library(reactable)
source("dataPrep.R")


server <- function(input, output, session) {
  T1 <- list()
  T2 <- list()
  
  
  output$tables <- renderUI({
    if (input$cohort == "targetCohort")
    {
      lapply(seq_len(length(targetCohort)), function(x) {
        if (!is.null(targetCohort[[x]]$Avg)) {
          T1[[x]] <- reactable(
            targetCohort[[x]],
            sortable = TRUE,
            showSortable = FALSE,
            highlight = TRUE,
            searchable = TRUE,
            theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 15, 20),
            defaultPageSize = 15,
            style = list(maxWidth = 1600, maxHeight = 900),
            columns = list(
              Boxplot = colDef(
                cell = function(a) {
                  div(class = "plot",
                      img(src = sprintf("p%s.png", a)))
                },
                width = 190,
                align = "center"
              )
            )
          )
        }
        else
        {
          T1[[x]] <- reactable(
            targetCohort[[x]],
            sortable = TRUE,
            showSortable = TRUE,
            highlight = TRUE,
            searchable = TRUE,
            theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 15, 20),
            defaultPageSize = 15,
            style = list(maxWidth = 1600, maxHeight = 900)
          )
        }
      })
    }
    else {
      lapply(seq_len(length(targetCohort)), function(x) {
        T2[[x]] <- reactable(
          comparatorCohort[[x]],
          sortable = TRUE,
          showSortable = TRUE,
          highlight = TRUE,
          searchable = TRUE,
          theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15,
          style = list(maxWidth = 1600, maxHeight = 900)
        )
      })
    }
  })
}