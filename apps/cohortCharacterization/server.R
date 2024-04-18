server <- function(input, output, session) {
  T1 <- list()
  T2 <- list()
  
  output$secondSelect <- renderUI({
      pickerInput(
        inputId = "analysis",
        label = "Analysis name",
        choices = c(cohortNames$targetCohort),
        selected = c(cohortNames$targetCohort),
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE
        )
      )
    })
  output$thirdSelect <- renderUI({
      pickerInput(
        inputId = "analysis",
        label = "Analysis name",
        choices = c(cohortNames$comparatorCohort),
        selected = c(cohortNames$comparatorCohort),
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE
        )
      )
    })
  
  a <-
    reactive({
      sapply(
        cohortNames$targetCohort,
        FUN = function(X)
          input$analysis %in% X
      )
    })
  l <-
    reactive({
      if (is.null(dim(a()))) {
        return(1)
      } else {
        length(a()[, 1])
      }
    })
  r <-
    reactive({
      lapply(seq_len(l()), function(x) {
        if (is.null(dim(a()))) {
          which(a())
        } else {
          which(a()[x, ])
        }
      })
    })
  
  b <-
    reactive({
      sapply(
        cohortNames$comparatorCohort,
        FUN = function(X)
          input$analysis %in% X
      )
    })
  m <-
    reactive({
      if (is.null(dim(b()))) {
        return(1)
      } else {
        length(b()[, 1])
      }
    })
  s <-
    reactive({
      lapply(seq_len(m()), function(x) {
        if (is.null(dim(b()))) {
          which(b())
        } else {
          which(b()[x, ])
        }
      })
    })
  
  
  output$tables <- renderUI({
    if (input$cohort == "targetCohort")
    {
      lapply(seq_len(l()), function(x) {
        if (!is.null(targetCohort[[r()[[x]]]]$Avg)) {
          tags$div(
            class = "header",
            checked = NA,
            tags$h4(cohortNames$targetCohort[[r()[[x]]]], align = 'right'),
            tags$hr(style = "border-top: 1px solid #000000;"),
            T1[[x]] <- reactable(
              targetCohort[[r()[[x]]]],
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
                  cell = function(x) {
                    div(class = "plot",
                        img(src = sprintf("p%s.png", x)))
                  },
                  width = 200,
                  align = "center"
                )
              )
            )
          )
        }
        else
        {
          tags$div(
            class = "header",
            checked = NA,
            tags$h4(cohortNames$targetCohort[[r()[[x]]]], align = 'right'),
            tags$hr(style = "border-top: 1px solid #000000;"),
            T1[[x]] <- reactable(
              targetCohort[[r()[[x]]]],
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
          )
        }
      })
    }
    else {
      lapply(seq_len(l()), function(x) {
        tags$div(
          class = "header",
          checked = NA,
          tags$h4(cohortNames$comparatorCohort[[s()[[x]]]], align = 'right'),
          tags$hr(style = "border-top: 1px solid #000000;"),
          T2[[x]] <- reactable(
            comparatorCohort[[s()[[x]]]],
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
        )
      })
    }
  })
}