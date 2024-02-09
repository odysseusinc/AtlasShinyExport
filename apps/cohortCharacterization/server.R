library(shiny)
library(reactable)
source("dataPrep.R")



server <- function(input, output, session) {
#  T1 <- list()
#  T2 <- list()
  
  output$secondSelect <- renderUI({
    if (input$cohort == names(cohortNames)[1])
    {
      pickerInput(
        inputId = "analysis",
        label = h4("Analysis name"),
        choices = cohortNames$targetCohort,
        selected = "All prevalence covariates"
      )
    }
    else
    {
      pickerInput(
        inputId = "analysis",
        label = h4("Analysis name"),
        choices = cohortNames$comparatorCohort,
        selected = "Comparison all prevalence covariates"
      )
    }
  })
  
  
  analysisName <- reactive({if (input$cohort == names(cohortNames)[1]) # imput cohort == targetCohort
  {
     return(input$analysis)
  }
  else
  {
    return(input$analysis)
  }
  })
  
  
  output$tables <- renderUI({
    
    resultInputAnalisys <- analysisName() 
    
    if (input$cohort == "targetCohort")
    {
      a <- which(sapply(cohortNames$targetCohort, FUN=function(X) resultInputAnalisys %in% X))
#      lapply(seq_len(length(targetCohort)), function(x) {
        if (!is.null(targetCohort[[a]]$Avg)) {
          T1 <- reactable(
            targetCohort[[a]],
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
                width = 200,
                align = "center"
              )
            )
          )
        }
        else
        {
          T1 <- reactable(
            targetCohort[[a]],
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
#      })
    }
    else {
#      lapply(seq_len(length(targetCohort)), function(x) {
      b <- which(sapply(cohortNames$comparatorCohort, FUN=function(X) resultInputAnalisys %in% X))
        T2 <- reactable(
          comparatorCohort[[b]],
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
#      })
    }
  })
}