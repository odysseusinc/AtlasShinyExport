library(shiny)
library(dplyr)
library(reactable)
library(dplyr)
source("dataPrep.R")


server <- function(input, output, session) {
  cohort <- reactive({
    input$cohort
  })
  
  dataSet <- eventReactive(input$Submit, {
    if (cohort() == "targetCohort") {
      return(targetCohort)
    }
    else {
      return(comparatorCohort)
    }
  })
  
  # cohort <- reactive({
  #   cohort <- input$cohort})
  #
  # if (reactive({cohort() == "targetCohort"})) {
  #   dataSet <- targetCohort
  # }
  # else {
  #   dataSet <- comparatorCohort
  # }
  
  ### Start Debugging Code.
  
  output$oid2 <- renderPrint({
    if (cohort() == "tagetCohort") {
      paste("You entered: ", cohort())
    } else {
      paste("You entered: ", cohort())
    }
  })
  
  ### End Debugging Code.
  
  dataList = list()
  
  renderedDataList <-
    renderReactable({for(a in 1:length(dataSet())) {
          if (is.null(dataSet()[[a]]$Avg)) {
            dataList[[paste0("table ", a)]] <-
              reactable(dataSet()[[a]],
              sortable = TRUE,
              showSortable = TRUE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15,
              style = list(maxWidth = 1600, maxHeight = 900)
            )
          }
          else {
            dataList[[paste0("table ", a)]] <-
              reactable(dataSet()[[a]],
              sortable = TRUE,
              showSortable = FALSE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15,
              style = list(maxWidth = 1600, maxHeight = 900),
              columns = list(Boxplot = colDef(
                cell = function(a) {
                  div(class = "plot",
                      img(src = sprintf("p%s.png", a)))
                },
                width = 120,
                align = "center"
              ))
            )
          }
        }
    })

# nameList <-
#   lapply(seq_len(length(renderedDataList())), function(a) {
#     paste0("table ", a)
#   })

output$tables <- renderUI({
  lapply(seq_len(length(renderedDataList())), function(a) {
    reactableOutput(paste0("table ", a))
  })
})
}