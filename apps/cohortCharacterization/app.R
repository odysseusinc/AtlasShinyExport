# read in data that was exported from the export button in Atlas Characterization tab

library(shiny)
library(dplyr)
library(reactable)
source("dataPrep.R")


ui <- fluidPage(
  titlePanel("Cohort Characterizations"),
  uiOutput("mytabs")
)

server <- function(input, output) {
  
  output$mytabs <- renderUI({
    thetabs <- lapply(paste0("table ", seq_len(length(app_data))),
                      function(x) {
                        tabPanel(x, reactableOutput(x))
                      })
    do.call(tabsetPanel, thetabs)
  })
  
  observe({
    lapply(seq_len(length(app_data)), function(x) {
      output[[paste0("table ", x)]] <- renderReactable({reactable(app_data[[x]])})
    })
  })
  
}

shinyApp(ui = ui, server = server)
