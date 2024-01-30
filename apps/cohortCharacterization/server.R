library(shiny)
library(dplyr)
library(reactable)
library(ggplot2)
library(htmltools)
library(dplyr)
library(st)
library(bit64)
source("dataPrep.R")

server <- function(input, output) {
  output$mytabs <- renderUI({
    thetabs <- lapply(paste0("table ", seq_len(length(app_data))),
                      function(x) {
                        tabPanel(x, reactableOutput(x))
                      })
    do.call(tabsetPanel, thetabs)
  })
  
  output$myplots <- renderUI({
    thetabs1 <- lapply(paste0("plot ", seq_len(length(app_data))),
                       function(x) {
                         if (!is.null(app_data[[a]]$Avg)) {
                           tabPanel(x, plotOutput(x))
                         }
                         else{
                           return()
                         }
                       })
    do.call(tabsetPanel1, thetabs1)
  })
  
  observe({
    lapply(seq_len(length(app_data)), function(a) {
      if (is.null(app_data[[a]]$Avg)) {
        output[[paste0("table ", a)]] <-
          renderReactable({
            reactable(
              app_data[[a]],
              sortable = TRUE,
              showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = "Percent",
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15,
              style = list(maxWidth = 1600, maxHeight = 920))
          })}
      else {
        output[[paste0("table ", a)]] <-
          renderReactable({
            reactable(
              app_data[[a]],
              sortable = TRUE,
              showSortable = FALSE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15,
              style = list(maxWidth = 1600, maxHeight = 800),
              columns = list(
                Boxplot = colDef(cell = function(a) {
                  div(
                    class = "plot",
                    img(src = sprintf("p%s.png", a))
                  )},
                  width = 120,
                  align = "center")))
          })
      }
    })
  })
}

#class = "box-plot", alt = paste0("p", a),