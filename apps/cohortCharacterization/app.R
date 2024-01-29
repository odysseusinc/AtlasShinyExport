# read in data that was exported from the export button in Atlas Characterization tab

library(shiny)
library(shinythemes)
library(dplyr)
library(reactable)
# library(ggplot2)
source("dataPrep.R")


ui <- fluidPage(theme = shinytheme("flatly"),
                fluidRow(navbarPage("Cohort Characterizations", 
                                    tabPanel(uiOutput("mytabs")),
                                    div(style = "height:200px")))
)

server <- function(input, output) {
  output$mytabs <- renderUI({
    thetabs <- lapply(paste0("table ", seq_len(length(app_data))),
                      function(x) {tabPanel(x, reactableOutput(x) #,
                                 # column(1, plotOutput(paste0("plot ", x))),
                                 # fullWidth = FALSE
                                 )
                      })
    do.call(tabsetPanel, thetabs)
  })
  
  observe({
    lapply(seq_len(length(app_data)), function(a) {
        output[[paste0("table ", a)]] <-
          renderReactable({
            reactable(
              app_data[[a]],
              sortable = TRUE,
              showSortable = TRUE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15
            )
          })
        # if (!is.null(app_data[[a]]$Avg)) {
        #   output[[paste0("table plot ", a)]] <- renderPlot({p})
        #     p <- ggplot(app_data[[2]], aes(x = `Analysis name`, fill = `Analysis name`)) +
        #     geom_boxplot(aes(
        #       ymin = Min,
        #       lower = P10,
        #       middle = Median,
        #       upper = P90,
        #       ymax = Max
        #     ),
        #     stat = "identity") 
        #   
        #   p + coord_flip() 
        #   p + scale_fill_brewer(palette = "Blues")
        #   p + theme(legend.title = element_blank(),
        #           axis.title.y = element_blank(),
        #           axis.text.y = element_blank(),
        #           axis.ticks = element_blank())
        # }
        # else {
        #   return()
        # }
      }
    )
  })
}

shinyApp(ui = ui, server = server)
