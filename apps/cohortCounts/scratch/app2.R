library(shiny)
library(sigma)

gexf <- here::here("example-data.xml")

ui = shinyUI(fluidPage(
  checkboxInput("drawEdges", "Draw Edges", value = TRUE),
  checkboxInput("drawNodes", "Draw Nodes", value = TRUE),
  sigmaOutput('sigma')
))

server = function(input, output) {
  output$sigma <- renderSigma(
    sigma(gexf, 
          drawEdges = input$drawEdges, 
          drawNodes = input$drawNodes)
  )
}

shinyApp(ui = ui, server = server)
