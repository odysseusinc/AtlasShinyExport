ui <- fluidPage(
  
  fluidRow(column(12, reactableOutput("firstTable"))),
  
  fluidRow(column(12, 
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = designations,
      color = c("blue", "blue", "blue", "blue", "blue", "blue"),
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    
    link = list(
      source = c(0,1,0,2,3,3),
      target = c(2,3,3,4,4,5),
      
      value =  c(8,4,2,8,4,2)
      
    )
    
  )
  )
  )
)