library(shiny)
library(reactable)


server <- function(input, output, session) {
T1 <- list()
T2 <- list()
# r <- list()
# s <- list()
  
  output$secondSelect <- renderUI({
    if (input$cohort == names(cohortNames)[1])
    {
      pickerInput(
        inputId = "analysis",
        label = h4("Analysis name"),
        choices = cohortNames$targetCohort,
        selected = targetListNames,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    }
    else
    {
      pickerInput(
        inputId = "analysis",
        label = h4("Analysis name"),
        choices = cohortNames$comparatorCohort,
        selected = comparatorListNames,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    }
  })
  
  ######## test 1
  
  # name <- reactiveValues(values = list())
  # 
  # observeEvent(input$analysis, {
  #   name$values <- name$values[input$analysis]
  # })
  # 
  # output$name <- renderText({
  #   name$values
  # })
  
  
  ####### test 2
  
  # name <- reactiveValues(testvalue = reactive({input$analysis}))
  # if (FALSE) {
  #   reactiveValuesToList(name)
  # }
  # 
  # output$test <- renderText({testvalue})
  # 
  # # To get the objects without taking dependencies on them, use isolate().
  # # isolate() can also be used when calling from outside a reactive context (e.g.
  # # at the console)
  # 
  # isolate(reactiveValuesToList(name))
  
  ####### test 3
  
  print("started shiny")
  
  observeEvent(input$analysis,{
    list_of_inputs <<- reactiveValuesToList(
      input)
    print(list_of_inputs)
    cat("input$analysis: ", input$analysis1,"\n")
  })
  
  isolate(input)

  # output$test <- renderText(input$analysis)
  
  analysisName <- reactive({if (input$cohort == names(cohortNames)[1]) # imput cohort == targetCohort
  { 
     return(input$analysis)
  }
  else
  {
    return(input$analysis)
  }
  })
  
  
  # output$test <- renderText(which(for(x in unlist(targetListNames)) {
  #   r[x] <- x %in% list(analysisName())})-1)
  # output$test <- renderText(c(which(sapply(targetListNames, FUN=function(X) analysisName() %in% X))))
  # r <- list()
  
  # for(x in unlist(targetListNames)) {
  #   r[x] <- x %in% list(input$analysis)
  # }
  
  # unlist(targetListNames)
  # names(targetListNames) <-seq(1,9)
  # targetListNames$'1'
  # "All prevalence covariates" %in% targetListNames
  
  l <- reactive({length(sapply(cohortNames$targetCohort, FUN=function(X) analysisName() %in% X)[,1])})
  a <- reactive({sapply(cohortNames$targetCohort, FUN=function(X) analysisName() %in% X)})
  r <- reactive({lapply(seq_len(l()), function(x) {which(a()[x,])})})
  
  m <- reactive({length(sapply(cohortNames$comparatorCohort, FUN=function(X) analysisName() %in% X)[,1])})
  b <- reactive({sapply(cohortNames$comparatorCohort, FUN=function(X) analysisName() %in% X)})
  s <- reactive({lapply(seq_len(m()), function(x) {which(b()[x,])})})
  
  
  output$tables <- renderUI({
    
    # l <- length(which(sapply(cohortNames$targetCohort, FUN=function(X) list_of_input$analysis[,1] %in% X)))
    # a <- sapply(cohortNames$targetCohort, FUN=function(X) list_of_inputs$analysis %in% X)
    # for (x in seq_len(l)) {r[x] <- which(a[x,])}
    
    if (input$cohort == "targetCohort")
    {
      lapply(seq_len(l()), function(x) {
        if (!is.null(targetCohort[[r()[[x]]]]$Avg)) {
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
        }
        else
        {
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
        }
      })
    }
    else {
      lapply(seq_len(l()), function(x) {
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
     })
    }
  })
}