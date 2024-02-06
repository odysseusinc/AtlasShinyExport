library(shiny)
library(reactable)
library(htmltools)
library(shinythemes)
library(shinyWidgets)


ui <- fluidPage(
    titlePanel("Cohort Charterizations"),
    
    fluidRow(column(3, pickerInput(inputId = "cohort", 
                                   label = h3("Cohort"),
                                   choices = c("Target cohort" = "targetCohort", 
                                               "Comparator cohort" = "comparatorCohort"),
                                   selected = "Target cohort"))),
    
    fluidRow(column(2, actionButton("Submit", "Submit"))),
    
    verbatimTextOutput("oid2"),
    
    fluidRow(column(3,textOutput("selected_var"))),
    
    fluidRow(column(12, uiOutput("tables"))),
    
    # fluidRow(column(12, tableOutput("dataset"))),
  )

# ui <- fluidPage(theme = shinytheme("flatly"), renderUI(uiOutput("myRows"))
#                 )
