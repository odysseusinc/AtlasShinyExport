library(shiny)
library(htmltools)
library(shinythemes)
library(shinyWidgets)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Cohort Charterizations"),
  
  fluidRow(column(
    3,
    pickerInput(
      inputId = "cohort",
      label = h3("Cohort"),
      choices = c("Target cohort" = "targetCohort",
                  "Comparator cohort" = "comparatorCohort"),
      selected = "Target cohort"
    )
  )),
  
  fluidRow(column(12, uiOutput("tables"))),
)