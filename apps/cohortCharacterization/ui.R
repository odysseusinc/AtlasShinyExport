ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Cohort Charterizations"),
  
  fluidRow(column(
    3,
    pickerInput(
      inputId = "cohort",
      label = h4("Cohort name"),
      choices = c("Target cohort" = "targetCohort",
                  "Compare target vs comparator cohort" = "comparatorCohort"),
      selected = "Target cohort"),
  ),
  column(
    3,
    uiOutput("secondSelect")
  )),
  
  # fluidRow(column(12, textOutput("test"))),
  
  fluidRow(column(12, uiOutput("tables"))),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
)