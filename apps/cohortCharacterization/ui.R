library(shiny)
library(reactable)
library(htmltools)
library(shinythemes)
library(bit64)


ui <- fluidPage(theme = shinytheme("flatly"),
                fluidRow(navbarPage("Cohort Characterizations",
                                    tabPanel(uiOutput("mytabs")
                                    )))
)
