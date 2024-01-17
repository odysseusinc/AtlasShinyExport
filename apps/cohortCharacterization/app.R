# read in data that was exported from the export button in Atlas Characterization tab

library(dplyr)
library(shiny)
library(readr)
library(reactable)

csv_files <-
  list.files("data/synpuf5pct", pattern = ".csv", full.names = T)
app_data <-
  purrr::map(
    csv_files,
    ~ readr::read_csv(., show_col_types = F) %>%  # Reads all csv files
      mutate(`Covariate short name` = as.character(`Covariate short name`))
  )  # Changes short name to char

app_data_categorical <-
  Filter(function(x)
    ncol(x) == 11, app_data) %>%  # Take only the first 11 columns of each files.
  # The first 11 cols are taken are taken when the function (predicate) ncols is true
  bind_rows()                                                            # Merge all rows from data frames above

app_data_continuous <-
  Filter(function(x)
    ncol(x) == 21, app_data) %>%  # IDEM but 21 columns
  bind_rows()                                                           # IDEM

app_data <-
  purrr::map(
    csv_files,
    ~ readr::read_csv(., show_col_types = F, col_select = 1:11) %>%   # Seems to amount to the same as the first filter
      # but that is not the case because here we take
      # everything even when the analys are not applicable
      # for the type of variable
      mutate(`Covariate short name` = as.character(`Covariate short name`))
  ) %>%
  bind_rows()

# ui <- fluidPage(
#   titlePanel("Initial Example"),
#   reactableOutput("table")
# )

ui <- navbarPage(
  title = div("Cohort Characterizations"),
  tabPanel("Categorical Data",
           reactableOutput(
             "table_categorical"
           )),
  tabPanel("Continuous Data",
           reactableOutput("table_continuous")),
  fluid = TRUE,
)

server <- function(input, output) {
  output$table_categorical <- renderUI
  ({
    reactable(
      app_data_categorical,
      groupBy = c(
        "Analysis name",
        "Strata name",
        "Cohort name",
        "Covariate short name"
      )
    )
  })
  
  output$table_continuous <- renderUI
  ({
    reactable(
      app_data_continuous,
      groupBy = c(
        "Analysis name",
        "Strata name",
        "Cohort name",
        "Covariate short name"
      )
    )
  })
}

shinyApp(ui = ui, server = server)
