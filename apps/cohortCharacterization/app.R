# read in data that was exported from the export button in Atlas Characterization tab

library(dplyr)
library(shiny)
library(readr)

csv_files <- list.files("data", pattern = ".csv", full.names = T)
app_data <- purrr::map(csv_files, ~readr::read_csv(., show_col_types = F) %>% 
  mutate(`Covariate short name` = as.character(`Covariate short name`)))

# sapply(app_data, ncol) # continuous variables have extra columns

app_data_categorical <- Filter(function(x) ncol(x) == 11, app_data) %>% 
  bind_rows()

app_data_continuous <- Filter(function(x) ncol(x) == 21, app_data) %>% 
  bind_rows()

app_data <- purrr::map(csv_files, ~readr::read_csv(., show_col_types = F, col_select = 1:11) %>% 
                         mutate(`Covariate short name` = as.character(`Covariate short name`))) %>% 
  bind_rows()

atlas_link <- readr::read_lines(file.path("data", "atlas_link.txt"))

cohorts <- app_data %>% 
  pull(`Cohort name`) %>% 
  unique()

stopifnot(length(cohorts) >= 1)


ui <- fluidPage(
  
)

server <- function(input, output) {
 
}

shinyApp(ui = ui, server = server)

