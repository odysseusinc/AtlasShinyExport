# read in data that was exported from the export button in Atlas Characterization tab

library(dplyr)
library(shiny)
library(readr)

csv_files <- list.files("data", pattern = ".csv", full.names = T) # Not sure why not data/synpuf5pct or data/synpuf100k. 
                                                                  # does not work as is
app_data <- purrr::map(csv_files, ~readr::read_csv(., show_col_types = F) %>%  # Reads all csv files
  mutate(`Covariate short name` = as.character(`Covariate short name`)))  # Changes short name to char

# sapply(app_data, ncol) # continuous variables have extra columns

app_data_categorical <- Filter(function(x) ncol(x) == 11, app_data) %>%  # Take only the first 11 columns of each files. 
                                                                         # The first 11 cols are taken are taken when the function (predicate) ncols is true
  bind_rows()                                                            # Merge all rows from data frames above

app_data_continuous <- Filter(function(x) ncol(x) == 21, app_data) %>%  # IDEM but 21 columns
  bind_rows()                                                           # IDEM

app_data <- purrr::map(csv_files, ~readr::read_csv(., show_col_types = F, col_select = 1:11) %>%   # Ammounts to the same as the first filter
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

