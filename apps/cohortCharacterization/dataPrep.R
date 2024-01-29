library(dplyr)
library(st)

csv_files <-
  list.files("data", pattern = ".csv", full.names = T)

app_data <-
  purrr::map(csv_files,
    ~ data.table::fread(., data.table = T) %>%     # Reads all csv files
      mutate(`Covariate short name` = as.character(`Covariate short name`)) %>%
      mutate(`Covariate short name` = tolower(`Covariate short name`)) %>%
      mutate(`Covariate short name` = gsub("^ ", "", `Covariate short name`)) %>%
      mutate(`Covariate short name` = DescTools::StrCap(`Covariate short name`)) %>%
      mutate(`Analysis name` = gsub('([a-z])([A-Z])', '\\1 \\2', `Analysis name`, perl = TRUE)) %>%
      mutate(Percent = tryCatch(round(Percent, digits = 1), error = function(z) return(NA))) %>% # rounding continuous variables where they exists
      mutate(Avg = tryCatch(round(Avg, digits = 1), error = function(z) return(NA))) %>%
      mutate(StdDev = tryCatch(round(StdDev, digits = 1), error = function(z) return(NA))) %>%
      mutate(`Target percent` = tryCatch(round(`Target percent`, digits = 1), error = function(z) return(NA))) %>%
      mutate(`Cohort percent` = tryCatch(round(`Cohort percent`, digits = 1), error = function(z) return(NA))) %>%
      mutate(`Std. Diff Of Mean` = tryCatch(round(`Std. Diff Of Mean`, digits = 1), error = function(z) return(NA))) %>%
      select(-contains("Value field")) %>%         # Eliminating this columns here 
      select(-contains("Missing Means Zero")) %>%  # because it was not possible to 
      select(-contains("Analysis ID")) %>%         # do inside the Shiny App
      select(-contains("Strata ID")) %>%
      select(-contains("Cohort ID")) %>%
      select(-contains("Covariate ID")) %>%
      select(-contains("Covariate name"))
  )

empty_columns <- lapply(seq_len(length(app_data)), function(x) colSums(is.na(app_data[[x]]) | app_data[[x]] == "") == nrow(is.na(app_data[[x]]))) # Delete all empty columns

app_data <- lapply(seq_len(length(app_data)), function(x) app_data[[x]] %>% purrr::discard(empty_columns[[x]]))
  
  
# z <- lapply(paste0("table ", seq_len(length(app_data))),
#             function(x) {
#               print(x)
#               print(paste0("plot ", x))
#             })
