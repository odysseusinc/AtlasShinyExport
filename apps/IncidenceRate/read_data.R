
# function to read in app data from saved json files extracted from Atlas/WebAPI
# `path` argument should point to the folder where the json files are stored

path = "data"
read_data <- function(path) {
  library(dplyr)
  
  # get the list of unique data source keys
  filenames <-  list.files(path, pattern = ".json$") %>% 
    stringr::str_subset("^cohort_definitions.json$", negate = TRUE) 
  
  data_sources <- filenames %>% 
    stringr::str_remove("_targetId.+$") %>% 
    stringr::str_unique()
  
  target_ids <- filenames %>% 
    stringr::str_extract("targetId\\d+") %>%
    stringr::str_remove("targetId") %>% 
    stringr::str_unique() %>% 
    as.integer()
  
  outcome_ids <- filenames %>% 
    stringr::str_extract("outcomeId\\d+") %>%
    stringr::str_remove("outcomeId") %>% 
    stringr::str_unique() %>% 
    as.integer()
  
  # check that all combinations of datasource, target, and outcome have been saved
  
  missing_files <- expand.grid(data_source = data_sources, target = target_ids, outcome = outcome_ids) %>% 
    as_tibble() %>% 
    mutate(filename = glue::glue("{data_source}_targetId{target}_outcomeId{outcome}.json")) %>% 
    pull(filename) %>% 
    setdiff(filenames)
  
  if (length(missing_files) > 0) {
    cli::cli_abort("Missing json files with data! \n{paste(missing_files, collapse = ',\n')}")
  }
  
  df <- expand.grid(data_source = data_sources, target = target_ids, outcome = outcome_ids) %>% 
    as_tibble() %>% 
    mutate(filename = glue::glue("{data_source}_targetId{target}_outcomeId{outcome}.json")) %>% 
    mutate(x = purrr::map(file.path('data', filename), ~jsonlite::read_json(., simplifyVector = FALSE))) %>%
    mutate(subgroup_id = NA_real_, subgroup_name = NA_character_) %>% 
    mutate(total_persons = purrr::map_int(x, ~.[["summary"]]$totalPersons)) %>% 
    mutate(time_at_risk = purrr::map_int(x, ~.[["summary"]]$timeAtRisk)) %>% 
    mutate(cases = purrr::map_int(x, ~.[["summary"]]$cases)) %>% 
    mutate(proportion_per_1k_persons = cases * 1000 / total_persons,
           rate_per_1k_years = cases * 1000 / time_at_risk) %>% 
    mutate(subgroup_df = purrr::map(x, 
                                    ~tibble(x = .$stratifyStats) %>% 
                                      tidyr::unnest_wider(col = x) %>% 
                                      transmute(
                                        subgroup_id = id,
                                        subgroup_name = name,
                                        total_persons = totalPersons, 
                                        time_at_risk = timeAtRisk,
                                        cases = cases,
                                        proportion_per_1k_persons = cases * 1000 / total_persons,
                                        rate_per_1k_years = cases * 1000 / time_at_risk)
    )) 
  
  app_data = list()
  
  app_data[["summary_table"]] <- df %>% 
    select(data_source, target, outcome, total_persons, time_at_risk, cases, proportion_per_1k_persons, rate_per_1k_years)
  
  app_data[["subgroup_table"]] <- df %>% 
    select(data_source, target, outcome, subgroup_df) %>% 
    tidyr::unnest(col = subgroup_df)
  
  app_data[["treemap_table"]] <- df %>%
    select(data_source, target, outcome, x) %>% 
    mutate(treemap_data = purrr::map(x, ~.$treemapData %>% 
                                         jsonlite::fromJSON(simplifyVector = FALSE) %>% 
                                         tidy_treemap_data())) %>% 
    select(-x) %>% 
    tidyr::unnest(treemap_data) %>% 
    rename(total_persons = totalPersons,
           subset_ids = name,
           time_at_risk = timeAtRisk) %>% 
    mutate(proportion_per_1k_persons = cases * 1000 / total_persons,
           rate_per_1k_years = cases * 1000 / time_at_risk)
    
  
  return(app_data)
}


# Helper function to convert the nested list with treemap data into a dataframe
tidy_treemap_data <- function(treemap_data) {
  
  # initialize vectors to hold output
  name <- character()
  size <- numeric()
  cases <- numeric()
  timeAtRisk <- numeric()
  
  # define recursive function that extracts values
  recurse <- function(lst) {
    for (item in lst) {
      if (is.list(item) && "size" %in% names(item)) {
        # the elements we want to extract have names "name" and "size"
        stopifnot("name" %in% names(item)) 
        name <<- c(name, item$name)
        size <<- c(size, item$size)
        cases <<- c(cases, item$cases)
        timeAtRisk <<- c(timeAtRisk, item$timeAtRisk)
      } else if (is.list(item)) {
        recurse(item)
      } else {
        next
      }
    }
  }
  
  # call the recursive function to populate the vectors
  recurse(treemap_data)
  
  # convert name encoding into a comma separated list of subset rule IDs
  name2 <- purrr::map_chr(name, function(encoded_name) {
    stringr::str_split_1(encoded_name, "") %>% 
      as.numeric() %>% 
      {.*seq_along(.)} %>% 
      as.character() %>% 
      stringr::str_subset("0", negate = T) %>% 
      stringr::str_c(collapse = ",") %>% 
      {ifelse(. == "", "None", .)}
  })
  
  # return a dataframe with the counts of each partition
  return(dplyr::tibble(name = name2, totalPersons = size, cases = cases, timeAtRisk = timeAtRisk))
}
