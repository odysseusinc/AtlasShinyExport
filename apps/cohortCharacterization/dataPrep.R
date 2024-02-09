library(dplyr)
library(st)
library(ggplot2)

csv_files <-
  list.files("data", pattern = ".csv", full.names = T)

app_data <-
  purrr::map(
    csv_files,
    ~ data.table::fread(., data.table = T) %>%     # Reads all csv files
      mutate(`Covariate short name` = as.character(`Covariate short name`)) %>%
      mutate(`Covariate short name` = tolower(`Covariate short name`)) %>%
      mutate(`Covariate short name` = gsub("^ ", "", `Covariate short name`)) %>%
      mutate(`Covariate short name` = DescTools::StrCap(`Covariate short name`)) %>%
      mutate(
        `Analysis name` = gsub('([a-z])([A-Z])', '\\1 \\2', `Analysis name`, perl = TRUE)
      ) %>%
      mutate(Percent = tryCatch(
        round(Percent, digits = 1),
        error = function(z)
          return(NA)
      )) %>% # rounding continuous variables where they exists
      mutate(Avg = tryCatch(
        round(Avg, digits = 1),
        error = function(z)
          return(NA)
      )) %>%
      mutate(StdDev = tryCatch(
        round(StdDev, digits = 1),
        error = function(z)
          return(NA)
      )) %>%
      mutate(`Target percent` = tryCatch(
        round(`Target percent`, digits = 1),
        error = function(z)
          return(NA)
      )) %>%
      mutate(`Cohort percent` = tryCatch(
        round(`Cohort percent`, digits = 1),
        error = function(z)
          return(NA)
      )) %>%
      mutate(`Std. Diff Of Mean` = tryCatch(
        round(`Std. Diff Of Mean`, digits = 1),
        error = function(z)
          return(NA)
      )) %>%
      select(-contains("Value field")) %>%         # Eliminating this columns here
      select(-contains("Missing Means Zero")) %>%  # because it was not possible to
      select(-contains("Analysis ID")) %>%         # do inside the Shiny App
      select(-contains("Strata ID")) %>%
      select(-contains("Cohort ID")) %>%
      select(-contains("Covariate ID")) %>%
      select(-contains("Covariate name"))
  )

empty_columns <-
  lapply(seq_len(length(app_data)), function(x)
    colSums(is.na(app_data[[x]]) |
              app_data[[x]] == "") == nrow(is.na(app_data[[x]]))) # Delete all empty columns

app_data <-
  lapply(seq_len(length(app_data)), function(x)
    app_data[[x]] %>% purrr::discard(empty_columns[[x]]))

app_data <-
  lapply(seq_len(length(app_data)), function(x)
    if (!is.null(app_data[[x]]$Avg)) {
      app_data[[x]] %>% mutate(`Boxplot` =  x)
    } else {
      app_data[[x]]
    }) # Insert plot columns where relevant

# Graphs for the app

lapply(seq_len(length(app_data)), function(a) {
  if (!is.null(app_data[[a]]$Avg)) {
    output <-
      ggplot(app_data[[a]],
             aes(x = `Analysis name`, fill = `Analysis name`)) +
      geom_boxplot(
        aes(
          ymin = Min,
          lower = P10,
          middle = Median,
          upper = P90,
          ymax = Max,
        ),
        alpha = 0,
        colour = "#0c439c",
        stat = "identity",
        show.legend = FALSE
      ) +
      coord_flip() +
      # scale_color_brewer(palette="Blues") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
    
    png(sprintf("www/p%s.png", a),
        width = 180,
        height = 80)
    plot(output)
    dev.off()
  }
  else {
    return()
  }
})


comparatorCohort <- list()
targetCohort <- list()

for (a in 1:length(app_data)) {
  if (!is.null(app_data[[a]]$`Comparator cohort name`)) {
    comparatorCohort[[a]] <- app_data[[a]]
  }
  else {
    targetCohort[[a]] <- app_data[[a]]
  }
}

comparatorCohort <-
  Filter(Negate(is.null), comparatorCohort) # Filter nulls from list

inputFilesNames <- list(csv_files)
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    sub('^data/Export ', '', x)
  }) # eliminating data/Export from the beggining
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    sub('.csv$', '', x)
  }) # eliminating .csv from the end
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    gsub('[()]', '', x, perl = TRUE)
  }) # eliminating parentheses
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    gsub('([a-z])([A-Z])', '\\1 \\2', x, perl = TRUE)
  }) # Adding spaces between words
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    tolower(x)
  }) # Everything lower case
inputFilesNames <-
  lapply(inputFilesNames, function(x) {
    stringr::str_to_sentence(x)
  }) # First letter in upper case

targetListNames <- list()
comparatorListNames <- list()
inputListNames <- list()

for (x in 1:(length(inputFilesNames[[1]])))
{
  inputListNames[[x]] <- inputFilesNames[[1]][x]
}

for (x in 1:(length(inputFilesNames[[1]]) / 2))
{
  targetListNames[[x]] <- inputFilesNames[[1]][x]
}

for (y in ((length(inputFilesNames[[1]]) / 2) + 1):(length(inputFilesNames[[1]])))
{
  comparatorListNames[[y]] <- inputFilesNames[[1]][y]
}

comparatorListNames <-
  Filter(Negate(is.null), comparatorListNames) # Eliminates NULL

cohortNames <- list("targetCohort" = targetListNames, "comparatorCohort" = comparatorListNames)

# use this as condition for Analysis input selector: length(unique(targetCohort[[1]]$`Analysis name`))


# ll <- list(1:4, 5:6, 7:12, 1:12)
# ll <- lapply(ll, as.character)
# 
# which(sapply(cohortNames$targetCohort, FUN=function(X) "Condition group era any time prior" %in% X))
