library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)
library(htmltools)
library(tippy)
library(bslib)

source("read_data.R")
# cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
datasource <- "SYNPUF5PCT"
app_data <- read_data(path = "data")
atlas_link <- readr::read_lines(file.path("data", "atlas_link.txt"))
repo_link <- readr::read_lines(file.path("data", "repo_link.txt"))

cohorts <- readr::read_csv(file.path("data", "cohorts.csv"))

target_cohorts <- filter(cohorts, type == "target") %>%
  {
    setNames(pull(., "cohort_id"), pull(., "cohort_name"))
  }

outcome_cohorts <- filter(cohorts, type == "outcome") %>%
  {
    setNames(pull(., "cohort_id"), pull(., "cohort_name"))
  }

stopifnot(length(target_cohorts) >= 1, length(outcome_cohorts) >= 1)

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>%
  stringr::str_remove("_targetId.*$") %>%
  stringr::str_subset("^cohorts.$", negate = T) %>%
  stringr::str_unique()

# theme <- bs_theme()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  bslib::page_navbar(
    theme = bslib::bs_theme(
      "navbar-bg" = "#005480",
      fg = "black",
      bg = "white"
    ),
    title = "Incidence Rate Analysis",
    id = "nav",
    sidebar = sidebar(
      conditionalPanel(
        "input.nav === 'Introduction'",
        "Application's and cohort descriptions."
      ),
      conditionalPanel(
        "input.nav === 'Analysis'",
        selectInput("target_id", "Target cohorts", target_cohorts),
        selectInput("outcome_id", "Outcome cohorts", outcome_cohorts
                    # selectize = F,
                    # width = "150px")),))
        )
      )
    ),
    nav_panel("Introduction", card(
      p("Incidence rates and proportions are statistics that are used in public health to assess 
        the occurrence of a new outcome in a population during a time-at-risk (TAR)."),
      p("In an incidence calculation, we describe: amongst the persons in the target cohort, 
        who experienced the outcome cohort during the time at risk period.")
    )),
    nav_panel(
      "Analysis",
      card(
        tags$h5("Summary Statistics for the Cohort"),
        reactableOutput("summary_table"),
        #tags$br(),
        #tags$br(),
        tags$br(),
        tags$h5("Summary Statistics for Strata within the Cohort"),
        reactableOutput("subgroup_table"),
        htmlOutput("selected_subset_text"),
        echarts4rOutput('treemap')
      ),
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(a(
        "Bitbucket Repository", href = repo_link, target = "_blank"
      )),
      nav_item(a(
        "OHDSI Atlas: GI bleed", href = atlas_link, target = "_blank"
      ))
    )
  )
) 
  
  server <- function(input, output) {
    # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
    # rows_to_highlight <- reactive({
    #   if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(FALSE)
    #   as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
    # })
    
    output$selected_subset_text <- renderText({
      if (!isTruthy(input$box_click$name))
        return("<br>")
      req(input$box_click$name)
      
      x <- app_data$treemap_table %>%
        filter(
          data_source == datasource,
          target == input$target_id,
          outcome == input$outcome_id,
          subset_ids == input$box_click$name
        )
      
      if (nrow(x) != 1)
        stop("Error with filtering. Only one subgroup should be selected!")
      
      output$description <- renderText(ROhdsiWebApi::getCohortDefinition(cohortId = 1747753, "http://api.ohdsi.org:8080/WebAPI/")[[3]]) 
      
      n_criteria <- app_data$subgroup_table %>%
        filter(data_source == datasource,
               target == input$target_id,
               outcome == input$outcome_id) %>%
        nrow()
      
      n_critera_passed <-
        length(stringr::str_split(x$subset_ids, ",")[[1]])
      n_critera_failed <- n_criteria - n_critera_passed
      
      glue::glue(
        "{x$cases} Cases, {x$time_at_risk} TAR, Rate: {round(x$rate_per_1k_years, 2)} <br> {x$total_persons} (%) people, {n_critera_passed} criteria passed, {n_critera_failed} criteria failed."
      )
    })
    
    # output$summary_table <- gt::render_gt(
    #   app_data[[input$datasource]][[input$level]]$summary_table %>%
    #     gt::gt() %>%
    #     gt::fmt_number(columns = dplyr::matches("count"), decimals = 0)
    # )
    
    output$summary_table <- renderReactable({
      app_data$summary_table %>%
        filter(data_source == datasource,
               target == input$target_id,
               outcome == input$outcome_id) %>%
        mutate(
          proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
          rate_per_1k_years = round(rate_per_1k_years, 2)
        ) %>%
        select(
          Persons = total_persons,
          Cases = cases,
          `Proportion \n(per 1k person-years)` = proportion_per_1k_persons,
          `Time at risk \n(years)` = time_at_risk,
          `Rate \n(per 1k person-years)` = rate_per_1k_years
        ) %>%
        reactable(
          columns = list(
            Persons = colDef(
              header = tippy(
                "Persons",
                "Total number of persons in the cohort.",
                placement = "right"
              )
            ),
            Cases = colDef(
              header = tippy(
                "Cases",
                "Total number of persons that arrive to one the end point.",
                placement = "right"
              )
            ),
            `Proportion \n(per 1k person-years)` = colDef(
              header = tippy(
                "Proportion \n(per 1k person-years)",
                "Thousands of cases in a year divided by persons.",
                placement = "right"
              )
            ),
            `Time at risk \n(years)` = colDef(
              header = tippy(
                "Time at risk \n(years)",
                "Estimated anount of time a person is at risk to arriving to the end point.",
                placement = "right"
              )
            ),
            `Rate \n(per 1k person-years)` = colDef(
              header = tippy(
                "Rate \n(per 1k person-years)",
                "Thousands of cases in a year divided by time at risk.",
                placement = "right"
              )
            )
          )
        )
    })
    
    # when a box on the treemap is clicked this reactive store the selected subgroup ids as a numeric vector
    selected_subgroup_ids <- reactive({
      if (!isTruthy(input$box_click$name) ||
          input$box_click$name == "None")
        return(seq_len(length(
          unique(app_data$subgroup_table$subgroup_id)
        )))
      as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
    })
    
    output$subgroup_table <- renderReactable({
      style_function <- function(value, index) {
        if (index %in% selected_subgroup_ids())
          list(color = "red")
        else
          list(color = "black")
      }
      
      app_data$subgroup_table %>%
        filter(data_source == datasource,
               target == input$target_id,
               outcome == input$outcome_id) %>%
        mutate(
          proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
          rate_per_1k_years = round(rate_per_1k_years, 2)
        ) %>%
        select(
          `Stratify rule` = subgroup_name,
          Persons = total_persons,
          Cases = cases,
          `Proportion (per 1k person-years)` = proportion_per_1k_persons,
          `Time at risk (years)` = time_at_risk,
          `Rate (per 1k person-years)` = rate_per_1k_years
        ) %>%
        reactable(
          columns = list(
            "Stratify rule" = colDef(
              header = tippy(
                "Stratify rule",
                "Groups defined inside the cohort.",
                placement = "right"
              ),
              style = style_function
            ),
            "Persons" = colDef(
              header = tippy(
                "Persons",
                "Total number of persons in the cohort.",
                placement = "right"
              ),
              style = style_function
            ),
            "Cases" = colDef(
              header = tippy(
                "Cases",
                "Total number of persons that arrive to one the end point.",
                placement = "right"
              ),
              style = style_function
            ),
            "Proportion (per 1k person-years)" = colDef(
              header = tippy(
                "Proportion \n(per 1k person-years)",
                "Thousands of cases in a year divided by persons.",
                placement = "right"
              ),
              style = style_function
            ),
            "Time at risk (years)" = colDef(
              header = tippy(
                "Time at risk \n(years)",
                "Estimated anount of time a person is at risk to arriving to the end point.",
                placement = "right"
              ),
              style = style_function
            ),
            "Rate (per 1k person-years)" = colDef(
              header = tippy(
                "Rate \n(per 1k person-years)",
                "Thousands of cases in a year divided by time at risk.",
                placement = "right"
              ),
              style = style_function
            )
          ),
          sortable = FALSE
        )
    })
    
    treemap_table <- reactive({
      app_data$treemap_table %>%
        mutate(
          ids = stringr::str_replace(name, "None", "0") %>% stringr::str_split(",") %>% lapply(as.integer)
        ) %>%
        filter(data_source == datasource,
               target == input$target_id,
               outcome == input$outcome_id) %>%
        select(-data_source, -target_id, -outcome_id)
    })
    
    output$treemap <- renderEcharts4r({
      shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
      
      # input = list(datasource = "SYNPUF1K", target_id = 1788144, outcome_id = 1773979)
      app_data$treemap_table %>%
        filter(data_source == datasource,
               target == input$target_id,
               outcome == input$outcome_id) %>%
        select(name = subset_ids, value = cases) %>%
        e_charts() %>%
        e_treemap(roam = F) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") %>%
        e_visual_map(value, inRange = list(color = c("#1f88a6", "#2fcefb", "#0f4251")))
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}", event = "mouseover") %>%
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: false});}", event = "mouseout")
    })
    
    # output$upper_summary_text <- renderText({
    #   s <- app_data[[input$datasource]][[input$level]]$summary_table
    #   glue::glue("Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}")
    # })
    #
    # output$lower_summary_text <- renderText({
    #   denominator <- sum(treemap_table()$value)
    #   numerator <- treemap_table() %>% filter(include_in_summary) %>% pull(value) %>% sum()
    #   percent_included <- scales::label_percent()(numerator / denominator)
    #   # s <- app_data[[input$datasource]][[input$level]]$summary_table
    #   glue::glue("Final Count: {format(numerator, big.mark=',', scientific = FALSE)} ({percent_included})")
    # })
  }
  
  shinyApp(ui = ui, server = server)
  