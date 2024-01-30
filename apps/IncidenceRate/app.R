library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)

source("read_data.R")
# cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
app_data <- read_data(path = "data")
atlas_link <- readr::read_lines(file.path("data", "atlas_link.txt"))

cohorts <- readr::read_csv(file.path("data", "cohorts.csv"))

target_cohorts <- filter(cohorts, type == "target") %>% 
  {setNames(pull(., "cohort_id"), pull(., "cohort_name"))}

outcome_cohorts <- filter(cohorts, type == "outcome") %>% 
  {setNames(pull(., "cohort_id"), pull(., "cohort_name"))}

stopifnot(length(target_cohorts) >= 1, length(outcome_cohorts) >= 1)

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>% 
  stringr::str_remove("_targetId.*$") %>% 
  stringr::str_subset("^cohorts.$", negate = T) %>% 
  stringr::str_unique()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(width = 6, titlePanel("Incidence Rate Analysis"), tags$a(atlas_link, href = atlas_link)),
    column(width = 6, selectInput("datasource", "Data Source", choices = data_sources), style = "margin-top: 15px")
  ),

  fluidRow(column(width = 12,
    "Showing target cohort", 
    tags$div(style="display:inline-block", selectInput("target_id", "", target_cohorts, selectize = F, width = "150px")),
    "and outcome cohort",
    tags$div(style="display:inline-block", selectInput("outcome_id", "", outcome_cohorts, selectize = F, width = "150px"))
  )),

  fluidRow(
    column(width = 6, 
           tags$br(),
           reactableOutput("summary_table"), 
           tags$br(),tags$br(),tags$br(),tags$br(),
           reactableOutput("subgroup_table")
    ),
    column(width = 6, htmlOutput("selected_subset_text"), echarts4rOutput('treemap'))
  )
  # fluidRow(column(width = 12, textOutput("lower_summary_text"))),
)

server <- function(input, output) {
  
  # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
  # rows_to_highlight <- reactive({
  #   if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(FALSE)
  #   as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  # })
  
  output$selected_subset_text <- renderText({
    if(!isTruthy(input$box_click$name)) return("<br>") 
    req(input$box_click$name)

    x <- app_data$treemap_table %>%
      filter(data_source == input$datasource, 
             target == input$target_id, 
             outcome == input$outcome_id,
             subset_ids == input$box_click$name)
    
    if (nrow(x) != 1) stop("Error with filtering. Only one subgroup should be selected!")

    n_criteria <- app_data$subgroup_table %>% 
      filter(data_source == input$datasource, target == input$target_id, outcome == input$outcome_id) %>% 
      nrow()
    
    n_critera_passed <- length(stringr::str_split(x$subset_ids, ",")[[1]])
    n_critera_failed <- n_criteria - n_critera_passed
   
    glue::glue("{x$cases} Cases, {x$time_at_risk} TAR, Rate: {round(x$rate_per_1k_years, 2)} <br> {x$total_persons} (%) people, {n_critera_passed} criteria passed, {n_critera_failed} criteria failed.")
  })
  
  # output$summary_table <- gt::render_gt(
  #   app_data[[input$datasource]][[input$level]]$summary_table %>% 
  #     gt::gt() %>% 
  #     gt::fmt_number(columns = dplyr::matches("count"), decimals = 0)
  # )
  
  output$summary_table <- renderReactable({
    app_data$summary_table %>% 
      filter(data_source == input$datasource, target == input$target_id, outcome == input$outcome_id) %>% 
      mutate(proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
             rate_per_1k_years = round(rate_per_1k_years, 2)) %>%
      select(Persons = total_persons,
             Cases = cases,
             `Proportion \n(per 1k person-years)` = proportion_per_1k_persons,
             `Time at risk \n(years)` = time_at_risk,
             `Rate \n(per 1k person-years)` = rate_per_1k_years) %>% 
      reactable()
  })
  
  # when a box on the treemap is clicked this reactive store the selected subgroup ids as a numeric vector
  selected_subgroup_ids <- reactive({
    if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(seq_len(length(unique(app_data$subgroup_table$subgroup_id))))
    as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  })
    
  output$subgroup_table <- renderReactable({
    
    style_function <- function(value, index) {
      if (index %in% selected_subgroup_ids()) list(color = "black") else list(color = "red")
    } 
    
    app_data$subgroup_table %>% 
      filter(data_source == input$datasource, target == input$target_id, outcome == input$outcome_id) %>% 
      mutate(proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
             rate_per_1k_years = round(rate_per_1k_years, 2)) %>% 
      select(
        `Stratify rule` = subgroup_name,
        Persons = total_persons, 
        Cases = cases,
        `Proportion (per 1k person-years)` = proportion_per_1k_persons,
        `Time at risk (years)` = time_at_risk,
        `Rate (per 1k person-years)` = rate_per_1k_years) %>% 
      reactable(
        columns = list("Stratify rule" = colDef(style = style_function),
                       "Persons" = colDef(style = style_function),
                       "Cases" = colDef(style = style_function),
                       "Proportion (per 1k person-years)" = colDef(style = style_function),
                       "Time at risk (years)" = colDef(style = style_function),
                       "Rate (per 1k person-years)" = colDef(style = style_function)),
        sortable = FALSE
      )
  })
  
  treemap_table <- reactive({
    app_data$treemap_table %>%
      filter(data_source == input$datasource, target == input$target_id, outcome == input$outcome_id) %>%
      select(-data_source, -target_id, -outcome_id) %>%
      mutate(ids = stringr::str_replace(name, "None", "0") %>% stringr::str_split(",") %>% lapply(as.integer))
  })

  output$treemap <- renderEcharts4r({
    shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")

    # input = list(datasource = "SYNPUF1K", target_id = 1788144, outcome_id = 1773979)
    app_data$treemap_table %>%
      filter(data_source == input$datasource, target == input$target_id, outcome == input$outcome_id) %>% 
      select(name = subset_ids, value = rate_per_1k_years) %>% 
      e_charts() %>%
      e_treemap(roam = F) %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") #%>%
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

