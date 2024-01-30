library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)

source("read_data.R")
cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
cohort_link <- readr::read_file(file.path("data", "cohort_link.txt"))
app_data <- read_data(path = "data")

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>% 
  stringr::str_remove("_by_(person|event).json$") %>% 
  stringr::str_unique()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(width = 6, titlePanel("Cohort Inclusion Report")),
    column(width = 6, selectInput("datasource", "Data Source", choices = data_sources), style = "margin-top: 15px")
  ),
  
  p(tags$a(cohort_name, href = cohort_link), style = "margin-bottom: 15px; font-size: 1.5em"),
  
  fluidRow(column(width = 6, offset = 5,
    shinyWidgets::radioGroupButtons(
      inputId = "level",
      label = "",
      selected = "person",
      individual = TRUE,
      choiceNames = c("By Person", "By Event"),
      choiceValues = c("person", "event")
    )
  )),
  
  fluidRow(column(width = 12, textOutput("upper_summary_text"))),
  
  fluidRow(column(width = 12, 
    tags$div(id = "filter_text",
      "Having", 
      tags$div(style="display:inline-block", selectInput("any_all", "", c("any", "all"), selectize = F, width = "80px")),
      "of selected criteria",
      tags$div(style="display:inline-block", selectInput("passed_failed", "", c("passed", "failed"), selectize = F, width = "100px"))
    ),
    tags$div(id = "filter_text_filler", HTML("<br><br><br>")) # fill the space when filter_text is hidden (i.e. when in attrition view).
  )),

  fluidRow(
    column(width = 4, 
           reactableOutput("inclusion_table"),
           tags$br(),
           textOutput("lower_summary_text")),
    
    column(width = 8,
      actionLink("switch_view", "Switch to attrition view", style = "float: right"), 
      tags$br(), tags$br(),
      htmlOutput("count_in_selected_subset_text"), 
      echarts4rOutput('plot'))
  ),
  
  div(a("Link to app code", href = "https://github.com/OdyOSG/AtlasShinyExport"), style = "postition: absolute; bottom: 0; left 0;")
)

server <- function(input, output) {
  shinyjs::hide("filter_text_filler")
  
  # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
  rows_to_highlight <- reactive({
    if(!isTruthy(input$box_click$name) || input$box_click$name == "None") return(FALSE)
    as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  })
  
  attritionView <- reactiveVal(0)
  
  observeEvent(input$switch_view, {
    if (attritionView() == 0) {
      # switch to attrition view
      updateActionLink(inputId = "switch_view", label = "Switch to intersect view")
      attritionView(1)
      shinyjs::hide("filter_text")
      shinyjs::show("filter_text_filler")
    } else {
      # switch to intersect view
      updateActionLink(inputId = "switch_view", label = "Switch to attrition view")
      attritionView(0)
      shinyjs::hide("filter_text_filler")
      shinyjs::show("filter_text")
    }
  })
  
  output$count_in_selected_subset_text <- renderPrint({
    if(!isTruthy(input$box_click$name) || attritionView() == 1) return(HTML("<br>"))
    req(input$box_click$name)
    
    x <- app_data[[input$datasource]][[input$level]]$treemap_table %>% 
      mutate(total = sum(value), percent = round(100*value/total, 2)) %>% 
      filter(name == input$box_click$name) %>% 
      mutate(value = format(value, big.mark=',', scientific = FALSE),
             percent = paste0(percent, "%"))
      
    if (input$box_click$name == "None") {
      glue::glue("Number of {input$level}s not matching any inclusion rules: {x$value} ({x$percent})")
    } else {
      glue::glue("Number of {input$level}s matching inclusion rules [{input$box_click$name}]: {x$value} ({x$percent})")
    }
  })
  
  # output$summary_table <- gt::render_gt(
  #   app_data[[input$datasource]][[input$level]]$summary_table %>% 
  #     gt::gt() %>% 
  #     gt::fmt_number(columns = dplyr::matches("count"), decimals = 0)
  # )
  
  output$inclusion_table <- renderReactable({
    
    if (attritionView() == 0) {
      
    style_function <- function(value, index) {
      if (index %in% rows_to_highlight()) list(color = "red") else list(color = "black")
    } 
    
    defaultSelected <- seq_len(nrow(app_data[[input$datasource]][[input$level]]$inclusion_table))
    
    app_data[[input$datasource]][[input$level]]$inclusion_table %>% 
      reactable(selection = "multiple", 
                onClick = "select", 
                defaultSelected = defaultSelected,
                columns = list("Inclusion Rule" = colDef(style = style_function),
                               "ID" = colDef(style = style_function, align = "left", maxWidth = 40),
                               "Count" = colDef(style = style_function),
                               "Percent" = colDef(style = style_function))
      )
    
    } else if (attritionView() == 1) {
      app_data[[input$datasource]][[input$level]]$attrition_table %>% 
        mutate(pct_remain = round(pct_remain, 4),
               pct_diff = round(pct_remain, 4)) %>% 
        reactable(sortable = FALSE,
                  columns = list("ID" = colDef(name = "ID"),
                                 "Inclusion Rule" = colDef(name = "Inclusion Rule"),
                                 "Count" = colDef(name = "Count"),
                                 "pct_remain" = colDef(name = "Percent remaining", format = colFormat(percent = TRUE)),
                                 "pct_diff" = colDef(name = "Percent difference", format = colFormat(percent = TRUE)))
        )
    } else {
      stop("There is a problem. attritionView should either be 1 or 0.")
    }
    
  })
    
  selected_rows <- reactive(getReactableState("inclusion_table", "selected"))
  
      # gt::gt() %>% 
      # gt::fmt_number(columns = dplyr::matches("count"), decimals = 0) %>% 
      # {if (isTruthy(input$box_click$name))
      #   gt::tab_style(.,
      #     style = list(gt::cell_fill(color = "lightblue")),
      #     locations = gt::cells_body(rows = rows_to_highlight())
      #   ) else .}
  
  treemap_table <- reactive({
    # app_data$SYNPUF_110k$person$treemap_table %>% # for testing
    app_data[[input$datasource]][[input$level]]$treemap_table %>%
      mutate(ids = stringr::str_replace(name, "None", "0") %>% stringr::str_split(",") %>% lapply(as.integer)) %>% 
      mutate(include_in_summary = case_when(
        input$any_all == "any" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~any(. %in% selected_rows())),
        input$any_all == "all" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~all(. %in% selected_rows())),
        input$any_all == "any" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~any(!(selected_rows() %in% .))),
        input$any_all == "all" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~all(!(selected_rows() %in% .))),
        TRUE ~ TRUE
      ))
    
  })
  
  output$plot <- renderEcharts4r({
    
    if (attritionView() == 0) {
    shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
    
    app_data[[input$datasource]][[input$level]]$treemap_table %>% 
      e_charts() %>% 
      e_treemap(roam = F) %>% 
      e_toolbox_feature(feature = "saveAsImage") %>% 
      e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}")
      # use the code below for mouse over interaction
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}", event = "mouseover") %>% 
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: false});}", event = "mouseout")
    } else if (attritionView() == 1) {
      app_data[[input$datasource]][[input$level]]$attrition_table
      
      df <- tibble(
        x = factor(5:1),
        y = rev(c(1000, 555, 99, 10, 0)),
        pad = (max(y) - y)/2
      )
      
      df %>% 
        e_charts(x) %>% 
        e_bar(pad, stack = "grp", legend = F, color = 'rgba(0,0,0,0)') %>% # used for offset
        e_bar(y, stack = "grp", legend = F) %>% 
        e_flip_coords() %>% 
        e_x_axis(show = F) %>% 
        e_y_axis(show = F) %>% 
        e_hide_grid_lines() 
      
    } else {
      stop("There is a problem. attritionView should only be 0 or 1.")
    }
  })
  
  output$upper_summary_text <- renderText({
    s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}")
  })
  
  output$lower_summary_text <- renderText({
    denominator <- sum(treemap_table()$value)
    numerator <- treemap_table() %>% filter(include_in_summary) %>% pull(value) %>% sum()
    percent_included <- scales::label_percent()(numerator / denominator)
    # s <- app_data[[input$datasource]][[input$level]]$summary_table 
    glue::glue("Final Count: {format(numerator, big.mark=',', scientific = FALSE)} ({percent_included})")
  })
}

shinyApp(ui = ui, server = server)





