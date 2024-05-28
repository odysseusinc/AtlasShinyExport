library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)
library(bslib)
library(tippy)

source("read_data.R")
cohort_name <- readr::read_file(file.path("data", "cohort_name.txt"))
cohort_link <- readr::read_file(file.path("data", "cohort_link.txt"))
app_data <- read_data(path = "data")

if (length(app_data[[1]]$event$inclusion_table$ID) < 1 |
    length(app_data[[1]]$person$inclusion_table$ID) < 1)
  stop(paste("zero inclusion rules in the cohort!"))

# get the list of unique data source keys
data_sources <- list.files("data", pattern = ".json$") %>%
  stringr::str_remove("_by_(person|event).json$") %>%
  stringr::str_unique()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  bslib::page_navbar(
    theme = bslib::bs_theme(
      "navbar-bg" = "#005480",
      fg = "black",
      bg = "white"
    ),
    underline = TRUE,
    title = "Cohort Inclusion Report",
    id = "nav",
    sidebar = sidebar(
      conditionalPanel(
        "input.nav === 'Introduction'",
        "Application's functionalities."
      ),
      conditionalPanel(
        "input.nav === 'Analysis'",
        shinyWidgets::radioGroupButtons(
          inputId = "level",
          label = "Cohort unit",
          selected = "person",
          individual = TRUE,
          choiceNames = c("Person", "Event"),
          choiceValues = c("person", "event"),
          size = "sm",
          width = "100%"
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "switch_view",
          label = "Cohort unit",
          selected = "Intersect",
          individual = TRUE,
          choiceNames = c("Intersect", "Attrition"),
          choiceValues = c("Intersect", "Attrition"),
          size = "sm",
          width = "100%"
        )
      )
    ),
    nav_panel("Introduction", card(p("Provide's important statistics and charts to describe
                                    cohort counts and percentages of persons or records in 
                                    the cohort afther apliying a certain set of inclusion 
                                    rules."))),
    nav_panel("Analysis", card(
      fluidRow(column(
        width = 12, textOutput("upper_summary_text")
      )),
      
      fluidRow(column(
        width = 12,
        tags$div(
          id = "filter_text",
          "Having",
          tags$div(
            style = "display:inline-block",
            selectInput(
              "any_all",
              "",
              c("any", "all"),
              selectize = F,
              width = "80px"
            )
          ),
          "of selected criteria",
          tags$div(
            style = "display:inline-block",
            selectInput(
              "passed_failed",
              "",
              c("passed", "failed"),
              selectize = F,
              width = "100px"
            )
          )
        ),
        tags$div(id = "filter_text_filler", HTML("<br><br><br>")) # fill the space when filter_text is hidden (i.e. when in attrition view).
      )),
      
      fluidRow(column(
        width = 8,
        reactableOutput("inclusion_table"),
        tags$br(),
        textOutput("lower_summary_text")
      )),
      fluidRow(
        column(
          width = 12,
          # actionButton("switch_view", "Switch to attrition view", style = "float: right"),
          tags$br(),
          tags$br(),
          htmlOutput("count_in_selected_subset_text"),
          echarts4rOutput('plot')
        )
      )
    )),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(
        a("Bitbucket Repository", href = "https://bitbucket.org/Odysseus/atlasshinyexport/src/main/apps/cohortCounts/", target = "_blank")
      ),
      nav_item(a(
        cohort_name, href = cohort_link, target = "_blank"
      ))
    ),
  )
)

server <- function(input, output) {
  shinyjs::hide("filter_text_filler")
  
  # when a box on the treemap is clicked this reactive will store the associated IDs as numbers
  rows_to_highlight <- reactive({
    if (!isTruthy(input$box_click$name) ||
        input$box_click$name == "None")
      return(FALSE)
    as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  })
  
  attritionView <- reactiveVal(1)
  
  observeEvent(input$switch_view, {
    if (attritionView() == 0) {
      # switch to attrition view
      updateActionLink(inputId = "switch_view", label = "View type")
      attritionView(1)
      shinyjs::hide("filter_text")
      shinyjs::show("filter_text_filler")
    } else {
      # switch to intersect view
      updateActionLink(inputId = "switch_view", label = "View type")
      attritionView(0)
      shinyjs::hide("filter_text_filler")
      shinyjs::show("filter_text")
    }
  })
  
  output$count_in_selected_subset_text <- renderPrint({
    if (!isTruthy(input$box_click$name) ||
        attritionView() == 1)
      return(HTML("<br>"))
    req(input$box_click$name)
    
    x <- app_data$SYNPUF_110k[[input$level]]$treemap_table %>%
      mutate(total = sum(value),
             percent = round(100 * value / total, 2)) %>%
      filter(name == input$box_click$name) %>%
      mutate(
        value = format(value, big.mark = ',', scientific = FALSE),
        percent = paste0(percent, "%")
      )
    
    if (input$box_click$name == "None") {
      glue::glue(
        "Number of {input$level}s not matching any inclusion rules: {x$value} ({x$percent})"
      )
    } else {
      glue::glue(
        "Number of {input$level}s matching inclusion rules [{input$box_click$name}]: {x$value} ({x$percent})"
      )
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
        if (index %in% rows_to_highlight())
          list(color = "red")
        else
          list(color = "black")
      }
      
      defaultSelected <- seq_len(nrow(app_data$SYNPUF_110k[[input$level]]$inclusion_table))
      
      app_data[[1]][[input$level]]$inclusion_table %>%
        reactable(
          selection = "multiple",
          onClick = "select",
          defaultSelected = defaultSelected,
          bordered = TRUE,
          columns = list(
            "Inclusion Rule" = colDef(
              style = style_function,
              header = tippy(
                "Inclusion Rule",
                "Criteria to be included in the cohort.",
                placement = "right"
              ),
              width = 500
            ),
            "ID" = colDef(
              style = style_function,
              align = "left",
              maxWidth = 40
            ),
            "Count" = colDef(
              style = style_function,
              header = tippy(
                "Count",
                "Number of persons that fullfill the criteria.",
                placement = "right"
              )
            ),
            "Percent" = colDef(
              style = style_function,
              header = tippy(
                "Percent",
                "Percent of the cohort that fullfill the criteria.",
                placement = "right"
              )
            )
          )
        )
      
    } else if (attritionView() == 1) {
      app_data[[1]][[input$level]]$attrition_table %>%
        mutate(pct_remain = round(pct_remain, 4),
               pct_diff = round(pct_diff, 4)) %>%
        reactable(
          sortable = FALSE,
          bordered = TRUE,
          columns = list(
            "ID" = colDef(name = "ID", maxWidth = 40),
            "Inclusion Rule" = colDef(
              name = "Inclusion Rule",
              header = tippy(
                "Inclusion Rule",
                "Criteria to be included in the cohort.",
                placement = "right"
              ),
              width = 500
            ),
            "Count" = colDef(
              name = "Count",
              header = tippy(
                "Count",
                "Number of persons that fullfill the criteria.",
                placement = "right"
              )
            ),
            "pct_remain" = colDef(
              name = "Percent remaining",
              format = colFormat(percent = TRUE),
              header = tippy(
                "Percent remaining",
                "Percentage of persons remaining in the cohort after fullfilling the criteria.",
                placement = "right"
              )
            ),
            "pct_diff" = colDef(
              name = "Percent difference",
              format = colFormat(percent = TRUE),
              header = tippy(
                "Percent difference",
                "Differebce in the percentage remaining after fullfilling the criteria.",
                placement = "right"
              )
            )
          )
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
    app_data[[1]][[input$level]]$treemap_table %>%
      mutate(
        ids = stringr::str_replace(name, "None", "0") %>% stringr::str_split(",") %>% lapply(as.integer)
      ) %>%
      mutate(
        include_in_summary = case_when(
          input$any_all == "any" &&
            input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~ any(. %in% selected_rows())),
          input$any_all == "all" &&
            input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~ all(. %in% selected_rows())),
          input$any_all == "any" &&
            input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~ any(!(
              selected_rows() %in% .
            ))),
          input$any_all == "all" &&
            input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~ all(!(
              selected_rows() %in% .
            ))),
          TRUE ~ TRUE
        )
      )
    
  })
  
  output$plot <- renderEcharts4r({
    if (attritionView() == 0) {
      shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
      
      app_data[[1]][[input$level]]$treemap_table %>%
        e_charts() %>%
        e_treemap() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") %>%
        e_visual_map(value, inRange = list(color = c("#1f88a6", "#2fcefb", "#0f4251")))
      # use the code below for mouse over interaction
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}", event = "mouseover") %>%
      # e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: false});}", event = "mouseout")
    }
    else if (attritionView() == 1) {
      df <- tibble(app_data[[1]][[input$level]]$attrition_table)
      df <- select(df, ID, pct_diff) %>%
        mutate(pct_diff = round(pct_diff * 100, 2)) %>%
        rename(`Percent diffecerence` = pct_diff) %>%
        e_charts(ID) %>%
        e_bar(`Percent diffecerence`, itemStyle = list(color = "#0f4251")) %>%
        e_labels() %>%
        e_hide_grid_lines()
    } else {
      stop("There is a problem. attritionView should only be 0 or 1.")
    }
  })
  
  output$upper_summary_text <- renderText({
    s <- app_data[[1]][[input$level]]$summary_table
    glue::glue(
      "Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}"
    )
  })
  
  output$lower_summary_text <- renderText({
    denominator <- sum(treemap_table()$value)
    numerator <- treemap_table() %>% filter(include_in_summary) %>% pull(value) %>% sum()
    percent_included <- scales::label_percent()(numerator / denominator)
    # s <- app_data[[input$datasource]][[input$level]]$summary_table
    glue::glue(
      "Final Count: {format(numerator, big.mark=',', scientific = FALSE)} ({percent_included})"
    )
  })
}

shinyApp(ui = ui, server = server)
