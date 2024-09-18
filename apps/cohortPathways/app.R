library(shiny)
library(dplyr)
library(bslib)
library(logger)
library(sunburstShinyWidget)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

log_level(INFO)

# Setup -------------------------------------------------------------------
log_info("reading input data")

data_dir <- "data"

PROPERTIES <- properties::read.properties(file.path(data_dir, "app.properties"))
repo_link <- PROPERTIES$repo_link
atlas_link <- PROPERTIES$atlas_link
datasource <- PROPERTIES$datasource

chartData <- jsonlite::read_json(file.path(data_dir, "chartData.json"))
design <- jsonlite::read_json(file.path(data_dir, "design.json"))
eventCodes <- chartData$eventCodes %>% dplyr::bind_rows()

# UI ----------------------------------------------------------------------

log_info("initializing Shiny UI")

ui <- fluidPage(
  shiny.info::version(as.character(packageVersion("cohortPathways")), position = "bottom right"),
  bslib::page_navbar(
    theme = bslib::bs_theme(
      "navbar-bg" = "#005480",
      fg = "black",
      bg = "white"
    ),
    underline = TRUE,
    title = "Cohort Pathways",
    id = "nav",
    # sidebar = sidebar(
    #   conditionalPanel(
    #     "input.nav === 'Introduction'",
    #     "Application's and cohort descriptions."
    #   ),
    #   conditionalPanel(
    #     "input.nav === 'Analysis'",
    #     # ----- module goes here
    #   )
    # ),
    nav_panel("Introduction", card(
                                  tags$h1("!! Intro page to be updated !!"),
                                  p("The application provide's important statistics and charts to describe
                                    cohort counts and percentages of persons or records in
                                    the cohort afther apliying a certain set of inclusion
                                    rules."),
                                   p(strong("Cohort entry event: "), "The cohort entry event (initial ev ent) defines
                                    the time when people enter the cohort, called the cohort index date. A cohort
                                    entry event can be any event recorded in the CDM such as drug exposures, conditions,
                                    procedures, measurements and visits. Initial events are defined by the CDM domain where
                                    the data are stored (e.g. PROCEDURE_OCCURRENCE, DRUG_EXPOSURE, etc), the
                                    concept sets built to identify the clinical activity (e.g. SNOMED codes for conditions,
                                    RxNorm codes for drugs) as well as any other specific attributes (e.g. age at occurrence,
                                    first diagnosis/procedure/etc, specifying start and end date, specifying visit type or
                                    criteria, days supply, etc). The set of people having an entry event is referred to as
                                    the initial event cohort."),
                                   p(strong("Inclusion criteria: "), "Inclusion criteria are applied to the initial event cohort to further
                                    restrict the set of people. Each inclusion criterion is defined by the CDM domain(s)
                                    where the data are stored, concept set(s) representing the clinical activity,
                                    domain-specific attributes (e.g. days supply, visit type, etc), and the temporal
                                    logic relative to the cohort index date. Each inclusion criterion can be evaluated
                                    to determine the impact of the criteria on the attrition of persons from the initial
                                    event cohort. The qualifying cohort is defined as all people in the initial event
                                    cohort that satisfy all inclusion criteria."),
                                   p(strong("Cohort exit criteria: "), "The cohort exit event signifies when a person no longer qualifies
                                    for cohort membership. Cohort exit can be defined in multiple ways such as the end of
                                    the observation period, a fixed time interval relative to the initial entry event, the
                                    last event in a sequence of related observations (e.g. persistent drug exposure) or through
                                    other censoring of observation period. Cohort exit strategy will impact whether a person
                                    can belong to the cohort multiple times during different time intervals."),
                                   p("The description of the cohort being analysed is: ", textOutput("description")))),
    nav_panel("Analysis",
              sunburstShinyWidget::sunburstUI("sunburst_plot")
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(
        a("Git Repository",
          href = repo_link,
          target = "_blank")
      ),
      nav_item(a(
        datasource, href = atlas_link, target = "_blank"
      ))
    ),
  )
)

log_info("initializing Shiny Server")
server <- function(input, output) {
  # output$description <- renderText(ROhdsiWebApi::getCohortDefinition(cohortId = 101431, "http://api.ohdsi.org:8080/WebAPI/")[[3]])
  sunburstServer("sunburst_plot", chartData, design)
}

log_info("running Shiny app")
shinyApp(ui = ui, server = server)
