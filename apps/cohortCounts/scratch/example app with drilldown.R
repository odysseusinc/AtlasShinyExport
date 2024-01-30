# Create mock data
mock_data <- tibble::tribble(
  ~level1  ,   ~level2     , ~level3           , ~sales,
  "Asia"   ,    "China"    ,   "Shanghai"      ,  32L,
  "Asia"   ,    "China"    ,   "Beijing"       ,  86L,
  "Asia"   ,    "China"    ,   "Chongqing"     ,  30L,
  "Asia"   ,    "India"    ,   "Mumbai"        ,  92L,
  "Asia"   ,    "India"    ,   "Kolkata"       ,  75L,
  "Asia"   ,    "India"    ,   "Chennai"       ,  99L,
  "America",    "USA"      ,   "New York"      ,   6L,
  "America",    "USA"      ,   "Chicago"       ,  12L,
  "America",    "Argentina",   "Buenos Aires"  ,  54L,
  "America",    "Argentina",   "Rosario"       ,  36L,
  "America",    "Brasil"   ,   "Sao Paulo"     ,   2L,
  "America",    "Brasil"   ,   "Rio de Janeiro",  64L,
  "Europe" ,    "Spain"    ,   "Madrid"        ,  54L,
  "Europe" ,    "Spain"    ,   "Barcelona"     ,  46L,
  "Europe" ,    "Spain"    ,   "Sevilla"       ,  67L,
  "Europe" ,    "Italy"    ,   "Rome"          ,  22L,
  "Europe" ,    "France"   ,   "Paris"         ,  42L,
  "Europe" ,    "France"   ,   "Marseille"     ,  91L
)

plot_sales_data <- function(chart_data, chart_title, chart_color) { chart_data |> 
    e_chart(x = level) |>
    e_bar(total, name = chart_title, color = chart_color) |>
    e_on(
      # query = "series.bar",
      query = NULL,
      # Set input values
      handler = "function(params){
         Shiny.setInputValue(
          'custom_bar_click',
          {clicked_level: 'level2', blah:'adsf', drilled_place: params.name}, {priority: 'event'}
         );
       }",
      event = "click"
    )
}
ui <- fluidPage(
  h1("Drill Down in Shiny"),
  echarts4rOutput("chart")
)
# Define server
server <- function(input, output) {
  
  # observeEvent(input$custom_bar_click, {
  #   
  #   print("asdfasdfsa\nadsfadfads\n")
  # })
  # 
  observe({
    input$custom_bar_click
    cli::cat_bullet("asdf")
    print(input$custom_bar_click$clicked_level)
    print(input$custom_bar_click$blah)
  })
  
  output$chart <- renderEcharts4r({
    # Our custom input value that we send from the bar click
    # print(input$custom_bar_click)
    if (is.null(input$custom_bar_click)) {
      # Prepare data for chart
      chart_data <- mock_data |>
        group_by(level = level1) |>
        summarise(total = sum(sales))
      
      # Create chart
      plot_sales_data(
        chart_data = chart_data,
        chart_title = "Sales by Continent",
        chart_color = "#5470C6"
      )      
    } else if(input$custom_bar_click$clicked_level == "level2") {
      # Prepare data for chart
      chart_data <- mock_data |>
        filter(level1 == input$custom_bar_click$drilled_place) |>
        group_by(level = level2) |>
        summarise(total = sum(sales))
      
      # Create chart
      plot_sales_data(
        chart_data = chart_data,
        chart_title = glue::glue(
          "Sales by Country (Filtered for {input$custom_bar_click$drilled_place})"
        ),
        chart_color = "#91CC75"
      )
    }
  })
}
shinyApp(ui, server)
