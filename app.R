library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Moving Average", moving_average_ui("moving_average")),
  tabPanel("Aggregate Planning", aggregate_planning_ui("aggregate_planning"))
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
  aggregate_planning_server("aggregate_planning")
}

shinyApp(ui, server)
