library(shiny)

# Original Assignment 1 modules (retained), plus Assignment 3.
source('R/ogrenme.R', local = TRUE)
source('R/moving_average.R', local = TRUE)
source('R/aggregate_core.R', local = TRUE)
source('R/aggregate_planning.R', local = TRUE)

ui <- navbarPage('Production Planning',
  tabPanel('Learning Curve', learning_ui('learning')),
  tabPanel('Moving Average', moving_average_ui('moving_average')),
  tabPanel('Aggregate Planning', aggregate_planning_ui('aggregate'))
)

server <- function(input, output, session) {
  learning_server('learning')
  moving_average_server('moving_average')
  aggregate_planning_server('aggregate')
}

shinyApp(ui, server)
