#' Fusioncharts Demo
#'
#' Running Shiny App 
#'
#' @import htmlwidgets
#' @import shiny
#' 
#' @examples
#' if(interactive()){
#' library(shiny)
#' library(fusionchartsR)
#' runDemo()
#' }
#'
#' @export
runDemo <- function(){
  ui <- shiny::fluidPage(
    shiny::tags$br(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          inputId = "input01", 
          label = "Select a type chart", 
          choices = c("column2d","column3d", "line", "area2d", "bar2d", "bar3d", 
                      "pie2d", "pie3d", "doughnut2d", "doughnut3d")
        ),
        shiny::textInput(
          inputId = "input02", 
          label = "Change the caption title", 
          value = "Caption"
        ),
        shiny::textInput(
          inputId = "input03", 
          label = "Change the subcaption title", 
          value = "Subcaption"
        ),
        shiny::textInput(
          inputId = "input04", 
          label = "Change the X-axis title", 
          value = "X-axis"
        ),
        shiny::textInput(
          inputId = "input05", 
          label = "Change the Y-axis title", 
          value = "Y-axis"
        ),
        shiny::selectInput(
          inputId = "input06", 
          label = "Choose a theme",  
          choices = c("fusion", "gammel", "candy", "zune", "ocean", 
                      "carbon", "umber")
        )
      ),
      shiny::mainPanel(
        fusionPlotOutput(outputId = "plot")
      )
    )
  )
  
  server <- function(input, output, session){
    output$plot <- renderfusionPlot({
      df <- data.frame(label = c("Venezuela", "Saudi", "Canada", "Russia"), value = c(290, 260,180, 115))
      df %>%
      fusionPlot(x = "label", y = "value", type = input$input01) %>%
        fusionCaption(caption = input$input02) %>%
        fusionSubcaption(subcaption = input$input03) %>%
        fusionAxis(xAxisName = input$input04, yAxisName = input$input05) %>%
        fusionTheme(theme = input$input06)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

runDemo()
