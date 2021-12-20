#' Create new charts
#'
#' Main function to make interactive charts.
#' Check all charts at \url{https://www.fusioncharts.com/charts}
#'
#'
#' @import htmlwidgets
#' @importFrom jsonlite toJSON
#' @importFrom grDevices boxplot.stats
#'
#'
#' @param data Default dataset to use
#' @param x,y character name of variable
#' @param type See `available_charts()`
#' @param numberSuffix Specify the suffix for all the Y-axis values on the chart
#'
#'
#' @export
fusionPlot <- function(data,x, y, type = "column2d", numberSuffix = NULL) {
  
  # Main arguments
  category <- NULL
  dataset <- NULL
  
  # boxandwhisker2d arguments
  showmean <- "0"
  showalloutliers <- NULL

  charts <- available_charts()

  if(type %in% charts[[1]][-c(13:14)]){

    new.data <- data.frame(label = factor(data[,x]), value = data[,y])
    data <- toJSON(x = new.data, pretty = TRUE)

  }
  
  else if(type == "boxandwhisker2d"){
    
    showmean <- "1"
    
    xaxis <- factor(data[,x])
    df <- list(category = data.frame(label = as.character(levels(xaxis))))
    category <- toJSON(x = df, pretty = TRUE)
    
    n <- unique(levels(xaxis))
    df.list <- lapply(1:length(n), function(i){
      yaxis <- data[data[,x] == n[i],y]
      stats <- boxplot.stats(yaxis)
      if(length(stats$out) >= 1){
        list(
          value = toString(yaxis[! yaxis %in% unique(stats$out)]),
          outliers = toString(unique(stats$out))
        )
      }
      else {
        list(
          value = toString(yaxis)
        )
      }
    })
    
    if(length(grep(pattern = "outliers", x = df.list)) > 0){
      showalloutliers <- 1
    }
    else {
      showalloutliers <- 0
    }
    
    newlist <- list(seriesname = y, data = df.list)
    dataset <- toJSON(x = newlist, pretty = TRUE, auto_unbox = TRUE)
    
  }
  else if(type == "scatter"){
    
    df <- data.frame(x = data[,x], y = data[,y])
    dataset <- list(data = df)
    dataset <- toJSON(x = dataset, pretty = TRUE, auto_unbox = TRUE)
    
  }
  else{
    stop('Chart not available. Please check `fusionMultiPlot()`')
  }


#' @examples
#' library(fusionchartsR)
#'
#' # Single
#' df <- data.frame(label = c("Venezuela", "Saudi", "Canada", "Russia"), value = c(290, 260,180, 115))
#' df %>%
#' fusionPlot(x = "label", y = "value", type = "pie2d") %>%
#' fusionTheme(theme = "fusion")
#' 

  # forward options using x
  x <- list(
    data = data,
    categories = category,
    dataset = dataset,
    type = type,
    numberSuffix = numberSuffix,
    showmean = showmean,
    showalloutliers = showalloutliers
    )

  # create widget
  widgets <- htmlwidgets::createWidget(
    name = 'fusionPlot',
    x = x,
    package = 'fusionchartsR'
  )

  widgets %>%
  fusionCaption() %>%
    fusionSubcaption() %>%
    fusionBackground() %>%
    fusionCanvas() %>%
    fusionAxis() %>%
    fusionCustomAxis() %>%
    fusionLegend() %>%
    fusionCustomLegend() %>%
    fusionPalette() %>%
    fusionAnchors() %>%
    fusionTrendline() %>%
    fusionDiv() %>%
    fusionTooltip() %>%
    fusionLogo() %>%
    fusionTheme()

}


#' Shiny bindings for fusionPlot
#'
#' Output and render functions for using fusionPlot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a fusionPlot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name fusionPlotOutput
#' @aliases renderfusionPlot
#'
#' @export
fusionPlotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'fusionPlot', width, height, package = 'fusionchartsR')
}

#' @rdname fusionPlotOutput
#' @export
renderfusionPlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, fusionPlotOutput, env, quoted = TRUE)
}
