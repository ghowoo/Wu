#' A heatmap Function
#'
#' This function allows you to create a heatmap.
#' @param 
#' @keywords heatmap
#' @export
#' @examples
#' heatmap()

heatmap <- function(obj, title = ""){
    obj <- Wu::delete_single_value_column(obj)
    obj <- as.data.frame(obj)
    cor_matrix <- Wu::cors_mixed(obj)
    cor_matrix <- cor_matrix^2
    cor_matrix <- round(cor_matrix, 3)
    trace1 <- list(
        type = "heatmap"
      , x = colnames(cor_matrix)
      , y = colnames(cor_matrix)
      , z = cor_matrix
    )

    library(plotly)
    library(Wu)
    layout <- list(title = title)
    p <- plotly::plot_ly(colors = colorRamp(c("#DDFFF7", Wu::Blues(1))))
    p <- plotly::add_trace(
                     p
                   , type = trace1$type
                   , x = trace1$x
                   , y = trace1$y
                   , z = trace1$z)
    p <- plotly::layout(
                     p
                   , title = layout$title
                   , xaxis = list(side = "top")
                   , width = 1000
                   , height = 1200)
    p
}
