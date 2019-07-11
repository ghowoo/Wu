#' A heatmap Function
#'
#' This function allows you to create a heatmap.
#' @param 
#' @keywords heatmap
#' @export
#' @examples
#' heatmap()

heatmap <- function(obj, title = "", width = 800, height = 800){
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
    vals <- unique(c(0, scales::rescale(c(cor_matrix))))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    layout <- list(title = title)
    p <- plotly::plot_ly(colorscale = colz)
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
                   , width = width
                   , height = height)
    p
}
