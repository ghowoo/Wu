#' A heatmap Function
#'
#' This function allows you to create a heatmap.
#' @param 
#' @keywords heatmap
#' @export


heatmap <- function(
                    obj
                  , title = "Matrix of R-squared"
                  , width = 800
                  , height = 800
                  , pallete = "Blues"){
    obj <- Wu::delete_single_value_column(obj)
    obj <- as.data.frame(obj)
    cor_matrix <- Wu::cors_mixed(obj)
    ## cor_matrix <- cor_matrix^2
    cor_matrix <- round(cor_matrix, 2)
    trace1 <- list(
        type = "heatmap"
      , x = colnames(cor_matrix)
      , y = colnames(cor_matrix)
      , z = cor_matrix
    )

    library(plotly)
    library(Wu)
    ## vals <- unique(c(0, scales::rescale(c(cor_matrix))))
    vals <- sort(unique(c(0, c(cor_matrix))))
    ## o <- order(vals, decreasing = FALSE)
    ## cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    ## cols <- heat.colors(length(vals))
    cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, pallete)[2:9])(length(vals))
    colz <- setNames(data.frame(vals, cols), NULL)

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
                   , xaxis = list(side = "bottom")
                   , width = width
                   , height = height)
    p
}
