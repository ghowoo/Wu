#' A flowchart Function
#'
#' This function allows you to create a flowchart
#' @param
#' @keywords flowchart
#' @export


flowchart <- function(data, vars, top = "Total", font_size = 15, icon_size = 40, edge_width = 2, plot_height = "800px", plot_width = "100%"){
    require(visNetwork)
    nodes <-  get_nodes(
        data
      , vars
      , top)
    nodes$font.size <- font_size
    nodes$size <- sqrt(nodes$size) * icon_size
    edges <- get_edges(nodes)
    edges$width <- sqrt(edges$width) * edge_width
    visNetwork::visNetwork(nodes, edges, height = plot_height, width = plot_width) %>%
        visOptions(highlightNearest = TRUE) %>%
        addFontAwesome()
}
