#' A flowchart Function
#'
#' This function allows you to create a flowchart
#' @param 
#' @keywords flowchart
#' @export
#' @examples
#' flowchart()




flowchart <- function(data, vars, top = "Total", font_size = 30, icon_size = 80, edge_width = 10, plot_height = "800px", plot_width = "100%"){

    nodes <-  get_nodes(
        data
      , vars
      , top)

    nodes$font.size <- font_size
    nodes$size <- nodes$size * icon_size
    
    edges <- get_edges(nodes)
    edges$width <- edges$width * edge_width

    visNetwork::visNetwork(nodes, edges, height = plot_height, width = plot_width) %>%
        visOptions(highlightNearest = TRUE) %>%
        visLayout(randomSeed = 123) %>%
        visLayout(hierarchical = TRUE) %>%
        visHierarchicalLayout(direction = "LR", levelSeparation = 500) %>%
        addFontAwesome() %>%
        visOptions(collapse = TRUE)
}

