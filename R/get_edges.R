#' A get_edges Function
#'
#' This function allows you to get edges
#' @param 
#' @keywords edge
#' @export



get_edges <- function(nodes){
    n1 <- nodes
    n1l <- n1
    n1r <- n1
    colnames(n1l) <- paste0(colnames(n1), "_l")
    colnames(n1r) <- paste0(colnames(n1), "_r")
    l1 <- merge(as.data.frame(n1l), as.data.frame(n1r))
    l1 <- data.table::as.data.table(l1)
    l1 <- l1[
        order_r - order_l == 1
    ][name_self_l == name_parent_r
      ][, from := id_l
        ][, to := id_r
          ][, arrows := "middle"
            ][, width := size_r]
    return(l1)
}


