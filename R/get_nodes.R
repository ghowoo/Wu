#' A get nodes Function
#'
#' This function allows you to get nodes
#' @param 
#' @keywords nodes
#' @export
#' @examples
#' get_nodes()

get_nodes <- function(obj, vars, top){
    df <- data.table::copy(obj)
    N <- nrow(df)
    nodes <- c(
        0
      , top
      , 1
      , paste0(top, "\n(", as.character(nrow(df)), ", 100%)")
      , top
      , NA
    )
    length_vars <- length(vars)
    for (i in 1:length_vars){
        vars_sum <- vars[0:i]
        vars_sum_upper <- vars[0:(i - 1)]
        na.omit(df, cols = vars_sum)
        fq <- df[
          , subtotal := .N, by = vars_sum_upper
        ][, .N, by = c(vars_sum, "subtotal")]
        setorderv(fq, vars_sum)
        for (j in 1:nrow(fq)){
            seq <- i
            name <- paste0(as.character(unlist(fq[, ..vars_sum][j])), sep = "", collapse = "_")
            name <- gsub("-| |/", "_", name)
            name_self <- name
            name_self <- gsub("-| |/", "_", name_self)
            name_parent <- ifelse(
                i == 1
                , top
                , paste0(as.character(unlist(fq[, ..vars_sum_upper][j])), sep = "", collapse = "_"))
            name_parent <- gsub("-| |/", "_", name_parent)
            value <- paste0(
                fq[[i]][j]
              , "\n("
              , as.character(fq$N[j])
              , ", "
              , Wu::percent(fq$N[j]/fq$subtotal[j], 1)
              , ")")
            size <- fq$N[j]/N
            if(!is.na(fq[[i]][j])){
                nodes <- rbind(nodes, c(i, name, size, value, name_self, name_parent))
            }
            
        }
    }
    nodes <- data.table::as.data.table(nodes)
    colnames(nodes) <- c("order", "node", "size", "label", "name_self", "name_parent")
    nodes$order <- as.numeric(nodes$order)
    nodes$size <- as.numeric(nodes$size)
    nodes[, id := 1:.N]
    return(nodes)
}
