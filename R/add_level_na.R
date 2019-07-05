#' A add_level_na function
#'
#' This function allows you to add the level "not available" to a factor variable.
#' @param 
#' @keywords na, not available, level
#' @export
#' @examples
#' add_level_na()

add_level_na <- function(x){
    x <- factor(x)
    levels(x) <- c(levels(x), "not available")
    x[is.na(x)] <- "not available"
    x <- droplevels(x)
    l1 <- levels(x)
    l1 <- l1[-length(l1)]
    x <- factor(x, levels = l1)
    return(x)
}
