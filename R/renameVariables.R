#' A renameVariables Function
#'
#' This function allows you to rename your variable names.
#' @param 
#' @keywords rename variable name
#' @export
#' @examples
#' renameVariables()

renameVariables <- function(obj, lst){
    ColNames <- colnames(obj)
    Index_A <- ColNames %in% names(lst)
    Index_B <- match(ColNames,names(lst))
    Index_B <- Index_B[!is.na(Index_B)]
    ColNames[Index_A]  <- lst[Index_B]
    colnames(obj) <- ColNames
    return(obj)
}
