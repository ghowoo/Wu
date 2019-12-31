#' A delete columns that all rows have only one value
#'
#' This function allows you to delete all columns in a data.frame or data.table that have only one unique values (missing value included)
#' @param 
#' @keywords delete single value columns
#' @export


delete_single_value_column <- function(obj){
    if (!(inherits(obj, "data.table"))){
        obj <- data.table::as.data.table(obj)
    }
    variables_not_unique <- unlist(lapply(obj, function(x){
        length(unique(x)) > 1
    }))
    obj_return <- obj[, variables_not_unique, with = FALSE]
    return(obj_return)
}
