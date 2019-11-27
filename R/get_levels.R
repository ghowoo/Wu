#' A function to get levels of all variables of a data.frame
#'
#' This function allows you to get levels of all variables of a data.frame.
#' @param 
#' @keywords levels
#' @export
#' @examples
#' get_levels()

get_levels <- function(data, vars = NULL){
    data <- droplevels(data)
    if(is.null(vars)){
        vars <- colnames(data)
    }
    num_levels <- unlist(
        lapply(vars, function(x){
            ifelse(is.null(levels(data[[x]])), 1, length(levels(data[[x]])))
        }))
    coefs <- unlist(lapply(vars, function(x){
        paste0(x, levels(data[[x]]), sep = "")
    }))
    var_levels <- unlist(
        lapply(vars, function(x){
            if(is.null(levels(data[[x]]))){
                x
            }else{
                levels(data[[x]])
                }
        }))
    var_labels <- unlist(
        lapply(vars, function(x){
            ifelse(Wu::label(data[[x]]) == "", x, Wu::label(data[[x]]))
        }))
    rtn <- data.table::data.table(
                    var_name = rep(vars, times = num_levels)
                  , var_order = rep(1:length(vars), times = num_levels)
                  , var_label = rep(var_labels, times = num_levels)
                  , coef_name = coefs
                  , coef_order = 1:length(coefs)
                  , var_level = var_levels
                    )
    rtn <- rtn[
      , rn := 1:.N, by = list(var_name)
    ][, var_label_o := ifelse(rn == 1, var_label, "")]
    return(rtn)
}
