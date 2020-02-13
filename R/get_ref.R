#' The get_ref Function
#'
#' This function allows you to all reference levels or median values from a dataframe.
#' @param 
#' @keywords get_ref
#' @export



get_ref <- function(data){
    vars <- colnames(data)
    rtn <- data[1, ]
    for (x in vars){
        if (class(data[[x]]) %in% c("numeric", "integer")){
            rtn[[x]] <- median(data[[x]], na.rm = TRUE)
        } else if (class(data[[x]]) %in% c("character", "factor")){
            lvls <- levels(data[[x]])
            rtn[[x]] <- factor(lvls[1], levels = lvls)
        } else {
            rtn[[x]] <- NA
        }
    }
    return(rtn)
}
