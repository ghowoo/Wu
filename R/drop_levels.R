#' The Drop Levels Functions
#'
#' This function allows you to drop all unused levels in a data.frame.
#' @param 
#' @keywords drop levels
#' @export


#' @export
drop_levels <- function(df){
    UseMethod("drop_levels")
}

#' @export
drop_levels.data.frame <- function(data, except = NULL, exclude, ...){
    fac_id <- vapply(data, is.factor, NA)
    if (!is.null(except)) 
        fac_id[except] <- FALSE
    vars <- colnames(data)[fac_id]
    lapply(vars
         , function(x){
             data[[x]] <- Wu::drop_levels(data[[x]])
         }
         )
    return(data)
}

#' @export
drop_levels.factor <- function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...){
    lbl <- Wu::label(x)
    x <- factor(x, exclude = exclude)
    Wu::label(x) <- lbl
    return(x)
}
