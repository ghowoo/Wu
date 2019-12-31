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
drop_levels.data.frame <- function(x, except = NULL, exclude, ...){
    ix <- vapply(x, is.factor, NA)
    if (!is.null(except))
        ix[except] <- FALSE
    x[ix] <- if (missing(exclude))
        lapply(x[ix], drop_levels)
    else lapply(x[ix], drop_levels, exclude = exclude)
    x
}

#' @export
drop_levels.factor <- function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...){
    lbl <- Wu::label(x)
    x <- factor(x, exclude = exclude)
    Wu::label(x) <- lbl
    return(x)
}
