#' A Function to get the variable names from a model object
#'
#' This function allows you to catch the names of variables from a model object
#' @param
#' @keywords model variables
#' @export
#' @examples
#' model_vars()


model_vars <- function(x, ...){
    UseMethod("model_vars")
}

model_vars.lmerMod <- function(obj){
    terms <- attr(obj@frame, "terms")
    attr(terms, "varnames.fixed")
}
