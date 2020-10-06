#' A Function to get the variable names from a model object
#'
#' This function allows you to catch the names of variables from a model object
#' @param
#' @keywords model variables


#' @export
model_vars <- function(x, ...){
    UseMethod("model_vars")
}


#' @export
model_vars.default <- function(obj){
    names(attr(obj$terms, "dataClasses"))
}


#' @export
model_vars.lmerMod <- function(obj){
    terms <- attr(obj@frame, "terms")
    attr(terms, "varnames.fixed")
}

#' @export
model_vars.glm <- function(obj){
    names(attr(obj$terms, "dataClasses"))
}

#' @export
model_vars.lm <- function(obj){
    names(attr(obj$terms, "dataClasses"))
}


#' @export
model_vars.lme <- function(obj){
    attr(obj$terms, "term.labels")
}
