#' A Function to get the coefficent names from a model object
#'
#' This function allows you to catch the names of coefficients from a model object
#' @param
#' @keywords model coefficients


#' @export
model_coef <- function(x, ...){
    UseMethod("model_coef")
}
#' @export
model_coef.lmerMod <- function(obj){
    attr(obj@pp$X, "dimnames")[[2]]
}
