#' A Function to get the data.frame from a model object
#'
#' This function allows you to catch the data.frame from a model object
#' @param
#' @keywords model data.frame

#' @export
model_data <- function(x, ...){
    UseMethod("model_data")
}

#' @export
model_data.lmerMod <- function(obj){
    obj@frame
}

#' @export
model_data.glm <- function(obj){
    obj$data
}

#' @export
model_data.lme <- function(obj){
    obj$data
}


#' @export
model_data.default <- function(obj){
    obj$data
}
