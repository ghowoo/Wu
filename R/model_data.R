#' A Function to get the data.frame from a model object
#'
#' This function allows you to catch the data.frame from a model object
#' @param
#' @keywords model data.frame
#' @export
#' @examples
#' model_data()


model_data <- function(x, ...){
    UseMethod("model_data")
}

model_data.lmerMod <- function(obj){
    obj@frame
}
