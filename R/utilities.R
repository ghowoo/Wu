#' Utility Functions
#'
#' This function allows you to negate the \%in\% function.
#' @param 
#' @keywords negate \%in\%
#' @export
#' @examples
#' \%notin\%
#'
#' 

Utilities <- function(){
    print("Utilities")
}

`%notin%` <- Negate(`%in%`)


