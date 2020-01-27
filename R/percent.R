#' A percent Function
#'
#' This function allows you to format a number as a percentage.
#' @param 
#' @keywords format percent percentage
#' @export


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format, ...), "%")
}


#' @export
fmtn <- function(x, decimals = 2){
    format(round(x, decimals), nsmall = decimals)
}
