#' A ci_to_str Function
#'
#' This function allows you to build a text string of confidence interver.
#' @param 
#' @keywords confidence interval, ci
#' @export
#' @examples
#' ci_to_str()

ci_to_str <- function(fit, lower, upper, digits = 2){
    str <- format(c(fit, lower, upper), digits = digits)
    return(paste0(str[1], "(", str[2], ",", str[3], ")"))
}
