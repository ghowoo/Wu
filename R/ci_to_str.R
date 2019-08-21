#' A ci_to_str Function
#'
#' This function allows you to build a text string of confidence intervals. Notice that R round function goes to the even digit.
#' @param 
#' @keywords confidence interval, ci
#' @export
#' @examples
#' ci_to_str(fit = 1.004, lower = 1.003, upper = 1.005001, digits = 3)
#' ci_to_str(fit = 0.985, lower = .943, upper = 1.005001, digits = 2)

ci_to_str <- function(fit, lower, upper, digits = 3){
    str <- format(c(fit, lower, upper), digits = digits)
    min_length <- min(nchar(trimws(gsub("\\.", "", str))))
    if (min_length < digits){
        str <- format(
            round(c(fit, lower, upper), digits - min_length)
          , digits = digits
          , nsmall = digits - min_length
        )
    }
    return(paste0(str[1], "(", str[2], ",", str[3], ")"))
}
