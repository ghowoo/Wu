#' A ci_to_str Function
#'
#' This function allows you to build a text string of confidence intervals. Notice that R round function goes to the even digit.
#' @param 
#' @keywords confidence interval, ci
#' @export
#' @examples
#' ci_to_str(fit = 1.004, lower = 1.003, upper = 1.005001, digits = 3)
#' ci_to_str(fit = 0.985, lower = .943, upper = 1.005001, digits = 2)

ci_to_str <- function(fit, lower, upper, digits = 2){
    fit <- round(fit, digits)
    lower <- round(lower, digits)
    upper <- round(upper, digits)
    length_max <- max(length(as.character(fit)), length(as.character(lower)), length(as.character(upper)))
    fit <- format(fit, nsmall = digits, justify = "right")
    lower <- format(lower, nsmall = digits, justify = "right")
    upper <- format(upper, nsmall = digits, justify = "right")
    return(paste0(
        fit
      , " ("
      , lower
      , ", "
      , upper
      , ")"
      , sep = ""
    ))
}
