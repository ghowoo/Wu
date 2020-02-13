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
    format(round(x, decimals), nsmall = decimals, scientific = FALSE)
}


#' @export
fmtp <- function(x, sig = 4) {
    sig_value <- 1 / (10 ^ sig)
    y <- ifelse(x < sig_value, paste0("< ", Wu::fmtn(sig_value, sig)), Wu::fmtn(x, sig))
    return(y)
}
