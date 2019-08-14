#' Deming Regression Function
#'
#' This function allows you to run Deming regression.
#' @param 
#' @keywords Deming regression
#' @export
#' @examples
#' deming_regression()

deming_regression <- function(x, y, lambda = 1){
    mu <- sum((x - mean(x))^2)
    q <- sum((y - mean(y))^2)
    p <- sum((x - mean(x)) * (y - mean(y)))
    b <- ((lambda * q - mu) + sqrt((mu - lambda * q)^2 + 4 * lambda * p^2)) / (2 * lambda * p)
    a <- mean(y) - b * mean(x)
    return(c(a, b))
}
