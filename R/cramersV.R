#' A Cramer's V function
#'
#' This function allows you to calcualte Cramer's V
#' @param 
#' @keywords Cramer's V
#' @export
#' @examples
#' cramersV()


cramersV <- function(..., correct = FALSE) {
    test <- chisq.test(...)
    N <- sum(test$observed)
    k <- min(dim(test$observed))
    V <- sqrt(test$statistic / (N * (k - 1)))
    names(V) <- NULL
    return(V)
}
