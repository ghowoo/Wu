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
    chi2 <- test$statistic
    N <- sum(test$observed)
    k <- min(dim(test$observed))
    V <- sqrt(chi2 / (N * (k - 1)))
    names(V) <- NULL
    return(V)
}
