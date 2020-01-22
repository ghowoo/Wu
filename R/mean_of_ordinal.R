#' A function get mean of ordinals
#'
#' This function allows you to get the mean of predicted ordinals from Frank's lrm object.
#' yhat is the linear predictions on the first intercept (second of the ranks)
#' @param 
#' @keywords mean ordinal
#' @export


mean_of_ordinal <- function(yhat, intercepts, ordinals){
    yhat <- yhat - intercepts[1]
    mx <- outer(yhat, intercepts, FUN = "+")
    mx_ap <- plogis(mx)
    mx_p <- cbind(1, mx_ap) - cbind(mx_ap, 0)
    return(mx_p %*% ordinals)
}
