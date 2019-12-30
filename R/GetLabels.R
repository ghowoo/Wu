#' A GetLabels Function
#'
#' This function allows you to get all labels from a dataframe.
#' @param 
#' @keywords labels
#' @export


GetLabels <- function(obj){
  rtn <- unlist(lapply(obj, label()))
  rtn  <- cbind(colnames(obj),rtn)
  rownames(rtn) <- NULL
  rtn <- as.data.frame(rtn)
  colnames(rtn) <- c("variable","label")
  return(rtn)
}
