#' A print column names Function
#'
#' This function allows you to print column names.
#' @param 
#' @keywords print column names
#' @export
#' @examples
#' prtColNames()

PrtColNames <- function(obj){
  return(cat(paste('\"',(trimws(colnames(obj))),'\"',sep=""),sep="\n,"))
}
