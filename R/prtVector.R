#' A print vector Function
#'
#' This function allows you to print a vector.
#' @param 
#' @keywords print vector
#' @export


prtVector <- function(v){
  return(cat(paste('\"', (trimws(v)), '\"', sep = ""), sep = "\n, "))
}
