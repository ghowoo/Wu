#' A write columns Function
#'
#' This function allows you to write down your columns.
#' @param 
#' @keywords columns
#' @export
#' @examples
#' wrtColumns


wrtColumns <- function(obj,file){
    require(Wu)
    rtn <- data.frame(
        name = colnames(obj)
      , label = unlist(sapply(obj, label))
      , class = unlist(lapply(sapply(obj,class),function(x) x[length(x)]))
    )
    rownames(rtn) <- NULL
    write.csv(rtn,file=file,row.names = FALSE)
}
