#' A print column names Function
#'
#' This function allows you to print column names.
#' @param 
#' @keywords print column names
#' @export


prtColNames <- function(data, type = NULL){
    if(is.null(type)){
        return(cat(paste('\"',(trimws(colnames(data))),'\"',sep=""),sep="\n,"))
    } else if(type=="n"){
        nums <- unlist(lapply(data, is.numeric))
        return(cat(paste('\"',(trimws(colnames(data)[nums])),'\"',sep=""),sep="\n,"))
    } else if(type=="c"){
        nums <- unlist(lapply(data, function(x){is.numeric(x) == FALSE}))
        return(cat(paste('\"',(trimws(colnames(data)[nums])),'\"',sep=""),sep="\n,"))
    } else {
        return(cat(paste('\"',(trimws(colnames(data))),'\"',sep=""),sep="\n,"))
    }
}
