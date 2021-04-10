#' A remove data function
#'
#' This function allows you to remove all objects in the environment except functions
#' @param
#' @keywords remove data
#' @export

rm_dt <- function(){
    lst <- ls(envir = parent.frame())
    list=lst[sapply(lst, function(x) {max(class(get(x)) %notin% c("function")) == 1})]
    rm(list=list, envir = parent.frame())
}
