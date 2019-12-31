#' A correlation between multiple variables
#'
#' This function allows you to calculate correlation between multiple variables mixed with continuous and categorical variables.
#' @param 
#' @keywords correlation
#' @export


cors_mixed <- function(df){
    stopifnot(inherits(df, c("data.frame", "data.table")))
    fun <- function(x, y){
        cor_mixed(
            subset(df, select = x, drop = TRUE)
          , subset(df, select = y, drop = TRUE))
    }
    fun <- Vectorize(fun)
    mtx  <- outer(
        1:ncol(df)
      , 1:ncol(df)
      , function(x, y) fun(x, y)
    )
    rownames(mtx) <- colnames(df)
    colnames(mtx) <- colnames(df)
    return(mtx)
}
