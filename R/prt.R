#' A desc_cont Function
#'
#' Print a table
#' @param
#' @keywords print a table to html
#' @export


prt <- function(dt, ...){
    knitr::kable(dt, align = "lrrrrrrrr", ...) %>% Wu::styling()
}
