#' A css function
#'
#' This function allows you to create a css style text
#' @param 
#' @keywords css html
#' @export
#' @examples
#' css()


css <- function(){
    library(Wu)
    txt <- readLines(file.path(path.package("Wu"), "blues.css"))
    txt <- paste0(c("<style>", txt, "</style>"), collapse = "  \n")
    cat(txt)
}

