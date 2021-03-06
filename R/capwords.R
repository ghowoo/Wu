#' A capwords Function
#'
#' This function allows you to capitalize your words.
#' @param
#' @keywords capitalize
#' @export


capwords <- function(s, strict = FALSE) {
    s <- tolower(s)
    cap <- function(s) paste(toupper(substring(s, 1, 1))
                           , {
                               s <- substring(s, 2); if (strict) tolower(s) else s
                           }
                         , sep = ""
                         , collapse = " ")
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

CapWords <- capwords
