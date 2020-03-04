#' A css function
#'
#' This function allows you to create a css style text
#' @param 
#' @keywords css html
#' @export

css <- function(){
    library(Wu)
    txt <- readLines(file.path(path.package("Wu"), "blues.css"))
    txt <- paste0(c("<style>", txt, "</style>"), collapse = "  \n")
    cat(txt)
}


#' @export

css_menu <- function(){
    library(Wu)
    txt <- readLines(file.path(path.package("Wu"), "menubar.css"))
    txt <- paste0(c("<style>", txt, "</style>"), collapse = "  \n")
    cat(txt)
}

#' @export
options_menu <- function(){
    knitr::opts_chunk$set(
                          echo = FALSE
                        , message = FALSE
                        , tidy = TRUE
                        , results = "asis"
                        , warning = FALSE
                        , code_folding = "hide"
                        , fig.width = 12
                        , fig.height = 8
                      )
    options(knitr.kable.NA = "")
    theme_update(
        axis.text = element_text(size = 10)
      , axis.title = element_text(size = 10)
    )
}
