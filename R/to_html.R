#' A html Function
#'
#' This function allows you to catch R output and export to html.
#' @param
#' @keywords html
#' @export


to_html <- function(x){
    title <- deparse(substitute(x))
    capture.output(x) %>%
        knitr::kable(caption = title, row.names = F, col.names = "") %>%
        Wu::styling() %>%
        kableExtra::column_spec(
                        1
                      , monospace = TRUE
                      , extra_css = "white-space:pre"
                      , include_thead = TRUE
                    )
}
