#' A style Function
#'
#' This function allows you to style your html output.
#' @param 
#' @keywords style html
#' @export
#' @examples
#' stl()

stl <- function(
                full_width = FALSE
              , bootstrap_options = c("striped", "hover", "condensed", "responsive")
              , position = "left"
              , fixed_thead = TRUE
              , ...
                ){
    kableExtra::kable_styling(
                    full_width = full_width
                  , bootstrap_options = bootstrap_options
                  , position = position
                  , fixed_thead = fixed_thead
                  , ...
                )
}
