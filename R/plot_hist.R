#' A plot_hist Function
#'
#' Describe a continuous variable with a histogram.
#' @param 
#' @keywords plot a histogram of a continuous variable
#' @export
#' @examples
#' x <- c(rnorm(100), rep(NA, 20))
#' plot_hist(x)

plot_hist <- function(x){
    txt <- Wu::desc_cont(x)
    txt <- paste0(
        c(paste0(txt[1:5], sep = "", collapse = "; ")
      , paste0(txt[6:12], sep = "", collapse = "; "))
      , sep = ""
      , collapse = "\n "
    )
    plot_ly(alpha = 0.6) %>%
        add_histogram(x) %>%
        config(displayModeBar = F) %>%
        layout(
            xaxis = list(title = paste0(label(x)))
          , yaxis = list(title = "Count")
          , title = list(text = txt
                       , side = "bottom")
        )
}
