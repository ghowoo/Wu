#' A desc_cont Function
#'
#' Describe a continuous variable.
#' @param 
#' @keywords describe continuous variable
#' @export
#' @examples
#' x <- c(rnorm(100), rep(NA, 20))
#' desc_cont(x)

desc_cont <- function(x, digits_p = 1, digits_c = 3) {
    n <- length(x)
    n_missing <- sum(is.na(x))
    n_nonmissing <- n - n_missing
    c(
        paste0("N: ", as.character(n))
      , paste0(
            "Non-Missing: "
          , as.character(n_nonmissing)
          , " ("
          , Wu::percent(n_nonmissing / n, digits_p)
          , ")"
        )
      , paste0(
            "Missing: "
          , as.character(n_missing)
          , " ("
          , Wu::percent(n_missing / n, digits_p)
          , ")"
        )
      , paste0(
            "Mean: "
          , format(mean(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "SD: "
          , format(sd(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Min: "
          , format(min(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Q1: "
          , format(quantile(x, na.rm = TRUE, p = .25), digits = digits_c)
        )
      , paste0(
            "Median: "
          , format(quantile(x, na.rm = TRUE, p = .5), digits = digits_c)
        )
      , paste0(
            "Q3: "
          , format(quantile(x, na.rm = TRUE, p = .75), digits = digits_c)
        )
      , paste0(
            "Max: "
          , format(max(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "IQR: "
          , format(IQR(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Range: "
          , format(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), digits = digits_c)
        )
    )
}
