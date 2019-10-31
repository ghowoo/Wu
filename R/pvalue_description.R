#' A function describes how p values being calcualted
#'
#' This function describes how p values being calcualted
#' @param 
#' @keywords p-value
#' @export
#' @examples
#' describe_pvalue()

pvalue_description <- function(...
                             , label = c(
                                   "The default test for categorical variables is chi-squared test with Yates correction for continuity."
                                 , "The default test for continuous variables is one-way ANOVA on means assuming equal variance. Kruskal-Wallis rank sum test has been applied for the variables where median and interquartile range (IQR) are show on the table.")
                             , notation = "symbol"
                               ) {
    kableExtra::add_footnote(
                    ...
                  , label = label
                  , notation = notation
                )
}
