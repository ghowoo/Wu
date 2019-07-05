#' A correlation between two variables
#'
#' This function allows you to calculate correlation between two variables mixed with continuous and categorical variables.
#' @param 
#' @keywords correlation
#' @export
#' @examples
#' cor_mixed()

cor_mixed <- function(var1, var2){
    if (inherits(var1, c("integer", "numeric")) &&
        inherits(var2, c("integer", "numeric"))) {
        return(stats::cor(var1, var2, use = "pairwise.complete.obs"))
    } else if (inherits(var1, c("integer", "numeric")) &&
               inherits(var2, c("factor", "character"))){
        return(summary(stats::lm(var1 ~ as.factor(var2)))[["r.squared"]])
    }
    else if (inherits(var2, c("integer", "numeric")) &&
             inherits(var1, c("factor", "character"))){
        return(summary(stats::lm(var2 ~ as.factor(var1)))[["r.squared"]])
    } else if (inherits(var1, c("factor", "character")) &&
               inherits(var2, c("factor", "character"))){
        return(cramersV(var1, var2))
    }
}
