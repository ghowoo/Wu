#' A correlation between two variables
#'
#' This function allows you to calculate correlation between two variables mixed with continuous and categorical variables.
#' If there are two continuous variables or one continuous and one categorical varialbe, the function returns r squared of the linear regression.
#' If there are two categorical variables, the function returns Cramer's V squared.
#' @param 
#' @keywords correlation
#' @export


cor_mixed <- function(var1, var2){
    if (inherits(var1, c("integer", "numeric")) &&
        inherits(var2, c("integer", "numeric"))) {
        return(summary(stats::lm(var1 ~ var2))[["r.squared"]])
    } else if (inherits(var1, c("integer", "numeric")) &&
               inherits(var2, c("factor", "character"))){
        return(summary(stats::lm(var1 ~ as.factor(var2)))[["r.squared"]])
    }
    else if (inherits(var2, c("integer", "numeric")) &&
             inherits(var1, c("factor", "character"))){
        return(summary(stats::lm(var2 ~ as.factor(var1)))[["r.squared"]])
    } else if (inherits(var1, c("factor", "character")) &&
               inherits(var2, c("factor", "character"))){
        return(Wu::cramersV(var1, var2)^2)
    }
}
