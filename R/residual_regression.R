#' A residual regression function
#'
#' Deming regression on two residuals of linear regression
#' @param 
#' @keywords residual regression
#' @export

residual_regression <- function(data,  outcome_x, outcome_y, predictors){
    get_residual <- function(data, outcome, predictors){
        lm(Wu::wu_formula(outcome, predictors), data)$residuals
    }
    res_x <- get_residual(data, outcome_x, predictors)
    res_y <- get_residual(data, outcome_y, predictors)
    return(Wu::deming_regression(x = res_x, y = res_y))
}
