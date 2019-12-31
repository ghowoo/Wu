#' A formula Function
#'
#' This function allows you to create a formula by an outcome and a vector of predictors.
#' @param 
#' @keywords formula
#' @export


wu_formula <- function(outcome, predictors){
    as.formula(paste0(
        outcome
      , " ~ "
      , paste0(
            predictors
          , sep = ""
          , collapse = "+")))
}
