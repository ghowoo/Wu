#' A Function to get coefficients and their confidence intervals from a model object
#'
#' This function allows you to get coefficients and their confidence intervals from a model object.
#' @param 
#' @keywords confidence interval, coefficient
#' @export


model_ci <- function(obj, digits = 2, ...){
    UseMethod("model_ci")
}

#' @export
model_ci.lmerMod <- function(obj, method = "Wald", digits = 2, str_ref = "ref", ...){
    fit <- lme4::fixef(obj)
    fit <- data.table::data.table(
                           coef_name = names(fit)
                         , fit = fit
                       )
    ci <- lme4::confint.merMod(obj, method = method)
    ci <- data.table::data.table(
                          coef_name = rownames(ci)
                        , lower = ci[, 1]
                        , upper = ci[, 2]
                      )
    rtn <- merge(
        x = fit
      , y = ci
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all = FALSE
    )
    lvls <- Wu::get_levels(
                    data = Wu::model_data(obj)
                  , vars = Wu::model_vars(obj)[-1]
                )
    rtn <- merge(
        x = lvls
      , y = rtn
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all.x = TRUE
      , all.y = FALSE
    )
    rtn <- rtn[
        order(var_order, coef_order)
    ][, ci_str := ci_to_str(fit, lower, upper, digits)
      ][is.na(fit), ci_str := str_ref
        ][, list(
           var_label_o
         , var_level
         , ci_str
         , coef_name
         , var_name
         , var_order
         , var_label
         , coef_order
         , rn
         , fit
         , lower
         , upper
       )]
    colnames(rtn) <- c(
        "Variable"
      , "Level"
      , "Estimate (95% CI)"
      , "coef_name"
      , "var_name"
      , "var_order"
      , "var_label"
      , "coef_order"
      , "rn"
      , "fit"
      , "lower"
      , "upper"
    )
    return(rtn)
}



#' @export
model_ci.glm <- function(obj, method = "Wald", digits = 2, str_ref = "ref", ...){
    fit <- coef(obj)
    fit <- data.table::data.table(
                           coef_name = names(fit)
                         , fit = exp(fit)
                       )
    ci <- exp(stats::confint.default(obj, methods = "Wald"))
    ci <- data.table::data.table(
                          coef_name = rownames(ci)
                        , lower = ci[, 1]
                        , upper = ci[, 2]
                      )
    rtn <- merge(
        x = fit
      , y = ci
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all = FALSE
    )
    lvls <- Wu::get_levels(
                    data = Wu::model_data(obj)
                  , vars = Wu::model_vars(obj)[-1]
                )
    rtn <- merge(
        x = lvls
      , y = rtn
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all.x = TRUE
      , all.y = FALSE
    )
    rtn <- rtn[
        order(var_order, coef_order)
    ][, ci_str := ci_to_str(fit, lower, upper, digits)
      ][is.na(fit), ci_str := str_ref
        ][, list(
           var_label_o
         , var_level
         , ci_str
         , coef_name
         , var_name
         , var_order
         , var_label
         , coef_order
         , rn
         , fit
         , lower
         , upper
       )]
    colnames(rtn) <- c(
        "Variable"
      , "Level"
      , "Estimate (95% CI)"
      , "coef_name"
      , "var_name"
      , "var_order"
      , "var_label"
      , "coef_order"
      , "rn"
      , "fit"
      , "lower"
      , "upper"
    )
    return(rtn)
}
