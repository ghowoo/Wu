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
model_ci.glmerMod <- function(obj, method = "Wald", digits = 2, str_ref = "ref", transform=exp, ...){
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
    rtn <- rtn[, fit_not_null := as.numeric(!is.na(fit))
               ][, max_fit := max(fit_not_null), by = list(var_name)
                 ][max_fit == 1] # remove random effects
    rtn <- rtn[
        order(var_order, coef_order)
    ][, fit := transform(fit)
      ][, lower := transform(lower)
        ][, upper := transform(upper)
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


#' @export
model_ci.lme <- function(obj, digits = 2, pdigits = 4, str_ref = "ref", ...){
    require(nlme)
    require(data.table)
    fit <- coef(summary(obj))
    coef_names <- rownames(fit)
    rtn <- data.table::as.data.table(fit)
    rtn$coef_name <- coef_names
    rtn <- rtn[
      , lower := Value - qt(.975, DF) * Std.Error
    ][, upper := Value + qt(.975, DF) * Std.Error
      ][, ci_str := ci_to_str(Value, lower, upper, digits)]
    lvls <- Wu::get_levels(data = Wu::model_data(obj), vars = Wu::model_vars(obj))
    rtn <- merge(
        x = lvls
      , y = rtn
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all.x = TRUE
      , all.y = FALSE
    )
    rtn <- rtn[order(var_order, coef_order)
               ][is.na(Value), ci_str := str_ref
        ][, list(
           var_label_o
         , var_level
         , ci_str
         , `p-value`
         , coef_name
         , var_name
         , var_order
         , var_label
         , coef_order
         , rn
         , Value
         , lower
         , upper
       )]
    colnames(rtn) <- c(
        "Variable"
      , "Level"
      , "Estimate (95% CI)"
      , "p-value"
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
    rtn <- rtn[, `p-value` := Wu::fmtp(`p-value`, pdigits)
               ][is.na(`p-value`), `p-value` := ""]
    return(rtn)
}

#' @export
model_ci.lm <- function(obj, method = "Wald", digits = 2, str_ref = "ref", ...){
    fit <- coef(obj)
    fit <- data.table::data.table(
                           coef_name = names(fit)
                         , fit = fit
                       )
    ci <- stats::confint.default(obj, methods = "Wald")
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
model_ci.coxph <- function(obj
                         , data
                         , method = "Wald"
                         , digits = 2
                         , str_ref = "ref"
                         , ...){
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
    lvls <- Wu::get_levels(data=data, vars=model_vars(obj)[-1])
    rtn <- merge(
        x = lvls
      , y = rtn
      , by.x = "coef_name"
      , by.y = "coef_name"
      , all.x = TRUE
      , all.y = FALSE
    )
    rtn <- rtn[
        order(coef_order)
    ][, ci_str := ci_to_str(fit, lower, upper, digits)
      ][is.na(fit), ci_str := str_ref
        ]
    rtn <- rtn[, list(
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
model_ci.clmm <- function(obj, method = "Wald", digits = 2, str_ref = "ref", ...){
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
