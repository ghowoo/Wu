#' A function to get odds ratios on a binary outcome
#'
#' This function allows you to get related measurements on a binary outcome
#' @param
#' @keywords odds ratio, univariate analysis
#' @export
get_or <- function(y, x, data, digits=2){
    m <- glm(as.formula(paste0(y, "~", x)), data=data, family = "binomial")
    rtn <- Wu::model_ci(m, digits=digits)
    rtn <- cbind(rtn, tab_freq(outcome=y, group=x, data=data, digits = digits))
    rtn$pvalue <- ""
    rtn$pvalue[1] <- fmtp(anova(m, test="Chisq")[2, 5], 4)
    rtn$pvalue_n <- anova(m, test="Chisq")[2, 5]
    rtn$r2_nagelkerke <- ""
    rtn$r2_nagelkerke[1] <- fmtp(performance::r2_nagelkerke(m), sig = 4)
    rtn$r2_nagelkerke_n <- performance::r2_nagelkerke(m)
    return(rtn)
}

#' @export
get_ors <- function(outcome, predictors, data, digits){
    l <- lapply(predictors, function(g){get_or(outcome, g, data=data, digits=2)})
    rtn <- do.call(rbind, l)
    rtn <- rtn[
      , seq := 1:.N
    ][, r2 := as.numeric(r2_nagelkerke)
      ][, r2 := max(r2, na.rm = TRUE), by = .(var_name)
        ][order(-r2, seq)
          ][, seq := NULL
            ][, r2 := NULL]
    return(rtn)
}
