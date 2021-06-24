#' A correlation function
#'
#' This function allows you to get the correlation coefficients from a continuous variable against a set of continuous variables
#' @param
#' @keywords correlation, Peason's Cor, Spearman's rho
#' @export
Table2_nbyn <- function(data, var, nvars, digits_p=3, digits_n=3, digits_pct = 1){
    fx4 <- function(x, y){
        t <- cor.test(x, y, method="pearson")
        c(paste0(format(round(t$estimate, digits_n), nsmall=digits_n)
               , " ("
               , format(round(t$conf.int[1], digits_n), nsmall=digits_n)
               , ", "
               , format(round(t$conf.int[2], digits_n), nsmall=digits_n)
               , ")"
                 )
          , fmtp(t$p.value, digits_p))
    }
    ## spearman ci Bonett & Wright 2000
    fx5 <- function(x, y){
        n <- sum(!is.na(x) * !is.na(y))
        t <- cor.test(x, y, method="spearman", exact = FALSE, continuity = TRUE)
        arctanh <- function(x){0.5 * log((1 + x) / (1 - x))}
        tanh <- function(x){(exp(2 * x) - 1) / (exp(2 * x) + 1)}
        sd <- sqrt((1 + t$estimate ^ 2) / (n - 3))
        c(paste0(format(round(t$estimate, digits_n), nsmall=digits_n)
               , " ("
               , format(round(tanh(arctanh(t$estimate) - qnorm(0.975) * sd), digits_n), nsmall=digits_n)
               , ", "
               , format(round(tanh(arctanh(t$estimate) + qnorm(0.975) * sd), digits_n), nsmall=digits_n)
               , ")"
                 )
        , fmtp(t$p.value, digits_p)
          )
    }
    single <- function(data, var, nvar){
        x <- data[[var]]
        y <- data[[nvar]]
        tst1 <- fx4(x, y)
        tst2 <- fx5(x, y)
        n <- length(x)
        n_missing <- sum(is.na(x) | is.na(y))
        t <- data.table(
          ifelse(label(data[[var]]) %in% "", var, label(data[[var]]))
        , ifelse(label(data[[nvar]]) %in% "", nvar, label(data[[nvar]]))
        , as.character(n)
        , as.character(n - n_missing)
        , paste0(as.character(n_missing)
               , " ("
               , Wu::percent(n_missing/n, digits_pct)
               , ")"
                 )
        , tst1[1]
        , tst1[2]
        , tst2[1]
        , tst2[2]
          )
        colnames(t) <- c("Variable x", "Variable y", "N", "n","Missing"
                       , "Pearson's Cor", "p value",
                         "Spearman's rho", "p value"
                     )
        t[["Variable"]][1] <- Wu::label(data[[nvar]])

        return(t[, ])
    }
    for (i in 1:length(nvars)) {
        ts <- single(data, var=var, nvars[i])
        if (i == 1) {
            rtn <- ts
        }
        else {
            rtn <- rbind(rtn, ts)
        }
    }
    rownames(rtn) <- NULL
    return(rtn)
}
