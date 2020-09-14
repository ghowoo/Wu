#' A correlation function
#'
#' This function allows you to get the correlation coefficients from a continuous variable against a set of continuous variables
#' @param
#' @keywords correlation, Peason's Cor, Spearman's rho
#' @export
table2_nbyn <- function(data, var, nvars, digits_p=1, digits_c=3){
    fx4 <- function(x, y, digits=3){
        t <- cor.test(x, y, method="pearson")
        c(paste0(format(t$estimate, digits=digits)
               , " ("
               , format(t$conf.int[1], digits=digits)
               , ", "
               , format(t$conf.int[2], digits=digits)
               , ")"
                 )
          , fmtp(t$p.value, digits))
    }
    ## spearman ci Bonett & Wright 2000
    fx5 <- function(x, y, digits=3){
        n <- sum(!is.na(x) * !is.na(y))
        t <- cor.test(x, y, method="spearman", exact = FALSE, continuity = TRUE)
        arctanh <- function(x){0.5 * log((1 + x) / (1 - x))}
        tanh <- function(x){(exp(2 * x) - 1) / (exp(2 * x) + 1)}
        sd <- sqrt((1 + t$estimate ^ 2) / (n - 3))
        c(paste0(format(t$estimate, digits=digits)
               , " ("
               , format(tanh(arctanh(t$estimate) - qnorm(0.975) * sd), digits=digits)
               , ", "
               , format(tanh(arctanh(t$estimate) + qnorm(0.975) * sd), digits=digits)
               , ")"
                 )
        , fmtp(t$p.value, digits)
          )
    }
    single <- function(data, var, nvar, digits_p, digits_c){
        x <- data[[var]]
        y <- data[[nvar]]
        tst1 <- fx4(x, y, digits = digits_c)
        tst2 <- fx5(x, y, digits = digits_c)
        n <- length(x)
        n_missing <- sum(is.na(x) * is.na(y))
        t <- data.table(""
        , "Correlation"
        , as.character(n)
        , paste0(as.character(n_missing)
               , " ("
               , Wu::percent(n_missing/n, digits_p)
               , ")"
                 )
        , tst1[1]
        , tst1[2]
        , tst2[1]
        , tst2[2]
          )
        colnames(t) <- c("Variable", "Level", "N", "Missing"
                       , "Pearson Cor", "p value",
                         "Spearman rho", "p value"
                     )
        t[["Variable"]][1] <- Wu::label(data[[nvar]])

        return(t[, ])
    }
    for (i in 1:length(nvars)) {
        ts <- single(data, var=var, nvars[i]
                          , digits_p=digits_p, digits_c=digits_c)
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
