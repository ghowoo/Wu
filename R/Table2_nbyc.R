#' A correlation function
#'
#' This function allows you to get the basis discriptive statistics from a continuous variable against a set of categorical variables
#' @param
#' @keywords mean, ANOVA, Kruskal Wallis
#' @export
Table2_nbyc <- function(data, var, groups, digits_p=1, digits_c=3){
    fx1 <- function(x, digits=3){
        if(min(is.na(x)) == 1){
            return(NA)
        }else{
            paste0(format(mean(x, na.rm = TRUE), digits=digits)
                 , " ("
                 , format(sd(x, na.rm=TRUE), digits = digits)
                 , ")"
                   )}
    }

    fx2 <- function(x, digits=3){
        if(min(is.na(x)) == 1){
            return(NA)
        }else{
            paste0(format(median(x, na.rm = TRUE), digits=digits)
                 , " ("
                 , format(quantile(x, na.rm=TRUE, p=0.25), digits = digits)
                 , ", "
                 , format(quantile(x, na.rm=TRUE, p=0.75), digits = digits)
                 , ")"
                   )
        }
    }


    fx3 <- function(x, digits=3){
        if(min(is.na(x)) == 1){
            return(NA)
        }else{
            paste0(if(sum(!is.na(x)) < 2){""
                   }else{
                       format(density(x)$x[which.max(density(x)$y)], digits=digits)}
                 , " ("
                 , format(min(x, na.rm=TRUE), digits = digits)
                 , ", "
                 , format(max(x, na.rm=TRUE), digits = digits)
                 , ")"
                   )
        }
    }
    single_n <- function(data, var, group, digits_p, digits_c){
        t <- lapply(levels(droplevels(data[[group]])), function(lvl){
            x <- data[data[[group]] == lvl][[var]]
            n <- length(x)
            n_missing <- sum(is.na(x))
            c(""
            , lvl
            , as.character(n)
            , paste0(as.character(n_missing)
                   , " ("
                   , Wu::percent(n_missing/n, digits_p)
                   , ")"
                     )
            , fx1(x, digits = digits_c)
            , fx2(x, digits = digits_c)
            , fx3(x, digits = digits_c)
              )
        })
        t <- do.call(rbind, t)
        t <- as.data.table(t)
        colnames(t) <- c("Variable", "Level", "N", "Missing"
                   , "Mean(SD)", "Median(IQR)", "Mode(Range)"
                     )
        t[["Variable"]][1] <- Wu::label(data[[group]])
        var_1 <- t[["Level"]][t[["N"]] <= 1]
        tst1 <- oneway.test(as.formula(paste0(var, "~", group))
                         , data=data[data[[group]] %notin% var_1]
                         , na.action="na.omit"
                         , var.equal = TRUE)
        tst2 <- kruskal.test(as.formula(paste0(var, "~", group))
                           , data=data[data[[group]] %notin% var_1]
                           , na.action="na.omit")
        t[["p(ANOVA)"]] <- ""
        t[["p(KW)"]] <- ""
        t[["p(ANOVA)"]][1] <- Wu::fmtp(tst1$p.value, sig=4)
        t[["p(KW)"]][1] <- Wu::fmtp(tst2$p.value, sig=4)
        return(t[, c("Variable", "Level", "N", "Missing"
                   , "Mean(SD)", "p(ANOVA)"
                   , "Median(IQR)", "p(KW)", "Mode(Range)"
                     )])
    }
    for (i in 1:length(groups)) {
        ts <- single_n(data, var=var, groups[i]
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
