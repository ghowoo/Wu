#' A function to get prevalence
#'
#' This function allows you to get prevalence by group.
#' @param
#' @keywords prevalence
#' @export

get_prop <- function(n, N){
    x <- prop.test(n, N)
    rtn <- c(n, N, x$estimate, x$conf.int)
    names(rtn) <- c("n", "N", "p", "ci_lower", "ci_higher")
    return(rtn)
}

#' @export
get_prop_str <- function(n, N, digits=1){
    rtn <- get_prop(n, N)
    rtn <- c(rtn
           , paste0(as.character(n)
                  , "/"
                  , as.character(N)
                  , collapse = "")
           , paste0(percent(rtn[3], digits)
                    , " ("
                    , percent(rtn[4], digits)
                      , ", "
                    , percent(rtn[5], digits)
                    , ")"
                    , collapse = ""
                  ))
    names(rtn)[6] <- "Observation"
    names(rtn)[7] <- "Prevalence"
    return(rtn)
}


#' @export
get_prevalence <- function(data, group, outcome, level=NULL){
    ot <- data[[deparse(substitute(outcome))]]
    gp <- data[[deparse(substitute(group))]]
    t <- as.data.frame.matrix(addmargins(table(gp, ot), margin=2))
    rtn <- apply(t[, 2:3]
               , 1
               , function(x) get_prop_str(x[1], x[2])
                 )
    rtn <- as.data.frame(t(rtn))
    colnames(rtn) <- paste0(colnames(rtn), " of ", colnames(t)[2])
    return(rtn)
}
