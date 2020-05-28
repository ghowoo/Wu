#' A desc_cont Function
#'
#' Describe a continuous variable.
#' @param
#' @keywords describe continuous variable
#' @examples
#' x <- c(rnorm(100), rep(NA, 20))
#' desc_cont(x)
#' @export
desc_cont <- function(x, digits_p = 1, digits_c = 3) {
    n <- length(x)
    n_missing <- sum(is.na(x))
    n_nonmissing <- n - n_missing
    n_unique <- length(unique(x))
    c(
        paste0("N: ", as.character(n))
      , paste0(
            "Non-Missing: "
          , as.character(n_nonmissing)
          , " ("
          , Wu::percent(n_nonmissing / n, digits_p)
          , ")"
        )
      , paste0(
            "Missing: "
          , as.character(n_missing)
          , " ("
          , Wu::percent(n_missing / n, digits_p)
          , ")"
        )
      , paste0("Unique: ", as.character(n_unique), " (", Wu::percent(n_unique/n, digits_p), ")")
      , paste0(
            "Mean: "
          , format(mean(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "SD: "
          , format(sd(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Min: "
          , format(min(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Q1: "
          , format(quantile(x, na.rm = TRUE, p = .25), digits = digits_c)
        )
      , paste0(
            "Median: "
          , format(quantile(x, na.rm = TRUE, p = .5), digits = digits_c)
        )
      , paste0(
            "Q3: "
          , format(quantile(x, na.rm = TRUE, p = .75), digits = digits_c)
        )
      , paste0(
            "Max: "
          , format(max(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "IQR: "
          , format(IQR(x, na.rm = TRUE), digits = digits_c)
        )
      , paste0(
            "Range: "
          , format(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), digits = digits_c)
        )
    )
}

#' @export
desc_cat <- function(x, digits_p=1, digits_c=3){
    n <- length(x)
    n_missing <- sum(is.na(x))
    n_nonmissing <- n - n_missing
    n_unique <- length(unique(x))
    t <- table(x, useNA="ifany")
    t <- t[order(-t)][1:min(7, length(t))]
    if(!is.null(attr(t, "dimnames")[[1]])){attr(t, "dimnames")[[1]] <- substring(attr(t, "dimnames")[[1]], first=1, last=20)}
    cats <- unlist(lapply(1:length(t), function(x){
        paste0(dimnames(t)[[1]][x]
             , ": "
             , as.character(t[x])
             , " ("
             , Wu::percent( t[x] / n, digits_p)
             , ")", collapse = "")
    }))
    c(paste0("N: ", as.character(n))
    , paste0("Non-Missing: ", as.character(n_nonmissing), " (", Wu::percent(n_nonmissing/n, digits_p), ")")
    , paste0("Missing: ", as.character(n_missing), " (", Wu::percent(n_missing/n, digits_p), ")")
    , paste0("Unique: ", as.character(n_unique), " (", Wu::percent(n_unique/n, digits_p), ")")
    , paste0("Min: ", min(as.character(x), na.rm = TRUE))
    , paste0("Max: ", max(as.character(x), na.rm = TRUE))
    , cats
      )
}


#' @export
desc_var <- function(x){
    ifelse(class(x) %in% c("integer", "numeric") & length(unique(x)) > 7
         , return(Wu::desc_cont(x))
         , return(Wu::desc_cat(x)))
}
