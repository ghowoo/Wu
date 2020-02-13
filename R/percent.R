#' A percent Function
#'
#' This function allows you to format a number as a percentage.
#' @param 
#' @keywords format percent percentage
#' @export


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format, ...), "%")
}


#' @export
fmtn <- function(x, decimals = 2){
    format(round(x, decimals), nsmall = decimals)
}


#' @export
fmtp <- function(pvals, sig.limit = .0001, digits = 4, html = FALSE) {

    roundr <- function(x, digits = 1) {
        res <- sprintf(paste0('%.', digits, 'f'), x)
        zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
        res[res == paste0('-', zzz)] <- zzz
        res
    }

    sapply(pvals, function(x, sig.limit) {
        if (x < sig.limit)
            if (html)
                return(sprintf('&lt; %s', format(sig.limit))) else
                                                                  return(sprintf('< %s', format(sig.limit)))
        if (x > .1)
            return(roundr(x, digits = 2)) else
                                              return(roundr(x, digits = digits))
    }, sig.limit = sig.limit)
}
