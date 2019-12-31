#' A html Function
#'
#' This function allows you to catch R output and export to html.
#' @param
#' @keywords html
#' @export


html <- function(x) UseMethod("html")


html.summary.gam <- function(obj){
    align = "lrrrrrrrrrrrrrrr"

    v1 <-  c(
        x$family
      , "Formula:"
    )
    
    if (is.list(x$formula)) 
        v1 <- c(v1, unlist(obj$formula))
    else v1 <- c(v1, obj$formula)

    knitr::kable(v1) %>% styling()
    
    if (length(x$p.coeff) > 0) {
        ## cat("\nParametric coefficients:\n")
        ## printCoefmat(x$p.table, digits = digits, signif.stars = signif.stars, 
        ##              na.print = "NA", ...)
        knitr::kable(
                   obj$p.table
                 , align = align
                 , caption = "Approximate significance of smooth terms"
               ) %>% styling()
    }

    knitr::kable(
               obj$s.table
             , align = align
             , caption = "Approximate significance of smooth terms"
           ) %>% styling()

    v2 <- c(NULL)
    
    if (!is.null(x$rank) && x$rank < x$np) 
    v2 <- c(v2, paste0("Rank: ", x$rank, "/", x$np, sep = ""))
    if (!is.null(x$r.sq)) 
    v2 <- v(v2, paste0("R-sq.(adj) = ", formatC(x$r.sq, digits = 3, width = 5), "  "))
    if (length(x$dev.expl) > 0) 
    v2 <- c(v2, paste0("Deviance explained = ", formatC(x$dev.expl * 100, 
            digits = 3, width = 4), "%", sep = ""))
    if (!is.null(x$method) && !(x$method %in% c("PQL", "lme.ML", 
                                                "lme.REML")))
        v2 <- c(v2, paste0(x$method, " = ", formatC(x$sp.criterion, digits = 5), 
            sep = ""))
        v2 <- c(v2, paste0("  Scale est. = ", formatC(x$scale, digits = 5, width = 8,flag = "-"), "  n = ", x$n, "\n", sep = ""))
    knitr::kable(v2) %>% styling()

}

