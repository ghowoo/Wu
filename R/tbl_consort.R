#' A consort table function
#'
#' This function allows you to create a table of a consort
#' @param 
#' @keywords tbl_consort
#' @export

wrap_p <- function(txt){
    paste0("(", txt, ")", collapse = "")
}

#' @export
wrap_b <- function(txt){
    paste0("[", txt, "]", collapse = "")
}


#' @export
wrap_f <- function(func, txt){
    paste0(deparse(substitute(func)), "(", txt, ")", collapse = "")
}

#' @export
run_txt <- function(txt){
    eval(parse(text = txt))
}

#' @export
get_rownum <- function(obj, criteria){
    nm <- deparse(substitute(obj))
    txt <- paste0(nm, wrap_b(criteria))
    txt <- paste0("nrow", wrap_p(txt))
    eval(parse(text = txt))
}


#' @export
tbl_consort <- function(obj, txt){
    mx <- matrix(txt, ncol = 2, byrow = TRUE)
    mx <- data.table::as.data.table(mx)
    colnames(mx) <- c("step", "code")
    mx[["excluded"]] <- 0
    mx[["total"]] <- 0
    mx <- rbind(mx, mx[nrow(mx) + 1, ])
    for(i in 1:nrow(mx)){
        if(i == 1){
            codes <- mx["code"][i]
            mx[["total"]][i] <- nrow(obj)
            mx[["excluded"]][i] <- get_rownum(obj, codes)
        } else if (i < nrow(mx)) {
            codes <- paste0("!", wrap_p(codes), " & ", wrap_p(mx[["code"]][i]))
            mx[["excluded"]][i] <- get_rownum(obj, codes)
            mx[["total"]][i] <- mx[["total"]][i - 1] - mx[["excluded"]][i - 1]
        } else {
            mx[["total"]][i] <- mx[["total"]][i - 1] - mx[["excluded"]][i - 1]
        }
    }
    Wu::label(mx[["total"]]) <- "Number of Subjects"
    Wu::label(mx[["step"]]) <- "Exclusion Reason"
    Wu::label(mx[["excluded"]]) <- "Excluded"
    return(mx[, list(total, step, excluded, code)])
}
