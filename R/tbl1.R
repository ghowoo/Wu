#' A tbl1 Function
#'
#' This function uses package "tableone" to create table one.
#' @param 
#' @keywords tableone
#' @export

combine_list <- function(lst){
  s <- names(lst)
  ind <- split(seq_along(s), s)
  ind <- unlist(lapply(ind, function(l){l[[1]]}))
  invisible(lst[sort(ind)])
}


#' @export

get_tbl1 <- function(...){
  lst <- as.list(match.call())[-1]
  args <- list(
    vars = Vars
  , factorVars = factorVars
  , test = FALSE
  , includeNA = TRUE
  , smd = TRUE
  , addOverall = FALSE
  )
  args <- c(lst, args, formals(CreateTableOne, envir = environment(CreateTableOne)))
  args <- combine_list(args)
  if (trimws(args$strata) %in% c("")){args$addOverall = FALSE} else {args$addOverall = TRUE}
  invisible(do.call(CreateTableOne, args))
}


#' @export

fmt_tbl1 <- function(...){
  lst <- as.list(match.call())[-1]
  args <- list(
    printToggle = FALSE
  , catDigits = 1
  , conDigits = 2
  , pDigits = 3
  , showAllLevels = TRUE
  , smd = TRUE
  , missing = TRUE
  , varLabels = TRUE
  )
  args <- combine_list(c(lst, args))
  rtn <- do.call(tableone:::print.TableOne, args)
  rtn <- as.data.table(cbind(rownames(rtn), rtn))
  colnames(rtn)[1] <- "Variable"
  invisible(rtn)
}

#' @export

tbl1 <- function(...){
  lst <- as.list(match.call())[-1]
  args <- list(
    vars = Vars
  , factorVars = factorVars
  , test = FALSE
  , includeNA = TRUE
  , smd = TRUE
  , addOverall = FALSE
  )
  args <- c(lst, args, formals(CreateTableOne, envir = environment(CreateTableOne)))
  args <- combine_list(args)
  args <- args[names(args) %in% names(formals(CreateTableOne))]
  if (trimws(args$strata) %in% c("")){args$addOverall = FALSE} else {args$addOverall = TRUE}
  tableone_t <- do.call(CreateTableOne, args)
  fx <- tableone:::print.TableOne 
  args2 <- list(
    printToggle = FALSE
  , catDigits = 1
  , contDigits = 2
  , pDigits = 3
  , showAllLevels = TRUE
  , smd = TRUE
  , missing = TRUE
  , varLabels = TRUE
  )
  args <- combine_list(c(lst, args2))
  args <- args[names(args) %in% names(formals(fx))]
  rtn <- do.call(tableone:::print.TableOne, c(list(x = tableone_t), args))
  rtn <- as.data.table(cbind(rownames(rtn), rtn))
  colnames(rtn)[1] <- "Variable"
  rownames(rtn) <- NULL
  invisible(rtn)
}

#' @export

tbl1n <- function(...){
  lst <- as.list(match.call())[-1]
  for (i in seq_along(Vars)){
  var <- Vars[i]
  if (var %in% factorVars) {
    ti <- do.call(tbl1, c(lst, vars = var, factorVars = var))
    }else{
      ti_1 <- do.call(tbl1, c(lst, vars = var, factorVars = NULL))
      ti_2 <- do.call(tbl1, c(lst, vars = var, factorVars = NULL, nonnormal = var))
      ti_3 <- do.call(tbl1, c(lst, vars = var, factorVars = NULL, nonnormal = var, minMax = TRUE))
      ti <- rbind(ti_1, ti_2[-1, ], ti_3[-1, ])
      ti$level <- gsub("^(.+)( )(\\(me)(.+)(\\))$", "\\3\\4\\5", ti$Variable, perl = TRUE)
      ti$level <- gsub("^\\((.+)\\)$", "\\1", ti$level, perl = TRUE)
      ti$Variable <- gsub("^(.+)( )(\\(me)(.+)(\\))$", "\\1", ti$Variable, perl = TRUE)
      ti$Variable[-c(1:2)] <- ""
    }
  if (i %in% c(1)) {
    rtn <- ti
  }else{
    rtn <- rbind(rtn, ti[-1, ])
  }
  }
  rownames(rtn) <- NULL
  invisible(rtn)
  }
