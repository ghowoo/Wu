#' A Table1 Function
#'
#' This function allows you to create table one.
#' @param 
#' @keywords table one
#' @export


Table1 <- function(
                   data
                 , Vars
                 , FactorVars
                 , NonNormals = NULL
                 , Strata
                 , contDigits = 1
                 , minMax = FALSE
                 , Test = FALSE
                 , ...
                   ){
    tableone::CreateTableOne(
                  data = data
                , vars = Vars
                , factorVars = FactorVars
                , includeNA = TRUE
                , test = Test
              ) %>%
        print(
            printToggle = FALSE
          , showAllLevels    = TRUE
          , nonnormal = NonNormals
          , contDigits = contDigits
          , varLabels = TRUE
          , missing = TRUE
          , minMax = minMax
        ) %>%
        data.frame(
            what = gsub("  ", " ", rownames(.), fixed = TRUE)
          , .
          , row.names = NULL
          , check.names = FALSE
          , stringsAsFactors = FALSE) -> t0

    tableone::CreateTableOne(
                  data = data
                , vars = Vars
                , factorVars = FactorVars
                , includeNA = TRUE
                , test = Test
                , strata = Strata
              ) %>%
        print(
            printToggle = FALSE
          , showAllLevels = TRUE
          , nonnormal = NonNormals
          , contDigits = contDigits
          , varLabels = TRUE
          , missing = TRUE
          , minMax = minMax
        ) %>%
        data.frame(
            what             = gsub("  ", " ", rownames(.), fixed = TRUE)
          , .
          , row.names        = NULL
          , check.names      = FALSE
          , stringsAsFactors = FALSE) -> t1
    t <- cbind(t0[, 1:3], t1[, 3:ncol(t1)])
    return(t)
}



#' @export

Table1n <- function(obj, Vars, FactorVars, Strata, ...){

    table1_factor <- function(obj, var, Strata, Test = FALSE, ...){
        Table1(
            data = obj
          , Vars = var
          , FactorVars = var
          , Strata = Strata
          , Test = Test
        )
    }

    reformat_numeric <- function(t){
        rtn <- t
        rtn[2, 1] <- gsub("^(.+)( )(\\(me)(.+)(\\))$", "\\1", t[2, 1], perl = TRUE)
        rtn[2, 2] <- gsub("^(.+)( )(\\(me)(.+)(\\))$", "\\3\\4\\5", t[2, 1], perl = TRUE)
        return(rtn)
    }
    reformat_numeric2 <- function(t){
        rtn <- t
        rtn[2, 1] <- ""
        rtn[2, 2] <- gsub("^(.+)( )(\\(me)(.+)(\\))$", "\\3\\4\\5", t[2, 1], perl = TRUE)
        return(rtn)
    }

    
    table1_numeric1 <- function(obj, var, Strata, Test = FALSE, ...){
        reformat_numeric(Table1(
            data = obj
          , Vars = var
          , Strata = Strata
          , Test = Test
        ))
    }

    table1_numeric2 <- function(obj, var, Strata, Test = FALSE, ...){
        reformat_numeric2(Table1(
            data = obj
          , Vars = var
          , NonNormals = var
          , Strata = Strata
          , Test = Test
        ))
    }

    table1_numeric3 <- function(obj, var, Strata, Test = FALSE, ...){
        reformat_numeric2(Table1(
            data = obj
          , Vars = var
          , NonNormals = var
          , minMax = TRUE
          , Strata = Strata
          , Test = Test
        ))
    }

    table1_single <- function(obj, var, Strata, FactorVars, ...){
        if (var %in% FactorVars) {
            return(table1_factor(obj, var, Strata, ...))
        } else {
            return(rbind(
                table1_numeric1(obj, var, Strata, ...)
              , table1_numeric2(obj, var, Strata, ...)[2, ]
              , table1_numeric3(obj, var, Strata, ...)[2, ]
            ))
        }
    }
    
    for (i in 1:length(Vars)){
        if(i == 1){
            rtn <- table1_single(obj, Vars[i], Strata, FactorVars, ...)
        } else {
            rtn <- rbind(rtn
                       , table1_single(obj, Vars[i], Strata, FactorVars, ...)[-1, ]
                         )
        }
    }
    rownames(rtn) <- NULL
    return(rtn)
}
