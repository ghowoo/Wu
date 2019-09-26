#' A Table1 Function
#'
#' This function allows you to create table one.
#' @param 
#' @keywords table one
#' @export
#' @examples
#' Table1()

Table1 <- function(
                   data
                 , Vars
                 , FactorVars
                 , NonNormals = NULL
                 , Strata
                 , contDigits = 1
                 , ...
                   ){
    tableone::CreateTableOne(
                  data = data
                , vars = Vars
                , factorVars = FactorVars
                , includeNA = TRUE
                , test = FALSE
              ) %>%
        print(
            printToggle = FALSE
          , showAllLevels    = TRUE
          , nonnormal = NonNormals
          , contDigits = contDigits
          , varLabels = TRUE
          , missing = TRUE
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
                , test = FALSE
                , strata = Strata
              ) %>%
        print(
            printToggle = FALSE
          , showAllLevels = TRUE
          , nonnormal = NonNormals
          , contDigits = contDigits
          , varLabels = TRUE
          , missing = TRUE
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
