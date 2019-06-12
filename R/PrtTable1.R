#' A PrtTable1 Function
#'
#' This function allows you to print table one.
#' @param
#' @keywords print table one
#' @export
#' @examples
#' PrtTable1

PrtTable1 <- function(){
  Text <- '
  tableone::CreateTableOne(
  data = df
  , vars = Vars
  , factorVars = FactorVars
  , strata = 
  ) %>% print(
  printToggle = FALSE
  , nonnormal = NonNormal
  , missing = TRUE
  , dropEqual = FALSE
  , showAllLevels = TRUE
  , varLabels = TRUE
  , contDigits = 2
  , catDigits = 2
  , test = FALSE
  , pDigits = 4
  ) %>% data.table::data.table(
  Variables = gsub("  ", " ", rownames(.), fixed = TRUE)
  , .
  , check.names = FALSE
  , stringsAsFactors = FALSE
  ) %>% .[,] %>% knitr::kable(
  align = "lrrrrr"
  ,caption = ""
  ) %>% kableExtra::kable_styling(
  full_width = FALSE
  , bootstrap_options = c("striped", "hover", "condensed", "responsive")
  , position = "left"
  , fixed_thead = T)
  )'
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(
    paste(Text), con, sep="\t", quote=FALSE,row.names=FALSE, col.names=FALSE
  )
  close(con)
}


