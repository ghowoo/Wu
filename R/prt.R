#' A desc_cont Function
#'
#' Print a table
#' @param
#' @keywords print a table to html
#' @export



prt <- function(dt, ...){
  knitr::kable(dt, align = "lrrrrrrrr", ...) %>%
    Wu::styling()
}

#' @export
prt1 <- function(dt, ...){
  dt <- as.data.table(dt)
  dt <- dt[, what_locf := what[nafill(replace(.I, what %in% c(""), NA), type="locf")]]
  group_index <- auto_index(dt$what_locf)
  knitr::kable(dt[, what := NULL][, what_locf := NULL], align = "lrrrrrrrr", ...) %>%
    Wu::styling() %>%
    pack_rows(index = group_index
            , hline_before = FALSE
            , hline_after = FALSE
            , label_row_css = "") 
}
