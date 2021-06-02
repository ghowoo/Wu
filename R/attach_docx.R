#' A attach_docx Function
#'
#' This function allows you to attach a docx file of a table to the html output.
#' @param
#' @keywords print column label
#' @export


attach_docx <- function(obj, file = "table1.docx"){
  flextable::flextable(obj) %>% flextable::save_as_docx(path = file)
  xfun::embed_file(file)
}
