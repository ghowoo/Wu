#' A create a DT datatable
#'
#' Generate a DT datatable
#' @param
#' @keywords DT datatable
#' @export


DT <- function(obj, ...){
    DT::datatable(
            data = obj
          , filter = "top"
          , rownames = FALSE
          , extensions = "Buttons"
          , options = list(
                dom = "Bfrtip"
              , buttons = I("colvis")
            )
          , ...
        )
}
