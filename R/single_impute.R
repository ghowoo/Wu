#' A single imputeFunction
#'
#' This function single impute mean for missing values for continuous variable and add na as a category for missing values in categorical variable.
#' @param 
#' @keywords impute
#' @export
#' @examples
#' single_impute()

single_impute <- function(vctr){
    if (inherits(vctr, c("integer", "numeric"))) {
        rtn <- vctr
        rtn[is.na(rtn)] <- median(rtn, na.rm = TRUE)
    } else if (inherits(vctr, c("factor", "character"))) {
        rtn <- Wu::add_level_na(vctr, drop1 = FALSE)
    } else {
        rtn <- vctr
    }
    return(rtn)
}
