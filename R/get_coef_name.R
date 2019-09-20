#' A get coefficients' names Function
#'
#' This function allows you to get all coefficients names from a dataframe.
#' @param 
#' @keywords coefficients
#' @export
#' @examples
#' get_coef_name()


get_coef_name <- function(obj){
  Classes <- sapply(obj,class)
  Vars <- names(Classes)
  Number_Levels <- sapply(obj,nlevels)
  Number_Levels <- pmax(1,Number_Levels)
  Levels <- sapply(obj,levels)
  Levels[unlist(lapply(Levels,is.null))] <- NA
  Labels <- sapply(obj,function(x) attr(x,"label"))
  Labels[unlist(lapply(Labels,is.null))] <- NA
  Labels <- unlist(Labels)
  rtn <- data.frame(
    var = rep(Vars,times=Number_Levels)
    ,class = rep(Classes,times=Number_Levels)
    ,num_levels = rep(Number_Levels,times=Number_Levels)
    ,level = unlist(Levels)
    ,label = rep(Labels,times=Number_Levels)
    ,stringsAsFactors = FALSE
  )
  rtn <- data.table::as.data.table(rtn)
  rtn <- rtn[
    ,order.level := 1:.N
    ][,order.withinlevel := 1:.N,by=list(var)
      ][,label := ifelse(is.na(label) | trimws(label) == "", var,label)
        ][,label := ifelse(order.withinlevel > 1, "",label)
          ][,coef.name := paste0(var,level)]
  rownames(rtn) <- NULL
  return(rtn)
}
