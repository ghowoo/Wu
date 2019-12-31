#' A PrtLabels Function
#'
#' This function allows you to print column labels.
#' @param
#' @keywords print column label
#' @export



PrtLabels <- function(data,copy=TRUE,print=FALSE){
  DataName <- deparse(substitute(data))
  VarNames <- names(data)
  Labels <- sapply(data,function(x){attr(x,which="label")})
  Index <- sapply(Labels, is.null)
  Labels[Index] <- VarNames[Index]
  if(print == TRUE){
    cat(paste('attr(',DataName,"$",VarNames,', which = ','\"','label','\"',")",' <- ','\"',Labels,'\"',sep=''),sep="\n")
  }
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(
    paste('attr(',DataName,"$",VarNames,', which = ','\"','label','\"',")",' <- ','\"',Labels,'\"',sep='')
    , con, sep="\t", quote=FALSE,row.names=FALSE, col.names=FALSE)
  close(con)
  Text <- paste('attr(',DataName,"$",VarNames,', which = ','"','label','"',")",' <- ','"',Labels,'"',sep='')
  # return(Text)
}

# TT <- PrtLabels(df)
# for(i in 1:length(TT)){
#   eval(parse(text=TT[i]))
# }
