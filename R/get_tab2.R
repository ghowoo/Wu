#' This function allows you to get a tabulation of two categorical variables.
#' @param 
#' @keywords tabulate
#' @export



tab_freq <- function(outcome,group,data,digits=2,digits.pct=1,label=NA){
  if(is.na(label)){label <- labelled::var_label(data[,group])}
  if(length(label) == 0){label <- group}
  t <- table(data[[group]],data[[outcome]])
  t <- as.data.frame.matrix(t)
  n <- rowSums(t)
  N <- sum(n)
  n.str <- paste(format(n,big.mark=","),"/",format(N,big.mark=","),sep="")
  t.rate <- as.matrix(t/n)
  t.rate.str <- matrix(
    paste(Wu::percent(t.rate,digits=digits.pct)
          , "("
          , format(t,big.mark=",")
          , "/"
          , format(n,big.mark=",")
          , ")"
          ,sep=""
          ,collapse = NULL)
    ,nrow=nrow(t))
  t.odds <- as.matrix(t/(n-t))
  t.odds.str <- matrix(
    paste(format(t.odds,digits=digits,scientific = FALSE)
          , "("
          , format(t,big.mark=",")
          , "/"
          , format(n-t,big.mark=",")
          , ")"
          ,sep=""
          ,collapse = NULL)
    ,nrow=nrow(t))
  
  colnames(t.rate) <- paste("rate.",colnames(t),sep="")
  colnames(t.rate.str) <- paste("rate.str.",colnames(t),sep="")
  colnames(t.odds) <- paste("odds.",colnames(t),sep="")
  colnames(t.odds.str) <- paste("odds.str.",colnames(t),sep="")
  colnames(t) <- paste("n.",colnames(t),sep="")
  level <- rownames(t)
  rownames(t) <- NULL
  predictor <- group
  coef.name <- paste(group,level,sep="")
  t2 <- cbind.data.frame(
      predictor
     ,label
     ,level
     ,coef.name
     ,N
     ,n
     ,t
     ,n.str
     ,t.rate
     ,t.rate.str
     ,t.odds
     ,t.odds.str
    , stringsAsFactors = FALSE      
  )
  
  rownames(t2) <- NULL
  t2 <- as.data.table(t2)[
    ,rn:=1:.N,by=list(predictor)
  ][rn>1,label:=""
  ][,rn:=NULL]
  ## t2 <- as.data.frame(t2)
  ## return(t2)
  return(t2)
}

#' @export
tab_freq2 <- function(outcome,group,data,digits=2,digits.pct=1,label=NA, yes = "Any Abx"){
    t <- Wu::tab_freq(outcome,group,data,digits,digits.pct,label=NA)
    vars <- c(
        "level"
      , "N"
      , "n"
      , paste0("n.", yes)
      , paste0("rate.", yes)
    )
    t <- t[, ..vars]
    colnames(t) <- c(
        "group"
      , "n_total"
      , "n_group"
      , "n_yes"
      , "pct_yes"
    )
    t$n_no <- t$n_group - t$n_yes
    t$pct_no <- t$n_no / t$n_group
    return(t)
}

