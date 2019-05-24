## Utilities


`%notin%` <- Negate(`%in%`)


## make words appropriate capitals
# capwords <- function(s, strict = FALSE) {
#   s <- tolower(s)
#   cap <- function(s) paste(toupper(substring(s, 1, 1))
#                            ,{s <- substring(s, 2); if(strict) tolower(s) else s}
#                            ,sep = ""
#                            , collapse = " " )
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }

# CapWords <- capwords
## print colnames for coding purpose
PrtColNames <- function(dd){
  return(cat(paste('\"',(trimws(colnames(dd))),'\"',sep=""),sep="\n,"))
}

PrtColNames.n <- function(dd){
  nums <- unlist(lapply(dd, is.numeric))
  return(cat(paste('\"',(trimws(colnames(dd)[nums])),'\"',sep=""),sep="\n,"))
}

PrtColNames.c <- function(dd){
  nums <- unlist(lapply(dd, function(x){is.numeric(x) == FALSE}))
  return(cat(paste('\"',(trimws(colnames(dd)[nums])),'\"',sep=""),sep="\n,"))
}

PrtLabel <- function(dd){
  VarNames <- names(dd)
  Labels <- sapply(dd,function(x){attr(x,which="label")})
  Index <- sapply(Labels, is.null)
  Labels[Index] <- VarNames[Index]
  # return(cat(paste(VarNames,' = ','\"',Labels,'\"',sep=''),sep="\n,"))
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(paste(',',VarNames,' = ','\"',Labels,'\"',sep=''), con, sep="\t", quote=FALSE,row.names=FALSE, col.names=FALSE)
  close(con)
}


## print vectors for coding purpose
PrtVector <- function(dd){
  return(cat(paste('\"',(trimws(dd)),'\"',sep=""),sep="\n,"))
}

format.percent <- function (x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format,...), "%")
}

# format.number <- function(x, digits=2) {if (is.na(x)) {""} else {trimws(format(round(x, digits), nsmall=digits))}}

# format.number <- function(x, digits=2) {
#   lapply(x,function(x,digits) if (is.na(x)) {""} else {trimws(format(round(x, digits), nsmall=digits))})
#           }
format.number <- function(x, digits=2) {ifelse(is.na(x),"",format(round(x, digits), nsmall=digits))}

WuFormatP <- function(x,eps=0.0001){
  dgs <- nchar(as.character(eps)) - 1
  rtn <- ifelse(x<eps,paste0("<",format(eps,scientific = FALSE),sep=""), format(round(x,dgs),scientific = FALSE))
  return(ifelse(is.na(x),"",rtn))
}
String.CI <- function(coef=0,se=1,digits=2,df=9999,alpha=0.05,trans=identity){
  ifelse(is.na(coef)
         ,""
         ,paste0(
            format.number(trans(coef),digits=digits)
            ,"("
            ,format.number(trans(coef + se*qt((alpha/2),df)),digits=digits)
            ,", "
            ,format.number(trans(coef + se*qt((1-alpha/2),df)),digits=digits)
            ,")"
           ,sep=""))
}


WuPercent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format,...), "%")
}

WuPercentCI <- function(x,n,digits=1){
  Pct_str <- function(x, digits = 1, format = "f", ...) {
    paste0(formatC(100 * x, digits = digits, format = format,...), "")
  }
  p <- x/n
  se <- sqrt(p*(1-p)/n)
  pl <- p - qnorm(0.975)*se
  pu <- p + qnorm(0.975)*se
  ci_str <- paste0(Pct_str(p,digits = digits)
                   ,"(",Pct_str(pl,digits = digits)
                   ," - ",Pct_str(pu,digits = digits),")",sep="")
  return(trimws(ci_str))
}

Pct_str <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format,...), "")
}

Wu.GetCoefName <- function(obj){
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


WuPercentCI_Exact <- function(x,n,digits=1){
  Pct_str <- function(x, digits = 1, format = "f", ...) {
    paste0(formatC(100 * x, digits = digits, format = format,...), "")
  }
  p <- x/n
  pl <- binom::binom.confint(x,n,methods = "exact")$lower
  pu <- binom::binom.confint(x,n,methods = "exact")$upper
  ci_str <- paste0(Pct_str(p,digits = digits)
                   ,"(",Pct_str(pl,digits = digits)
                   ," - ",Pct_str(pu,digits = digits),")",sep="")
  return(trimws(ci_str))
}



Wu.coef.lm <- function(obj,trans=identity,alpha=0.05,digits=2){
  if  ("rq" %in% class(obj)) {
    VCov <- sqrt(diag(summary.rq(obj, se="iid", covariance=TRUE)$cov))
    df <- summary.rq(obj, se="iid", covariance=TRUE)$rdf
    pvalue <- coef(summary(obj))[,4]
  } else {
    VCov <- sqrt(diag(vcov(obj)))
    df <- as.numeric(obj$df.residual)
    pvalue <- coef(summary(obj))[,4]
  }
  x <- data.frame(
    names(obj$coefficients)
    ,as.numeric(obj$coefficients)
    ,df
    ,VCov
    ,pvalue
    ,stringsAsFactors = FALSE
    ,row.names = NULL)
  colnames(x) <- c("term","coef","df","se","pvalue")
  if("lm" %in% class(obj)){
    x$ci.lower <- x$coef + x$se*qt((alpha/2),df)
    x$ci.upper <- x$coef + x$se*qt((1-alpha/2),df)
  }else{
    x$ci.lower <- x$coef + x$se*qnorm((alpha/2))
    x$ci.upper <- x$coef + x$se*qnorm((1-alpha/2))
  }

  x$coef <- trans(x$coef)
  x$ci.lower <- trans(x$ci.lower)
  x$ci.upper <- trans(x$ci.upper)
  x$ci_str <- paste0(
    format(round(x$coef,digits),nsmall=digits)
    ,"("
    ,format(round(x$ci.lower,digits),nsmall=digits)
    ,", "
    ,format(round(x$ci.upper,digits),nsmall=digits)
    ,")"
    ,sep="")
  return(x)
}

Wu.coef.levels <- function(obj){
  ttt <- sapply(obj$model,class)
  ttt <- ttt[-1]
  x1 <- as.data.frame(cbind(names(ttt), ttt))
  colnames(x1) <- c("predictor","predictor.type")
  rownames(x1) <- NULL
  x1 <- data.table(x1)[,predictor.order:=1:.N]

  ttt <- lapply(obj$model,levels)[-1]
  x2 <- rep(names(ttt),times=lapply(ttt,length))
  x3 <- unlist(ttt,recursive = TRUE, use.names = FALSE)
  x4 <- as.data.frame(cbind(x2,x3))
  colnames(x4) <- c("predictor","level")
  x4 <- as.data.table(x4)[,level.order := 1:.N,by=list(predictor)]
  x5 <- merge(
    x=x1
    ,y=x4
    ,by.x="predictor"
    ,by.y="predictor"
    ,all.x=TRUE
    ,all.y=FALSE
  )

  x6 <- as.data.table(x5)[
    ,ref := !is.na(level) & level.order == 1
    ][,coef.name := paste0(trimws(predictor),ifelse(is.na(level),"",trimws(level)),sep="")]
  return(as.data.frame(x6))
}



Wu.table <- function(outcome,group,data,digits=2,digits.pct=1,label=NA){
  if(is.na(label)){label <- labelled::var_label(data[,group])}
  if(length(label) == 0){label <- group}
  t <- table(data[,group],data[,outcome])
  t <- as.matrix(t)
  n <- rowSums(t)
  N <- sum(n)
  n.str <- paste(format(n,big.mark=","),"/",format(N,big.mark=","),sep="")
  t.rate <- as.matrix(t/n)
  t.rate.str <- matrix(
    paste(format.percent(t.rate,digits=digits.pct)
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
  predictor <- group
  coef.name <- paste(group,level,sep="")
  t2 <- cbind(predictor,label,level,coef.name,N,n,n.str,t,t.rate,t.rate.str,t.odds,t.odds.str)

  rownames(t2) <- NULL
  t2 <- as.data.table(t2)[
    ,rn:=1:.N,by=list(predictor)
  ][rn>1,label:=""
  ][,rn:=NULL]
  t2 <- as.data.frame(t2)
  return(t2)
}

Wu.coef.full <- function(obj,trans=identity,digits=2){
  x <- merge(
    x=Wu.coef.levels(obj)
    ,y=Wu.coef.lm(obj,trans=trans,digits = digits)
    ,by.x="coef.name"
    ,by.y="term"
    ,all.x=TRUE
    ,all.y=FALSE
  )
  x <- x[order(x$predictor.order,x$level.order),]
  rownames(x) <- NULL
  x$ci_str[x$ref == TRUE] <- "ref"
  x$pvalue[x$ref == TRUE] <- NA
  return(x)
}


Wu.or <- function(y,x,data=data,trans=exp,digits=2,digits.pct=1,label=NA){
  if(is.na(label)){label <- labelled::var_label(data[,x])}
  if(length(label) == 0){label <- x}
  Formula <- as.formula(paste0(y, "~",x))
  model <- glm(
    formula = Formula
    , family = binomial(link = "logit")
    , data=data
  )
  ci <- merge(
    x=Wu.coef.full(model,trans = trans,digits=digits)
    ,y=base::subset(Wu.table(outcome=y,group=x,data=data,digits=digits,digits.pct=digits.pct,label=label), select = -c(predictor,level))
    ,by.x = "coef.name"
    ,by.y= "coef.name"
    ,all.x=TRUE
    ,all.y=FALSE
  )
  ci <- ci[order(ci$predictor.order,ci$level.order),]
  rownames(ci) <- NULL
  ci$ci_str[ci$ref==TRUE] <- "ref"
  ci$label <- label
  ci <- as.data.table(ci)[
    ,rn:=1:.N,by=list(predictor)
    ][rn>1,label:=""
      ][,rn:=NULL]
  ci <- as.data.frame(ci)
  # x <- as.data.table(x)[
  #   ,rn:=1:.N,by=list(predictor.order)
  #   ][rn==1,ci_str:="ref"
  #     ][,rn:=NULL]
  # x <- as.data.frame(x)
  return(ci)
}

Wu.rr <- function(y,x,data=data,trans=exp,digits=2){
  Formula <- as.formula(paste0(y, "~",x))
  model <- glm(
    formula = Formula
    , family = binomial(link = "log")
    , data=data
  )
  ci <- merge(
    x=Wu.coef.full(model,trans = trans,digits=digits)
    ,y=base::subset(Wu.table(outcome=y,group=x,data=data,digits=digits), select = -c(predictor,level))
    ,by.x = "coef.name"
    ,by.y= "coef.name"
    ,all.x=TRUE
    ,all.y=FALSE
  )
  return(ci)
}

# return number of missing values
Wu.freq.m <- function(x,data,digits.p=1,label=NA){
  obj <- data[,x]
  var.name <- x
  if (is.na(label)) var.label <- x
  else var.label <- label
  n.missing <- sum(is.na(obj))
  n.total <- length(obj)
  pct.missing <- n.missing/n.total
  str.missing <- paste0(
    WuPercent(pct.missing,digits = digits.p)
    ,"("
    ,format(n.missing,big.mark=",")
    ,"/"
    ,format(n.total,big.mark=",")
    ,")"
    ,sep="")
  output <- as.data.frame(cbind(var.name,var.label,n.total,n.missing,pct.missing,str.missing))
  return(output)
}

Wu.label <- function(x){
  rtn <- labelled::var_label(x)
  if(is.null(rtn)) {
    rtn <- ""
  }
  return(rtn)
}

Wu.GetLabel <- function(DF){
  x <- unlist(lapply(DF,Wu.label))
  x <- cbind(names(x),x)
  rownames(x) <- NULL
  x <- as.data.frame(x)
  colnames(x) <- c("variable","label")
  return(x)
}

Wu.ci.glmer <- function(obj,digits=2){
  est <- summary(obj)$coefficients[,c(1,2,4)]
  est <- as.data.frame(est)
  est$level <- rownames(est)
  colnames(est) <- c("estimate","se","pvalue","level")
  est$ci <- String.CI(coef=est$estimate,se=est$se,digits=digits,df=9999,trans=exp)
  rownames(est) <- NULL
  return(est)
}


Wu.Freq.html <- function(df,Vars,FactorVars){
  tableone::CreateTableOne(
    data = df
    ,vars = Vars
    ,factorVars = FactorVars
    # ,strata=c("community")
    ,includeNA=TRUE
    ,test=FALSE
  ) %>%
    print(
      printToggle      = FALSE,
      showAllLevels    = TRUE,
      cramVars         = "kon"
      ,nonnormal = NonNormal
      ,contDigits = 1
      ,varLabels = TRUE
    ) %>%
    {data.frame(
      what             = gsub("  ", " ", rownames(.), fixed = TRUE), .,
      row.names        = NULL,
      check.names      = FALSE,
      stringsAsFactors = FALSE)} -> TableForPrint



  knitr::kable(
    TableForPrint
    ,caption = "HIV Section"
    # ,col.names = c("Characteristic","# HHs Surveyed per Cell","# of Cells")
    # ,align = c("l","l","r")
  ) %>% kable_styling(full_width = FALSE
                      ,bootstrap_options = c("striped"
                                             , "hover"
                                             , "condensed"
                                             , "responsive")
                      , position = "left")
}
