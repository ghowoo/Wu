## Design_Matrix

## function of variable names and classes

Wu.VarClass <- function(dt){
  class <- sapply(dt,class)
  num_unique <- sapply(dt, function(x) length(unique(x)))
  flag_na <- sapply(dt, function(x) sum(is.na(x)) > 1)
  num_rows <- nrow(dt)
  pct_na <- sapply(dt, function(x) sum(is.na(x))/num_rows)
  rtn <- data.frame(
    name= names(class)
    ,class=class
    ,num_unique=num_unique
    ,flag_na = flag_na
    ,pct_na = pct_na
  )
  rownames(rtn) <- NULL
  return(rtn)
}


## generate a formula from outcome and predictors
Wu.formula <- function(outcome,predictors){
  as.formula(paste0(outcome," ~ ",paste0(predictors, sep="",collapse = "+")))
}

## get all predictors names from formula
Wu.GetPredictors <- function(formula,data){
  attr(terms(formula,data=data),"term.labels")
}

Wu.GetLevels <- function(formula,data){
  Predictor <- attr(terms(formula,data=data),"term.labels")
  Class <- sapply(data[,Predictor],class)
  Number_Levels <- sapply(data[,Predictor],nlevels)
  Number_Levels <- pmax(1,Number_Levels)
  Levels <- sapply(data[,Predictor],levels)
  Levels[unlist(lapply(Levels,is.null))] <- NA
  Labels <- sapply(data[,Predictor],function(x) attr(x,"label"))
  Labels[unlist(lapply(Labels,is.null))] <- NA
  Labels <- unlist(Labels)
  
  rtn <- data.frame(
    predictor = rep(Predictor,times=Number_Levels)
    ,class = rep(Class,times=Number_Levels)
    ,number_levels = rep(Number_Levels,Number_Levels)
    ,level = unlist(Levels)
    ,label = rep(Labels,times=Number_Levels))
  rownames(rtn) <- NULL
  rtn <- data.table::as.data.table(rtn)
  rtn <- rtn[
    ,level.order := 1:.N
  ][,coef.name := paste0(predictor,level)
  ][is.na(level),coef.name := predictor
  ][is.na(label),label := predictor
  ][,level.order.within := 1:.N,by=list(predictor)
  ][level.order.within != 1, label:=""]
  # rtn$coef.name <- paste0(rtn$predictor,rtn$level)
  # rtn$coef.name[is.na(rtn$level)] <- rtn$predictor[is.na(rtn$level)]
  # rtn$label[is.na(rtn$label)] <- rtn$predictor[is.na(rtn$label)]
  
  # rtn$level.order <- 1:sum(Number_Levels)
  
  
  return(rtn)
}


