## Wu_Functions


WuPercent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, digits = digits, format = format,...), "%")
}

WuFrequency <- function(x){
  temp <- as.data.frame(table(x,useNA = "always"), stringsAsFactors =FALSE)
  rtn <- cbind(temp,temp[,2]/length(x))
  rownames(rtn) <- NULL
  colnames(rtn) <- c("Level","Freq","Pct")
  rtn <- rtn[order(-rtn$Freq),]
  rtn$Pct <- WuPercent(rtn$Pct,4)
  return(rtn)
}

WuSummary <- function(x){
  x <- as.data.frame(x)
  NumRows <- nrow(x)
  NumCols <- ncol(x)
  ColumnNames <- colnames(x)
  for(i in 1:NumCols){
    x.t <- x[,i]
    rtn.class <- class(x.t)
    rtn.NumMissing <- sum(is.na(x.t))
    rtn.PctMissing <- WuPercent(rtn.NumMissing/NumRows,4)
    rtn.NumValues <- sum(!is.na(x.t))
    rtn.PctValues <- WuPercent(rtn.NumValues/NumRows,4)
    rtn.NumUniqueValues <- length(unique(x.t[!is.na(x.t)]))
    rtn.PctUniqueValues <- rtn.NumUniqueValues/rtn.NumValues
    rtn.frequency <- WuFrequency(x.t)
    rtn.level.1 <- ifelse(is.na(rtn.frequency[1,1]),NA,rtn.frequency[1,1])
    rtn.level.1.freq <- ifelse(is.na(rtn.frequency[1,2]),NA,rtn.frequency[1,2])
    rtn.level.1.pct <- ifelse(is.na(rtn.frequency[1,3]),NA,rtn.frequency[1,3])
    rtn.level.2 <- ifelse(is.na(rtn.frequency[2,1]),NA,rtn.frequency[2,1])
    rtn.level.2.freq <- ifelse(is.na(rtn.frequency[2,2]),NA,rtn.frequency[2,2])
    rtn.level.2.pct <- ifelse(is.na(rtn.frequency[2,3]),NA,rtn.frequency[2,3])
    rtn.level.3 <- ifelse(is.na(rtn.frequency[3,1]),NA,rtn.frequency[3,1])
    rtn.level.3.freq <- ifelse(is.na(rtn.frequency[3,2]),NA,rtn.frequency[3,2])
    rtn.level.3.pct <- ifelse(is.na(rtn.frequency[3,3]),NA,rtn.frequency[3,3])
    rtn.level.4 <- ifelse(is.na(rtn.frequency[4,1]),NA,rtn.frequency[4,1])
    rtn.level.4.freq <- ifelse(is.na(rtn.frequency[4,2]),NA,rtn.frequency[4,2])
    rtn.level.4.pct <- ifelse(is.na(rtn.frequency[4,3]),NA,rtn.frequency[4,3])
    rtn.level.5 <- ifelse(is.na(rtn.frequency[5,1]),NA,rtn.frequency[5,1])
    rtn.level.5.freq <- ifelse(is.na(rtn.frequency[5,2]),NA,rtn.frequency[5,2])
    rtn.level.5.pct <- ifelse(is.na(rtn.frequency[5,3]),NA,rtn.frequency[5,3])
    rtn.row <- cbind(
      ColumnNames[i]
      ,rtn.class
      ,NumRows
      ,rtn.NumMissing
      ,rtn.PctMissing
      ,rtn.NumValues
      ,rtn.PctValues
      ,rtn.NumUniqueValues
      ,rtn.PctUniqueValues
      ,rtn.level.1
      ,rtn.level.1.freq
      ,rtn.level.1.pct
      ,rtn.level.2
      ,rtn.level.2.freq
      ,rtn.level.2.pct
      ,rtn.level.3
      ,rtn.level.3.freq
      ,rtn.level.3.pct
      ,rtn.level.4
      ,rtn.level.4.freq
      ,rtn.level.4.pct
      ,rtn.level.5
      ,rtn.level.5.freq
      ,rtn.level.5.pct
    )
    if(i == 1){
      if(exists("rtn") == TRUE){rm("rtn")}
      rtn <- rtn.row
    }
    else{
      rtn <- rbind(rtn,rtn.row)
    }
  }
  rownames(rtn) <- NULL
  return(rtn)
}

Moving_Median <- function(date,value,range=15){
  Date_Start <- min(date)
  Date_End <- max(date)
  Seq <- seq(from=0,to=as.numeric(Date_End - Date_Start),by=1)
  Date_Seq <- Date_Start + Seq
  Date_Seq <- Date_Seq[(range+1):(length(Date_Seq)-range)]
  MovingMedian <- as.data.frame(Date_Seq)
  MovingMedian$median <- NA
  colnames(MovingMedian) <- c("date","median")
  for(i in 1:length(Date_Seq)){
    Date_Median <- Date_Seq[i]
    MovingMedian$median[i] <- median(value[date >= Date_Median - range & date <= Date_Median + range])
  }
  return(MovingMedian)
}

MMonth <- function(Dt){
  temp <- Dt
  temp[!is.na(temp) & as.numeric(format(temp, "%d")) >= 21] <- temp[!is.na(temp) & as.numeric(format(temp, "%d")) >= 21] + 11
  return(as.numeric(format(temp, "%Y%m")))
}

MYear <- function(Dt){
  temp <- Dt
  temp[!is.na(temp) & as.numeric(format(temp, "%d")) >= 21] <- temp[!is.na(temp) & as.numeric(format(temp, "%d")) >= 21] + 11
  lubridate::month(temp) <- lubridate::month(temp) + 3
  return(as.numeric(format(temp, "%Y")))
}



# GetObs <- function(Obs="5497", DBName='OpenMRS3'){
#   con <- DBI::dbConnect(RMySQL::MySQL()
#                    ,user = RootUser
#                    ,password = RootPW
#                    ,dbname = DBName
#                    ,host = LocalHost
#                    ,port = LocalPort)
#   DBI::dbGetQuery(con, "SET NAMES utf8")
#   DB_SQL <- paste0('select* 
#                    from t_obs
#                    where concept_id in ('
#                    ,Obs
#                    ,')and voided = 0')
#   DB <- DBI::dbGetQuery(con
#                    , DB_SQL
#   )
#   DBI::dbDisconnect(con)
#   DB$obs_date <- as.Date(DB$obs_datetime)
#   DB$obs_year <- as.numeric(format(DB$obs_date, "%Y")) 
#   DB$obs_month <- as.numeric(format(DB$obs_date, "%Y%m"))
#   
#   DB$obs_avante_year <- MYear(DB$obs_date)
#   
#   DB$created_date <- as.Date(DB$date_created)
#   DB$created_year <- as.numeric(format(DB$created_date, "%Y")) 
#   DB$created_month <- as.numeric(format(DB$created_date, "%Y%m"))
#   return(data.table::data.table(DB))
# }


# GetTable<- function(Table="t_encounter", DBName='OpenMRS3'){
#   con <- DBI::dbConnect(RMySQL::MySQL()
#                         ,user = RootUser
#                         ,password = RootPW
#                         ,dbname = DBName
#                         ,host = LocalHost
#                         ,port = LocalPort)
#   DBI::dbGetQuery(con, "SET NAMES utf8")
#   DB <- DBI::dbReadTable(con,Table)
#   DBI::dbDisconnect(con)
#   return(data.table::data.table(DB))
# }





`%notin%` <- Negate(`%in%`) 

capwords <- function(s, strict = FALSE) {
  s <- tolower(s)
  cap <- function(s) paste(toupper(substring(s, 1, 1))
                           ,{s <- substring(s, 2); if(strict) tolower(s) else s}
                           ,sep = ""
                           , collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}



# Schemas <- GetTable(Table="r_Schemas",DBName="OpenMRS3")
# 
# Concept_ID_Name <- GetTable(Table="Concept_ID_Name",DBName="Reference")
# GetConceptName <- function(x){
#   Concept_ID_Name$concept_name[match(x,Concept_ID_Name$concept_id)]
# }
# 
# 
# Program_ID_Name <- GetTable(Table="Program_ID_Name",DBName="Reference")
# GetProgramName <- function(x){
#   Program_ID_Name$name[match(x,Program_ID_Name$program_id)]
# }
# 
# GetProgramDescription <- function(x){
#   Program_ID_Name$description[match(x,Program_ID_Name$program_id)]
# }
# 
# State_ID_Name <- GetTable(Table="State_ID_Name",DBName="Reference")
# GetStateName <- function(x){
#   State_ID_Name$state_name[match(x,State_ID_Name$state_id)]
# }
# 
# 
# Districts <- GetTable(Table="Districts",DBName="Reference")
# 
# Location_ID_Name <- GetTable(Table="Location_ID_Name",DBName="Reference")



Func_KWtest <- function(dt,group,x){
  dt <- data.table::as.data.table(dt)
  srm1 <- dt[,.(.N,Median = median(eval(x)),Q1=quantile(eval(x),0.25),Q3=quantile(eval(x),0.75)),by=eval(group)][order(eval(group))]
  srm1 <- as.data.frame(srm1)
  srm1$IQR <- paste0(
    trimws(as.character(round(srm1$Median)))
    ," ["
    ,trimws(as.character(round(srm1$Q1)))
    ,", "
    ,trimws(as.character(round(srm1$Q3)))
    ,"]"
    ,sep=""
    ,callapse=NULL)
  srm1$p_value <- NA
  Frml <- as.formula(paste0(group," ~ ", x))
  kt <- kruskal.test(Frml, data = dt)
  srm1$p_value[1] <- kt$p.value
  srm1$characteristic <- NA
  srm1$characteristic[1] <- as.character(group)
  srm1 <- srm1[,c(8,1,2,6,7)]
  return(srm1)
}


PrtColNames <- function(dd){
  return(cat(paste('\"',(trimws(colnames(dd))),'\"',sep=""),sep="\n,"))
}

