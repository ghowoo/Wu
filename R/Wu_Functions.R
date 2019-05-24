## Wu_Functions

# Folder <- "/home/gongw/Projects/WuFunctions/"
# source(file = paste0(Folder,"Wu_Functions_0.R",sep="") )

## Functions for MySQL utility
# source(file = paste0(Folder,"MySQL.R",sep="") )

## Functions for coding


## Functions for Coeficients
# source(file = paste0(Folder,"Utilities.R",sep="") )

## Functions for Design Matrix
# source(file = paste0(Folder,"Design_Matrix.R",sep="") )


## HTML Style
# source(file = paste0(Folder,"HTML_Style.R",sep="") )


## Functions for uitility

`%notin%` <- Negate(`%in%`)

capwords <- function(s, strict = FALSE) {
  s <- tolower(s)
  cap <- function(s) paste(toupper(substring(s, 1, 1))
                           ,{s <- substring(s, 2); if(strict) tolower(s) else s}
                           ,sep = ""
                           , collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


## Function for getting a frequency table with margins
Wu.MarginTable <- function(dt,rowby,colby){
  t <- table(dt[[rowby]],dt[[colby]])
  t2 <- rbind(c(margin.table(t,2)),t)
  t3 <- cbind(c(margin.table(t),margin.table(t,1)),t2)
  t3 <- as.data.frame(t3,stringsAsFactors = FALSE)
  group <- rownames(t3)
  group[1] <- "Overall"
  t4 <- cbind(group,t3)
  colnames(t4)[1:2] <- c("Group","Total")
  rownames(t4) <- NULL
  return(t4)
}

