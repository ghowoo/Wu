#' A weighted table() function
#'
#' Tabulate two factors with weight.
#' @param 
#' @keywords wtable
#' @export


wtable <- function(data, rowvar, colvar, weightvar = 1
                   , rowlabel = NULL, collabel = NULL){
    if (is.null(rowlabel)) {
        varlabel <- Wu::label(data[[deparse(substitute(rowvar))]])
    }
    if (is.null(collabel)) {
        bylabel <- Wu::label(data[[deparse(substitute(colvar))]])
    }
    rowvar <- data[[deparse(substitute(rowvar))]]
    colvar <- data[[deparse(substitute(colvar))]]
    weightvar <- data[[deparse(substitute(weightvar))]]
    fun <- function(x, y) {
        sum(as.numeric(rowvar == x) * as.numeric(colvar == y) * weightvar)
    }
    fun <- Vectorize(fun)
    collvl <- levels(colvar)
    rowlvl <- levels(rowvar)
    o <- outer(rowlvl, collvl, function(x, y) fun(x, y))
    o <- cbind(rowlvl, o)
    colnames(o) <- c("Level", collvl)
    return(as.data.table(o))
}


#' @export
wtable2 <- function(data, rowvar, colvar, weightvar = 1
                   , rowlabel = NULL, collabel = NULL){
    if (is.null(rowlabel)) {
        rowlabel <- Wu::label(data[[deparse(substitute(rowvar))]])
    }
    if (is.null(collabel)) {
        collabel <- Wu::label(data[[deparse(substitute(colvar))]])
    }
    rowvar <- data[[deparse(substitute(rowvar))]]
    colvar <- data[[deparse(substitute(colvar))]]
    weightvar <- data[[deparse(substitute(weightvar))]]
    fun <- function(x, y) {
        sum(as.numeric(rowvar == x) * as.numeric(colvar == y) * weightvar)
    }
    fun <- Vectorize(fun)
    collvl <- levels(colvar)
    rowlvl <- levels(rowvar)
    o <- outer(rowlvl, collvl, function(x, y) fun(x, y))
    colnames(o) <- collvl
    o_rowsum <- margin.table(o, 1)
    o_pct <- prop.table(o)
    colnames(o_pct) <- paste0("Cell % ", collvl)
    o_rowpct <- prop.table(o, 1)
    colnames(o_rowpct) <- paste0("Row % ", collvl)
    o_colpct <- prop.table(o, 2)
    colnames(o_colpct) <- paste0("Col % ", collvl)
    t <- cbind(o_rowsum, o, o_pct, o_rowpct, o_colpct)
    colnames(t)[1] <- "Row Sum"
    t <- rbind(margin.table(t, 2), t)
    t <- as.data.table(t, stringsAsFactors = FALSE)
    t <- cbind(rowlabel, c("Col Sum", rowlvl), t)
    colnames(t)[1:2] <- c("Variable", "Level")
    t$order_level <- 0:length(rowlvl)
    return(t)
}

