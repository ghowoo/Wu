#' A one-hot function
#'
#' This function allows you to transform variables into dummy variables.
#' @param 
#' @keywords one-hot dummy variable
#' @export


one_hot <- function(data, vars_numeric, vars_factor){
    require(data.table)
    obj <- as.data.table(data)
    column_names <- colnames(obj)
    ## work on numeric variables
    obj_numeric <- obj[, column_names %in% vars_numeric, with = FALSE]
    column_names_numeric <- colnames(obj_numeric)
    ## add dummy variables to numeric variables
    column_names_notA <- paste0(column_names_numeric, "_notA")
    obj_numeric <- obj_numeric[, (column_names_notA) := lapply(.SD, function(x){as.numeric(is.na(x))}), .SDcols = column_names_numeric]
    ## single impute missing values as median
    obj_numeric <- obj_numeric[, (column_names_numeric) := lapply(.SD, function(x){x[is.na(x)] <- median(x, na.rm = TRUE); x}), .SDcols = column_names_numeric]
    ## work on factor variables, add missing level and drop the last level
    add_level_notA <- function(x){
        x <- factor(x)
        levels(x) <- c(levels(x), "notA")
        x[is.na(x)] <- "notA"
        x <- droplevels(x)
        l1 <- levels(x)
        l1 <- l1[-length(l1)]
        x <- factor(x, levels = l1)
        return(x)
    }
    obj_factor <- obj[, column_names %in% c(vars_factor, column_names_notA), with = FALSE]
    column_names_factor <- colnames(obj_factor)
    obj_factor <- obj_factor[, (column_names_factor) := lapply(.SD, add_level_notA), .SDcols = column_names_factor]
    tempDT <- obj_factor
    tempDT[, sequence_index := .I]
    melted <- melt(tempDT, id = "sequence_index", value.factor = T, na.rm=TRUE)
    ## remove the last row of melted value by sequnce index, or assign a NA value on levels
    melted <- melted[, value := paste0(variable, "-", value)][, variable := NULL]
    casted <- dcast(melted, sequence_index ~ value, drop = F, fun.aggregate = length)
    casted <- casted[order(sequence_index)][,sequence_index := NULL]
    obj_return <- cbind(obj_numeric, casted)
    ## remove single value columns
    variables_not_unique <- unlist(lapply(obj_return, function(x){length(unique(x)) > 1}))
    obj_return <- obj_return[, variables_not_unique, with = FALSE]
    return(obj_return)
}
