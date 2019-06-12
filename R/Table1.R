#' A Table1 Function
#'
#' This function allows you to create table one.
#' @param 
#' @keywords table one
#' @export
#' @examples
#' Table1()

# Table1 <- Table1(...) {
#   tableone::CreateTableOne(...
#   ) %>% 
#     print(
#       printToggle = FALSE,
#       , showAllLevels = TRUE
#       , nonnormal = NonNormal
#       , contDigits = 1
#       , varLabels = TRUE
#     ) %>% 
#     {data.frame(
#       what             = gsub("  ", " ", rownames(.), fixed = TRUE), ., 
#       row.names        = NULL, 
#       check.names      = FALSE, 
#       stringsAsFactors = FALSE)} -> TableForPrint0
#   
#   tableone::CreateTableOne(
#     data = df
#     ,vars = Vars
#     ,factorVars = FactorVars
#     ,includeNA=TRUE
#     ,strata = "obese_group"
#     ,test=TRUE
#   ) %>% 
#     print(
#       printToggle      = FALSE,
#       showAllLevels    = TRUE,
#       cramVars         = "kon"
#       ,nonnormal = NonNormal
#       ,contDigits = 1
#       ,varLabels = TRUE
#     ) %>% 
#     {data.frame(
#       what             = gsub("  ", " ", rownames(.), fixed = TRUE), ., 
#       row.names        = NULL, 
#       check.names      = FALSE, 
#       stringsAsFactors = FALSE)} -> TableForPrint1
#   
#   TableForPrint <- cbind(TableForPrint0[,c("Overall")],TableForPrint1)
#   TableForPrint <- TableForPrint[,c(2,3,1,4,5,6,7,8)]
# }

