#' A variable importance plot function
#'
#' This function to plot variable importance.
#' @param
#' @keywords variable importance, plot
#' @export
get_varimp.glm <- function(mod){
    require(data.table)
    av <- car::Anova(mod)
    dv <- as.data.table(av)
    dv$var_name <- rownames(av)
    colnames(dv) <- c("Deviance", "df", "pvalue", "var_name")
    dv <- dv[var_name %notin% c(NA, NULL, "NULL")
             ][, var_imp := Deviance / sum(Deviance)][order(var_imp)]
    dv$var_name <- factor(dv$var_name, levels = dv$var_name)
    return(dv)
}

#' @export
plt_varimp <- function(data, var_name, var_imp
                     , xlabel = "Relative Variable Importance"
                     , ylabel = ""){
    var_name <- rlang::enquo(var_name)
    var_imp <- rlang::enquo(var_imp)
    plot_ly(data = data
          , x = var_imp
          , y = var_name
            ) %>%
        add_trace(type = "bar"
                , width = 0.5
                , marker = list(
                      color = Wu::Blues(5)
                    , line = list(color = Wu::Blues(15), width = 0)
                  )
                  ) %>%
        layout(bargap = 0
             , xaxis = list(title = xlabel
                          , zeroline = FALSE, showline = FALSE
                          , tickformat = ".1%"
                          , width = 0, gridwidth = 0)
             , yaxis = list(title = ylabel
                          , zeroline = FALSE, showline = FALSE
                          , width = 0
                          , gridwidth = 0)
               )
}
