#' A plot proportional odds function
#'
#' This function allows you to plot propotional odds compare two groups on logit of accumulative proportions
#' @param
#' @keywords plot proportional odds

#' @export
get_cdf <- function(data, ordinal, group){
    rtn <- data[
      , .(n=.N), by = setNames(list(get(ordinal), get(group)), c(ordinal, group))
    ][order(get(ordinal), get(group))
    ][, df := n / sum(n), by = list(get(group))
        ][, cdf := cumsum(df), by = list(get(group))
          ][, lcdf := qlogis(1 - cdf)
            ]
    rtn[[paste0(ordinal, "_n")]] <- as.numeric(rtn[[ordinal]])
    return(rtn)
}


#' @export
plt_po <- function(data, ordinal, group){
    cdf <- get_cdf(data=data, ordinal=ordinal, group=group)
    cdf <- cdf[, size := n / max(n)]
    group_label <- ifelse(label(data[[group]]) %in% c(""), group, label(data[[group]]))
    p <- ggplot(data=cdf
     , aes_string(x=ordinal
          ,y="lcdf"
         , group=group
         , color=group
         , alpha="size")) +
    geom_step() +
    theme_bw() +
    theme(panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.position = c(0.99, 0.99)
        , legend.justification = c("right", "top")
          ) +
        guides(alpha=FALSE) +
    labs(y=""
       , x=""
       , color=group_label)
    return(p)
}
