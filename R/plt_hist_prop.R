#' This function allows you to plot a histogram along with a percentage change
#' @param 
#' @keywords histogram percentage
#' @export


get_range <- function(x, digits = 2, pad = 0.5){
    x_range <- range(x, na.rm = TRUE)
    eps <- seq(-10, 10, 1)
    yh <- ceiling(x_range[2] * 10^eps)
    ind <- which(yh < 10^digits & yh > 10^(digits - 1))
    yh <- yh[ind]
    yl <- floor(x_range[1] * 10^(eps[ind]))
    range <- yh - yl
    yh2 <- yh + range * pad
    yl2 <- yl - range * pad
    int <- ifelse(range >= 40, 2, 1)
    return(c(yl, yh, int, range, yl2, yh2) / (10^eps[ind]))
}


#' @export

plt_hist_prop <- function(data, x = yearmonth_appt, yes_var = flag_abx, yes_value = "Any Abx", no_value = "", xlabel = "", pct_range = NA){
    tb1 <- Wu::tab_freq2(data = data, group = x, outcome = yes_var, yes = yes_value)
    if(is.na(pct_range)){
        pct_range <- get_range(tb1$pct_yes, digits = 1)
    }
    plot_ly(data = tb1
          , x = ~ group
          , y = ~ n_yes
          , type = "bar"
          , name = yes_value
            ## , hoverinfo = "text"
            ## , text = ~ txt_pat
          , opacity = 0.5
          , showlegend = FALSE
            ) %>%
        add_trace(y = ~ n_no
                , name = no_value
                , type = "bar"
                  ## , hoverinfo = "text"
                  ## , text = ~ txt_nonpat
                , opacity = 0.5
                , showlegend = FALSE
                  ) %>%
        layout(yaxis = list(title = "Count")
             , xaxis = list(title = xlabel, showticklabels = TRUE)
             , barmode = "stack") %>%
    add_trace(type = "scatter"
            , mode = "line"
            , y = ~ pct_yes
            ## , hoverinfo = "text"
            ## , text = ~ txt_pct
            ## , name = "PAT %"
              , showlegend = FALSE
            , yaxis = "y2"
              ) %>%
    layout(yaxis2 = list(
               overlaying = "y"
             , side = "right"
             , range = c(0, 1)
             , tick0 = 0
             , dtick = 0.2
             , showticklabels = TRUE
             , tickformat = ".1%"
             , title = ""
           ))
}
