#' A plot_hist Function
#'
#' Describe a continuous variable with a histogram.
#' @param 
#' @keywords plot a histogram of a continuous variable
#' @export
#' @examples
#' x <- c(rnorm(100), rep(NA, 20))
#' plot_hist(x)

plot_hist <- function(x){
    txt <- Wu::desc_cont(x)
    txt <- paste0(
        c(paste0(txt[1:5], sep = "", collapse = "; ")
      , paste0(txt[6:12], sep = "", collapse = "; "))
      , sep = ""
      , collapse = "\n "
    )
    plot_ly(alpha = 0.6) %>%
        add_histogram(x) %>%
        config(displayModeBar = F) %>%
        layout(
            xaxis = list(title = paste0(label(x)))
          , yaxis = list(title = "Count")
          , title = list(text = txt
                       , side = "bottom")
        )
}

#' @export
get_hist_count <- function(x, xmin = NULL, xmax = NULL, xint = NULL, xlabel = NULL){
    require(data.table)
    if(is.null(xlabel)){
        xlabel <- if_else(Wu::label(x) %in% c(""), deparse(substitute(x)), Wu::label(x))
    }
    if(is.null(xmin)){
        xmin <- min(x, na.rm = TRUE)
    }
    if(is.null(xmax)){
        xmin = max(x, na.rm = TRUE)
    }
    if(is.null(xint)){
        xint <- (xmax - xmin)/20
    }
    h <- hist(
        x
      , breaks = seq(xmin, xmax, xint)
      , plot = FALSE
      , include.lowest = TRUE
      , right = TRUE
    )
    rtn <- data.table::data.table(
        xstart = h$breaks[-length(h$breaks)]
      , xend = h$breaks[-1]
      , mid = h$mids
      , count = h$counts
      , density = h$density * xint
        )
    rtn <- rtn[
        , density_accu := cumsum(density)
    ][, xint := xend - xstart
      ][, x_label := xlabel
        ][, rn := 1:.N]
    rtn <- rtn[
      , str := paste0(
            "Range: "
            , if_else(rn == 1, "[", "(")
          , as.character(xstart)
          , "-"
          , xend
          , "]"
          , "\n"
          , "Count: "
          , as.character(count)
          , "\n"
          , "Prop.: "
          , as.character(round(density, 3))
          , "\n"
          , "Accu.: "
          , as.character(round(density_accu, 3))
        )
    ]
}

#' @export
plt_h <- function(h){
    plot_ly(h) %>%
        add_trace(
            x = ~ mid
          , y = ~ count
          , type = "bar"
          , width = ~ xint
          , hoverinfo = "text"
          , text = ~ str
        ) %>%
        layout(
            bargap = 0
          , xaxis = list(title = unique(h$x_label))
          , yaxis = list(title = "Count")
        )
}

#' @export

plt_hist <- function(x, xmin = NULL, xmax = NULL, xint = NULL, xlabel = NULL){
    h <- get_hist_count(x, xmin, xmax, xint, xlabel)
    txt <- Wu::desc_cont(x)
    txt <- paste0(
        c(paste0(txt[1:5], sep = "", collapse = "; ")
        , paste0(txt[6:12], sep = "", collapse = "; "))
      , sep = ""
      , collapse = "\n "
    )
    plot_ly(h) %>%
        add_trace(
            x = ~ mid
          , y = ~ count
          , type = "bar"
          , width = ~ xint
          , hoverinfo = "text"
          , text = ~ str
          , name = " "
          , showlegend = FALSE
        ) %>%
        layout(
            bargap = 0
          , xaxis = list(title = unique(h$x_label)
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
          , yaxis = list(title = "Count"
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
        ) %>%
        add_segments(
            x = min(x, na.rm = TRUE), xend = max(x, na.rm = TRUE)
          , y = 0, yend = 0
          , hoverinfo = "text"
          , text = txt
          , name = " "
          , showlegend = FALSE
        )
}

#' @export
plt_hist_v <- function(x, xmin = NULL, xmax = NULL, xint = NULL, xlabel = NULL){
    h <- get_hist_count(x, xmin, xmax, xint, xlabel)
    txt <- Wu::desc_cont(x)
    txt <- paste0(
        c(paste0(txt[1:5], sep = "", collapse = "; ")
        , paste0(txt[6:12], sep = "", collapse = "; "))
      , sep = ""
      , collapse = "\n "
    )
    plot_ly(h) %>%
        add_trace(
            y = ~ mid
          , x = ~ count
          , type = "bar"
          , width = ~ xint
          , hoverinfo = "text"
          , text = ~ str
          , orientation = 'h'
          , name = " "
          , showlegend = FALSE
        ) %>%
        layout(
            bargap = 0
          , xaxis = list(title = unique(h$x_label)
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
          , yaxis = list(title = "Count"
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
        ) %>%
        add_segments(
            y = min(x, na.rm = TRUE), yend = max(x, na.rm = TRUE)
          , x = 0, xend = 0
          , hoverinfo = "text"
          , text = txt
          , name = " "
          , showlegend = FALSE
        )
}



#' @export
plt_ci <- function(data
                 , x
                 , xlabel = NULL
                 , ylabel = NULL
                 , fit
                 , lower
                 , upper
                 , xrange = NULL
                 , yrange = NULL
                 , xtick0 = NULL
                 , xdtick = NULL
                 , ytick0 = NULL
                 , ydtick = NULL
                   ){
    x <- rlang::enquo(x)
    fit <- rlang::enquo(fit)
    lower <- rlang::enquo(lower)
    upper <- rlang::enquo(upper)
    plot_ly(data = data, x = x, y = fit, showlegend = FALSE) %>% 
        add_lines(showlegend = FALSE) %>%
        add_markers(showlegend = FALSE, name = " "
                    ) %>%
        add_ribbons(ymin = lower
                  , ymax = upper
                  , opacity = 0.3
                  , line = list(opacity = 0, width = 0)
                  , name = " "
                  , showlegend = FALSE
                    ) %>%
        layout(xaxis = list(
                   zeroline = FALSE
                 , showline = FALSE
                 , width = 0
                 , gridwidth = 0
                 , title = xlabel
                 , range = xrange
                 , tick0 = xtick0
                 , dtick = xdtick
               )
               , yaxis = list(
                   zeroline = FALSE
                 , showline = FALSE
                 , width = 0
                 , gridwidth = 0
                 , title = ylabel
                 , range = yrange
                 , tick0 = ytick0
                 , dtick = ydtick
               )
               ) %>%
        style(showlegend = FALSE) %>%
        layout(height = 600, width = 1200)
}


#' @export
plt_ci_g <- function(data
                 , x
                 , xlabel = NULL
                 , ylabel = NULL
                 , fit
                 , lower
                 , upper
                 , group
                 , xrange = NULL
                 , yrange = NULL
                 , xtick0 = NULL
                 , xdtick = NULL
                 , ytick0 = NULL
                 , ydtick = NULL
                   ){
    x <- rlang::enquo(x)
    fit <- rlang::enquo(fit)
    lower <- rlang::enquo(lower)
    upper <- rlang::enquo(upper)
    group <- rlang::enquo(group)
    plot_ly(data = data, x = x, y = fit, color = group, showlegend = FALSE) %>% 
        add_lines(showlegend = FALSE, color = group) %>%
        add_markers(showlegend = FALSE, color = group
                    ) %>%
        add_ribbons(ymin = lower
                  , ymax = upper
                  , color = group
                  , opacity = 0.8
                  , line = list(opacity = 0, width = 0)
                  , showlegend = FALSE
                    )  %>%
        layout(xaxis = list(
                   zeroline = FALSE
                 , showline = FALSE
                 , width = 0
                 , gridwidth = 0
                 , title = xlabel
                 , range = xrange
                 , tick0 = xtick0
                 , dtick = xdtick
               )
               , yaxis = list(
                   zeroline = FALSE
                 , showline = FALSE
                 , width = 0
                 , gridwidth = 0
                 , title = ylabel
                 , range = yrange
                 , tick0 = ytick0
                 , dtick = ydtick
               )
               ) %>%
        layout(height = 600, width = 1200)
}
