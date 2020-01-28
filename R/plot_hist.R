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
        xmax = max(x, na.rm = TRUE)
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
          , marker = list(color = Wu::Blues(15)
                        , line = list(color = Wu::Blues(15), width = 0))
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
        add_trace(
            mode = "markers"
          , name = " "
          , type = "scatter"
          , x = ~ jitter(x, factor = 0.05)
          , y = 0
          , marker = list(
                symbol = "line-ns-open"
              , color = Wu::Blues(1)
            )
          , hoverinfo = "text"
          , text = as.character(x)
          ## , hovertemplate =  "%{x:.3f}"
          , showlegend = FALSE
          , opacity = 0.5
        ) %>%
        add_segments(
            x = min(x, na.rm = TRUE), xend = max(x, na.rm = TRUE)
          , y = -0.05, yend = -0.05
          , hoverinfo = "text"
          , text = txt
          , name = " "
          , showlegend = FALSE
          , color = Wu::Blues(1)
          , line = list(color = Wu::Blues(1), opacity = 0.5)
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
          , marker = list(color = Wu::Blues(15)
                        , line = list(color = Wu::Blues(15), width = 0))
        ) %>%
        layout(
            bargap = 0
          , yaxis = list(title = unique(h$x_label)
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
          , xaxis = list(title = "Count"
                       , zeroline = FALSE
                       , showline = FALSE
                       , width = 0
                       , gridwidth = 0
                         )
        ) %>%
        add_trace(
            mode = "markers"
          , name = " "
          , type = "scatter"
          , y = ~ jitter(x, factor = 0.05)
          , x = 0
          , marker = list(
                symbol = "line-ew-open"
              , color = Wu::Blues(1)
            )
          , hoverinfo = "text"
          , text = as.character(x)
          ## , hovertemplate =  "%{x:.3f}"
          , showlegend = FALSE
          , opacity = 0.5
        ) %>%
        add_segments(
            y = min(x, na.rm = TRUE), yend = max(x, na.rm = TRUE)
          , x = -0.05, xend = -0.05
          , hoverinfo = "text"
          , text = txt
          , name = " "
          , showlegend = FALSE
          , color = Wu::Blues(1)
          , line = list(color = Wu::Blues(1), opacity = 0.5)
        )
}



#' @export
plt_ci <- function(data
                  , x
                  , u
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
    plot_ly(data = data, x = x, y = fit, showlegend = FALSE, name = " ") %>% 
        add_lines(showlegend = FALSE
                , line = list(color = Wu::Blues(1), opacity = 0.5)
                , name = " "
                  ) %>%
        add_ribbons(ymin = lower
                  , ymax = upper
                  , opacity = 0.3
                  , fillcolor = Wu::Blues(15)
                  , line = list(opacity = 0, width = 0, color = Wu::Blues(15), opacity = 0.3)
                  , name = " "
                  , showlegend = FALSE
                    ) %>%
        add_trace(
            mode = "markers"
          , name = " "
          , type = "scatter"
          , x = jitter(u, factor = 0.05)
          , y = ytick0
          , marker = list(
                symbol = "line-ns-open"
              , color = Wu::Blues(1)
            )
          , text = as.character(round(u, 3))
          , hoverinfo = "text"
          , showlegend = FALSE
          , opacity = 0.5
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
                  , u
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
    plot_ly(data = data, x = x, y = fit
          , showlegend = FALSE, name = " ", color = group) %>%
        add_lines(showlegend = FALSE
                , name = group, color = group
                  ) %>%
        add_ribbons(ymin = lower
                  , ymax = upper
                  , opacity = 0.5
                  , color = group
                  , line = list(opacity = 0, width = 0)
                  , name = group
                  , showlegend = FALSE
                    ) %>%
        add_trace(
            mode = "markers"
          , name = " "
          , type = "scatter"
          , x = jitter(u, factor = 0.05)
          , text = as.character(round(u, 3))
          , hoverinfo = "text"
          , y = ytick0
          , inherit = FALSE
          , marker = list(
                symbol = "line-ns-open"
              , color = Wu::Blues(1)
            )
          , showlegend = FALSE
          , opacity = 0.5
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
plt_box_nc <- function(data, var_n, var_c, var_n_label = NULL, var_c_label = NULL){
    if (is.null(var_n_label)){
        var_n_label <- Wu::label(data[[deparse(substitute(var_n))]])
    }
    if (is.null(var_c_label)){
        var_c_label <- Wu::label(data[[deparse(substitute(var_c))]])
    }
    var_n <- rlang::enquo(var_n)
    var_c <- rlang::enquo(var_c)
    plot_ly(data = data
          , x = var_c
          , y = var_n
          , color = var_c
            ) %>%
        add_trace(type = "box"
                , marker = list(opacity = 0.5)
                , boxpoints = "all"
                , jitter = 0.8
                , pointpos = 0
                , showlegend = FALSE
                  ) %>%
        layout(xaxis = list(title = var_c_label)
             , yaxis = list(title = var_n_label)
               )
}
