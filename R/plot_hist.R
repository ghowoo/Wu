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
        ) %>%
        layout(
            bargap = 0
          , xaxis = list(title = unique(h$x_label))
          , yaxis = list(title = "Count")
          , title = list(text = txt
                       , side = "bottom")
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
        ) %>%
        layout(
            bargap = 0
          , yaxis = list(title = unique(h$x_label))
          , xaxis = list(title = "Count")
          , title = list(text = txt
                       , side = "bottom")
        )
}
