#' A plot function for nonlinear relationship
#'
#' This function allows you to plot a graph on the nonlinear relationship of two variables.
#' @param
#' @keywords plot_nonlinear
#' @export



plot_nonlinear <- function(y, x, data, k = 10, family = "binomial"){
    name_x <- deparse(substitute(x))
    name_y <- deparse(substitute(y))
    x <- data[[name_x]]
    y <- data[[name_y]]
    class_x <- class(x)
    if(class_x %in% c("Date")){x <- as.numeric(x)}
    label_x <- Wu::label(x)
    label_y <- Wu::label(y)
    if (label_x == ""){
        label_x <- name_x
    }
    if (label_y == ""){
        label_y <- name_y
    }
    mod <- mgcv::gam(
                     y ~ s(x, k = k)
                   , method = "REML"
                   , family = family
                 )
    newdata <- data.frame(x =  x[order(x)])
    p <- predict(mod, newdata, type = "link", se.fit = TRUE)
    if(class_x %in% c("Date")){newdata$x <- as.Date(newdata$x, origin = "1970-01-01")}
    ci <- data.frame(
        x = newdata$x
      , y = p$fit
      , lower = p$fit - qnorm(.975) * p$se.fit
      , upper = p$fit + qnorm(.975) * p$se.fit
    )
    ggplot(ci, aes(x, y)) +
        geom_point(position = "jitter", color =  Wu::Blues(5)) +
        geom_line(color = "grey70", alpha = 0.3) +
        geom_ribbon(
            data = ci
          , aes(ymin = lower, ymax = upper)
          , fill = Wu::Blues(15)
          , alpha = 0.3) +
        labs(x = label_x, y = label_y)
}
