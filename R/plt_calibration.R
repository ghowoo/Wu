#' A Calibration Plot Function
#'
#' This function allows you to plot observed risk (rolling average) against predicted risk (rolling average from the lowerest predicted probability to the highest).
#' @param 
#' @keywords plot calibration
#' @export

plt_calibration <- function(response, prediction, n){
    t <- data.table(
        response = response
      , prediction = prediction
    )
    t <- t[order(prediction)]
    t$response_mean <- zoo::rollmean(t$response, n * 2 + 1, fill = NA)
    t$prediction_mean <- zoo::rollmean(t$prediction, n * 2 + 1, fill = NA)
    tsm <- t[,.(num = .N
             , mean_pred = mean(response))
           , by = list(prediction, response)
             ][, response_pn := case_when(
                     response == 1 ~ 1
                   , TRUE ~ -1
                 )]
plot_ly(alpha = 0.5, width = 800, height = 800) %>%
    add_lines(x = ~ prediction_mean
            , y = ~ response_mean
            , data = t
            , name = "Observed vs Predicted Risk (Rolling Average)"
            , line = list(color = Blues(10), width = 2)
            , alpha = 0.9
              ) %>%
    add_segments(
        x = 0.1
      , xend = 0.8
      , y = 0.1
      , yend = 0.8
      , name = "Ideal"
      , color = I(Blues(20))
      , line = list(dash = "dash", width = 2)
    ) %>%
    add_segments(
        x = ~ prediction
      , xend = ~ prediction
      , y = 0
      , yend = ~ response_pn * num * 0.02
      , color = I("#333333")
      , data = tsm
      , inherit = FALSE
      , showlegend = FALSE
      , hoverinfo = "skip"
      , alpha = 0.3
      , name = ""
    ) %>%
    layout(
        legend = list(orientation = "v"
                    , yanchar = "top"
                    , x = 0.1)
      , xaxis = list(title = "Predicted Probability"
                   , tickformat = "%")
      , yaxis = list(title = "Observed Probability"
                   , tickformat = "%")
    )
}
