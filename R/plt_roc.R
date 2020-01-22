#' A plot ROC Function
#'
#' This function allows you to plot ROC curve. Input is a pROC::roc() object.
#' @param 
#' @keywords plot ROC
#' @export

plt_roc <- function(obj){
    dr <- data.table::data.table(
                          tpr = obj$sensitivities
                        , fpr = 1 - obj$specificities
                      )
    dr <- dr[order(fpr, tpr)]
    plot_ly(data = dr, x = ~ fpr, y = ~ tpr, width = 800, height = 800) %>%
    add_trace(
        type = "scatter"
      , mode = "lines"
      , name = " "
      , line = list(
            shape = "hvh"
          , color = Wu::Blues(5)
          , opacity = 0.5
        )) %>%
    add_trace(
        type = "scatter"
      , mode = "lines"
      , name = " "
      , text = ""
      , hoverinfo = "text"
      , x = c(0, 1)
      , y = c(0, 1)
      , line = list(
            color = Wu::Blues(15))
        , opacity = 0.5
    ) %>%
    layout(xaxis = list(
               zeroline = FALSE
              , showline = FALSE
             , width = 0
             , gridwidth = 0
             , title = "False Positive Rate"
             , tickformat = ".1%"
             , range = c(-0.02, 1.02)
             , tick0 = 0
             , dtick = 0.2)
         , yaxis = list(
               zeroline = FALSE
             , showline = FALSE
             , width = 0
             , gridwidth = 0
             , title = "True Positive Rate"
             , tickformat = ".1%"
             , range = c(-0.02, 1.02)
             , tick0 = 0
             , dtick = 0.2)
         , annotations = list(
               x = 0.4
             , y = 0.5
             , xref = "x"
             , yref = "y"
             , text = paste0("AUC: ", as.character(round(obj$auc, 3)))
             , showarrow = FALSE
           )
           ) %>%
        style(showlegend = FALSE)
}

#' @export
nri <- function(response, predictor1, predictor2){
    require(data.table)
    dt <- data.table(
        response = response
      , predictor1 = predictor1
      , predictor2 = predictor2
    )
    rtn <- dt[
      , flag_up := predictor2 > predictor1
    ][, flag_down := predictor2 < predictor1
      ][, .(
         sum(flag_up & response == 1)/sum(response == 1) -
         sum(flag_down & response == 1)/sum(response == 1) -
         sum(flag_up & response == 0)/sum(response == 0) +
         sum(flag_down & response == 0)/sum(response == 0))
        ]
    return(rtn[[1]][1])
}
