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
    format_auc <- function(x, digits = 3){
        format(round(x, digits = digits), nsmall = digits)
    }
    text_auc <- paste0(format_auc(obj$ci[2])
                      , "["
                      , format_auc(obj$ci[1])
                      , ", "
                      , format_auc(obj$ci[3])
                      , "]")
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
             , text = paste0("AUC: ", text_auc)
             , showarrow = FALSE
           )
           ) %>%
        style(showlegend = FALSE)
}

#' @export
nri0 <- function(response, predictor1, predictor2){
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

#' @export

nri <- function(response, predictor1, predictor2, alpha = 0.05, decimals = 4){
    num_events <- sum(response == 1L)
    num_nonevents <- sum(response == 0L)
    pup_events <- sum((predictor2 > predictor1) * (response == 1L))/num_events
    pdown_events <- sum((predictor2 < predictor1) * (response == 1L))/num_events
    pup_nonevents <- sum((predictor2 > predictor1) * (response == 0L))/num_nonevents
    pdown_nonevents <- sum((predictor2 < predictor1) * (response == 0L))/num_nonevents
    var_events <- (pup_events + pdown_events) / num_events
    var_nonevents <- (pup_nonevents + pdown_nonevents) / num_nonevents
    se_nri <- sqrt(var_events + var_nonevents)
    se_nri_events <- sqrt(var_events)
    se_nri_nonevents <- sqrt(var_nonevents)
    nri_events <- (pup_events - pdown_events)
    nri_nonevents <- pdown_nonevents - pup_nonevents
    nri <-  nri_events + nri_nonevents
    nri_ci <- nri + c(-1, 1) * qnorm(1 - alpha / 2) * se_nri
    nri_events_ci <- nri_events + c(-1, 1) * qnorm(1 - alpha / 2) * se_nri_events
    nri_nonevents_ci <- nri_nonevents + c(-1, 1) * qnorm(1 - alpha / 2) * se_nri_nonevents
    pbar_new_events <- mean(predictor2[response == 1L])
    pbar_old_events <- mean(predictor1[response == 1L])
    pbar_new_nonevents <- mean(predictor2[response == 0L])
    pbar_old_nonevents <- mean(predictor1[response == 0L])
    idi <- (pbar_new_events - pbar_new_nonevents) - (pbar_old_events - pbar_old_nonevents)
    diff_p <- predictor2 - predictor1
    se_idi <- sqrt(var(diff_p[response == 1L]) / num_events + var(diff_p[response == 0L]) / num_nonevents)
    idi_ci <- idi + c(-1, 1) * qnorm(1 - alpha / 2) * se_idi
    rtn <- data.frame(
        type = c("NRI", "NRI Events", "NRI Nonevents", "IDI")
      , estimate = c(nri, nri_events, nri_nonevents, idi)
      , lower = c(nri_ci[1], nri_events_ci[1], nri_nonevents_ci[1], idi_ci[1])
      , upper = c(nri_ci[2], nri_events_ci[2], nri_nonevents_ci[2], idi_ci[2])
    )
    fmt3 <- function(x){format(round(x, decimals), nsmall = decimals)}
    rtn$ci <- paste0(fmt3(rtn$estimate)
                       , "["
                       , fmt3(rtn$lower)
                       , ", "
                       , fmt3(rtn$upper)
                       , "]"
                         )
    return(rtn)
}


#' @export
plt_roc2 <- function(roc1, roc2){
    dr1 <- data.table::data.table(
                           tpr = roc1$sensitivities
                         , fpr = 1 - roc2$specificities
                       )
    dr1 <- dr1[order(fpr, tpr)]

    dr2 <- data.table::data.table(
                           tpr = roc2$sensitivities
                         , fpr = 1 - roc2$specificities
                       )
    dr2 <- dr2[order(fpr, tpr)]
    format_auc <- function(x, digits = 3){
        format(round(x, digits = digits), nsmall = digits)
    }
    text_auc1 <- paste0(format_auc(roc1$ci[2])
                      , "["
                      , format_auc(roc1$ci[1])
                      , ", "
                      , format_auc(roc1$ci[3])
                      , "]")
    text_auc2 <- paste0(format_auc(roc2$ci[2])
                      , "["
                      , format_auc(roc2$ci[1])
                      , ", "
                      , format_auc(roc2$ci[3])
                      , "]")
    nri_df <- Wu::nri(
                       response = roc1$original.response
                     , predictor1 = roc1$original.predictor
                     , predictor2 = roc2$original.predictor
                   )
    plot_ly(width = 800, height = 800) %>%
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
    add_trace(
        data = dr1
      , x = ~ fpr
      , y = ~ tpr
      , type = "scatter"
      , mode = "lines"
      , name = "roc1"
      , line = list(
            shape = "hvh"
          , color = Wu::Blues(15)
          , opacity = 0.3
        )) %>%
    add_trace(
        data = dr2
      , x = ~ fpr
      , y = ~ tpr
      , type = "scatter"
      , mode = "lines"
      , name = "roc2"
      , line = list(
            shape = "hvh"
          , color = "#F16913"
          , opacity = 0.5
        )) %>%
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
               x = 0.6
             , y = 0.4
             , xref = "x"
             , yref = "y"
             , text = paste0(
                   "AUC1: ", text_auc1
                 , "\nAUC2: ", text_auc2
                 , "\n NRI: ", nri_df[1, 5]
                 , "\n IDI: ", nri_df[4, 5]
               )
             , showarrow = FALSE
           )
           ) %>%
        style(showlegend = FALSE)
}
