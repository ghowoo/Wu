#' A Blues Function
#'
#' This function allows you to get a color of blue.
#' @param 
#' @keywords color blue
#' @export


Blues  <- function(n) {
    c(
        "#0000FF"
       ,"#1A19FF"
       ,"#2727FF"
       ,"#3133FF"
       ,"#383DFF"
       ,"#3E46FF"
       ,"#434FFF"
       ,"#4758FE"
       ,"#4B60FE"
       ,"#4E68FE"
       ,"#5170FE"
       ,"#5377FE"
       ,"#557FFE"
       ,"#5687FE"
       ,"#578EFD"
       ,"#5896FD"
       ,"#589DFD"
       ,"#58A5FD"
       ,"#57ACFC"
       ,"#56B4FC"
       ,"#54BBFC"
       ,"#52C2FB"
       ,"#4FCAFB"
       ,"#4BD1FA"
       ,"#46D9FA"
       ,"#41E0F9"
       ,"#39E8F8"
       ,"#2FEFF8"
       ,"#21F7F7"
       ,"#00FFF7"
    )[n]
}

#' @export
Greens  <- function(n) {
    c(
        "#F7FCF5"
      , "#E5F5E0"
      , "#C7E9C0"
      , "#A1D99B"
      , "#74C476"
      , "#41AB5D"
      , "#238B45"
      , "#006D2C"
      , "#00441B"
    )[n]
}


#' @export
Oranges  <- function(n) {
    c(
        "#FFF5EB"
      , "#FEE6CE"
      , "#FDD0A2"
      , "#FDAE6B"
      , "#FD8D3C"
      , "#F16913"
      , "#D94801"
      , "#A63603"
      , "#7F2704"
    )[n]
}


Colors <- function(n){
    require(RColorBrewer)
    colorRampPalette(brewer.pal(10, "Spectral"))(n)
}
