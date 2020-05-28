#' A sv_desc Function
#'
#' This function allows you to save (output) a html file that describes fields in the input dataset.
#' @param
#' @keywords describe dataset field
#' @export

sv_desc <- function(data, file_name="Description_of_Data_Field.html"){
    col_labels <- Wu::GetLabels(data)
    t <- lapply(data, function(x){
        ds <- Wu::desc_var(x)
        data.table(
            N=ds[1]
          , Non_Missing=ds[2]
          , Missing=ds[3]
          , Unique=ds[4]
          , Description=paste0(ds[-(1:4)], sep="", collapse = " | ")
        )
    })
    t2 <- do.call(rbind, t)
    t3 <- cbind(data.table(col_labels), t2)
    t4 <- DT::datatable(t3, filter="top", rownames = FALSE
                      , options=list(
                            pageLength =nrow(t3)
                          , lengthMenu = c(10, 20, 50, 100, 500, 1000, nrow(t3))
                          , autoWidth = TRUE
                            ## , scrollX = TRUE
                            ## , columnDefs = list(list(width = "10%", targets = c(1, 2, 3, 4)))
                            ## , columnDefs = list(list(width = '200px', targets = c(2)))
                            ## , columnDefs = list(list(width = '300px', targets = c(3)))
                            ##   list(width = '100px', targets = c(1))
                            ## , list(width = '200px', targets = c(2))
                            ## , list(width = '20px', targets = c(3))
                            ## , list(width = '200px', targets = c(4, 5, 6))
                            ## , list(width = '500px', targets = c(7))
                            ## )
                        )
                        )
    DT::saveWidget(t4, file_name)
}
