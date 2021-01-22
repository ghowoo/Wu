#' A function to add variables of month and year to a date variable
#'
#' Add several variables to a dataframe on a date variable with a name on a pattern "date_dob", month, year, yearmonth,weeday
#' @param
#' @keywords add_month
#' @export


add_month <- function(data, name){
    date_name <- paste0("date_", name)
    month_name <- paste0("month_", name)
    year_name <- paste0("year_", name)
    yearmonth_name <- paste0("yearmonth_", name)
    weekday_name <- paste0("weekday_", name)
    data[[month_name]] <- factor(format(data[[date_name]], "%m"))
    data[[year_name]] <- format(data[[date_name]], "%Y")
    data[[yearmonth_name]] <- format(data[[date_name]], "%Y%m")
    data[[weekday_name]] <- weekdays(data[[date_name]])
    data[[weekday_name]] <- factor(
        data[[weekday_name]]
      , levels = c(
            "Sunday"
          , "Monday"
          , "Tuesday"
          , "Wednesday"
          , "Thursday"
          , "Friday"
          , "Saturday"
        )
    )
    return(data)
}
