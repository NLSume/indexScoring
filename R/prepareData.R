#' Create a data that combined all target data and response data
#'
#' \code{prepareData} combined all rows of target data and response data
#' for building index scores
#'
#' @param data a dataset that have a column which indicate if the target responded.
#' @param classVar the column name for variable that indicates the response
#' @param classValue the value that indicates response
#' @return a dataframe that combined all data rows and response rows
#' @export


prepareData <- function(data,classVar,classValue){
    allData <- data.frame(data,Class=c("All"))
    resData <- data.frame(data[data[[classVar]] == classValue, ],Class=c('Res'))
    return(rbind(allData,resData))
}
