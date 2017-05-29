#' Score the respondents based on their profiles and index scores
#'
#' \code {scoring}

scoringIndex <- function(data,indexTable){
    varList <- unqiue(indexTable$varList)

    for (i in varList){
        dt <- merge(data,indexTable[indexTable$varList == i, ], by.x = i, by.y = 'Category')
    }
}
