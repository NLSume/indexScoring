#' Create Index score to assign to each respondents
#'
#' \code{createIndex} create Index scores to assign to each respondents based on
#' their profiles attributes
#' @param data a dataframe that is resulted in from \code {prepareData} function
#' @param class the colum that records the response
#' @param varList a list that includes all the attributes of respondents
#' @param pValue a cutoff value for p-value to be considered as significant for modelling
#' @return a dataframe that includes the index value/score for each category of attributes
#' @export
#' @import tidyr

createIndex <- function(data,class,varList,pValue){
    indFrame <- data.frame(varList = character(),
                           Category = character(),
                           All = numeric(),
                           Res = numeric())

    for (i in varList){
        tbl <- table(data[[class]],data[[i]])
        chiSq <- chisq.test(tbl)

        if (chiSq$p.value <= pValue){
            tbFrame <- as.data.frame(prop.table(tbl,1))
            wideFrame <- tbFrame %>%
                            spread(Var1,Freq)
            wideFrame$varList <- i
            names(wideFrame) <- c('Category','All','Res','varList')
            indFrame <- rbind(indFrame,wideFrame)
        }else{
            cat(i,"doesn't have significant p-value",chiSq$p.value)
        }
    }

    indFrame$Index <- round(indFrame$Res/indFrame$All,2)
    return(indFrame)
}
