% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createIndex.R
\name{createIndex}
\alias{createIndex}
\title{Create Index score to assign to each respondents}
\usage{
createIndex(data, class, varList, pValue)
}
\arguments{
\item{data}{a dataframe that is resulted in from \code {prepareData} function}

\item{class}{the colum that records the response}

\item{varList}{a list that includes all the attributes of respondents}

\item{pValue}{a cutoff value for p-value to be considered as significant for modelling}
}
\value{
a dataframe that includes the index value/score for each category of attributes
}
\description{
\code{createIndex} create Index scores to assign to each respondents based on
their profiles attributes
}
