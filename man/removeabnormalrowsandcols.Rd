% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsurfer.R
\name{removeabnormalrowsandcols}
\alias{removeabnormalrowsandcols}
\title{Remove Abnormalities}
\usage{
removeabnormalrowsandcols(df, verbose)
}
\arguments{
\item{df}{The data frame imported using fsimport}

\item{verbose}{Whether the print debug information}
}
\value{
The data frame with abnormal rows and columns removed
}
\description{
Removes columns and rows which have been exported from 'Freesurfer' and may cause classification problems
}
\examples{
data <- generaterandomsubjects()
data <- removeabnormalrowsandcols(data,T)
}
\author{
Alexander Luke Spedding, \email{alexspedding271@gmail.com}
}
