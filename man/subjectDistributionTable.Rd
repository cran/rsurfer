% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsurfer.R
\name{subjectDistributionTable}
\alias{subjectDistributionTable}
\title{Create Subject Distribution Table}
\usage{
subjectDistributionTable(data, targetClassName)
}
\arguments{
\item{data}{The subject data to create the table from}

\item{targetClassName}{The name of the field to group the data by}
}
\value{
The subject distribution table in a data frame
}
\description{
Creates a data frame with the distributions of the subjects age and gender grouped by the field 'targetClassName', providing an overview of the subjects. Note: requires an 'Age' and 'Gender' column.
}
\examples{
all <- generaterandomsubjects(1000)
all$Age <- stats::runif(1000,50,80)
all <- addrandomgender(all)
all <- addrandomdiagnosis(all)
subjectDistributionTable(all, "Diagnosis")
}
\author{
Alexander Luke Spedding, \email{alexspedding271@gmail.com}
}
