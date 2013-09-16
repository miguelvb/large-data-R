\name{subsetffdf}
\alias{subsetffdf}
\title{Subset a ffdf, with splits and conditions.}
\usage{
  subsetffdf(data, conditioncols = names(data), condition,
    splits = 1, verbose = F)
}
\arguments{
  \item{data}{: a ffdf data base.}

  \item{conditioncols}{: the columns that are used in the
  condition to subset}

  \item{condition}{: a condition to subset, like " diagnose
  != "a" "}

  \item{splits}{: number of splits to make while
  subsetting. Will avoid memory problems.}

  \item{verbose}{: if verbose the process.}
}
\description{
  subsetffdf will subset the ffdf using ffwhich over
  conditions ... and will make the data using indexffdf
}

