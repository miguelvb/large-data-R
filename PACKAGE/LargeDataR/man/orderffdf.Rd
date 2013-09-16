\name{orderffdf}
\alias{orderffdf}
\title{Order a ffdf, in splits to avoid memory problems.}
\usage{
  orderffdf(data, ordercols = names(data), splits = 1,
    verbose = F)
}
\arguments{
  \item{data}{: a ffdf data base.}

  \item{ordercols}{: vector of names of the columns to
  order the data}

  \item{splits}{: number of splits to make while ordering.
  Will avoid memory problems.}

  \item{verbose}{: if verbose the process.}
}
\description{
  orderffdf will order the ffdf using a column it will make
  the process using each time a portion of the total
  database
}

