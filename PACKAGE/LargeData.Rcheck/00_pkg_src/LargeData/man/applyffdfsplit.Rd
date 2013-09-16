\name{applyffdfsplit}
\alias{applyffdfsplit}
\title{Apply a function over a ffdf, in splits.}
\usage{
  applyffdfsplit(datainput, fu, nsplits = 1, verbose = F)
}
\arguments{
  \item{datainput}{: the ffdf to apply the function over.}

  \item{fu}{: function to apply in the ffdf. It must return
  a data.frame, as if it is working over a R data.frame.}

  \item{splits}{: number of splits to process. Avoids
  memory issues.}

  \item{verbose}{: verbose the process.}
}
\description{
  will apply a function to a ffdf: avoids memory overflow
  by splitting the computation in several parts. the
  function fu should return a R data.frame as applied to a
  R data.frame.
}

