\name{mergeby}
\alias{mergeby}
\title{Merge by  two ffdf by chunks}
\usage{
  mergeby(x, y, keycols, splitname, chk)
}
\arguments{
  \item{x,y}{: ffdfs to merge.}

  \item{keycols}{: common columns.}

  \item{splitname}{: name of the column to make the
  splits.}

  \item{chk:}{chunk size (number of rows to take in a
  single pass)}
}
\description{
  merge by is an application of fumerge, for a big data
  ffdf. it will split the comput by chunks of size chk.
}

