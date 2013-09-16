\name{applyffdfchunks}
\alias{applyffdfchunks}
\title{Apply a function over a ffdf, in chunks (number of rows)}
\usage{
  applyffdfchunks(data, fu, chunksize = 1e+06, verbose = F)
}
\arguments{
  \item{data}{: the ffdf to apply the function over.}

  \item{fu}{: function to apply in the ffdf. It must return
  a data.frame, as if it is working over a R data.frame.}

  \item{chunksize}{: number rows to process by split.
  Avoids memory issues.}

  \item{verbose}{: verbose the process.}
}
\description{
  will apply a function to a ffdf avoids memory overflow by
  splitting the computation in several parts. the function
  fu should return a R data.frame as applied to a R
  data.frame.
}

