\name{splitapplycpp}
\alias{splitapplycpp}
\title{A split-apply-combine over and ORDERED ffdf data.base.}
\usage{
  splitapplycpp(inputdata, splitvector, chksize, fu)
}
\arguments{
  \item{inputdata}{: the input ffdf. It MUST BE ORDERED by
  the SPLIT VECTOR !!.}

  \item{splitvector}{: the ff vector to use for the
  splits.}

  \item{chksize}{: the number of rows to take in a single
  computing pass (RAM)}

  \item{fu}{: a function that returns a data.frame over a
  data.frame (for a single value of the split vector)}
}
\description{
  split apply over an ORDERED data frame (or ffdf), using
  the cpp functions getsplitindexes and
  getchunksindexesforsplitlist \code{fu(x)} will be
  something like : \code{ x <- data.table(x) ; setkey(x,
  "ident") ; x[ , list( max.diagnose = max(dignose)), by =
  ident ] } for example....  or any function that gives a
  data frame operating in each split of the data, that are
  "respecting" the split-vector (i does not break data for
  same values of split-vector)
}

