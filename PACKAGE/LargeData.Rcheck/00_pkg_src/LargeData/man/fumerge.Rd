\name{fumerge}
\alias{fumerge}
\title{Merge two data bases (ffdf)}
\usage{
  fumerge(x, y, splitcolname, keycols)
}
\arguments{
  \item{splitcolname}{: it is taking splitcolname to make
  splits and not break for same values of that col.}

  \item{keycols}{: the common cols to make the merge.}
}
\description{
  merge two data bases ffdf , using a merge in data.table
  in the form :: res <- y[x,nomatch =0 ], so the result
  will be as y[x, nomatch =0] .
}

