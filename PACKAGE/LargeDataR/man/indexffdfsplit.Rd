\name{indexffdfsplit}
\alias{indexffdfsplit}
\title{Index a ffdf by an index vector idx (re-order or subsetting)}
\usage{
  indexffdfsplit(datainput, idx, nsplits = 1,
    verbose = F)
}
\arguments{
  \item{x}{: ffdf}

  \item{idx}{: index vector (ff vector or ram vector -not
  prefered-)}

  \item{nsplits:}{number of splits to run. Avoids memory
  issues.}

  \item{verbose}{: verbose the process.}
}
\description{
  Will index a ffdf by the index idx. (reorder). avoids
  memory overflow if one single col is able to go into
  memory.... avoids memory overflow by splitting the
  computation in several parts.
}

