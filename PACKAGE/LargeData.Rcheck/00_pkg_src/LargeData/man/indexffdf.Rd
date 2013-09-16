\name{indexffdf}
\alias{indexffdf}
\title{Index a ffdf by an index vector idx (re-order or subsetting)}
\usage{
  indexffdf(xwe34574, idx, verbose = F)
}
\arguments{
  \item{x}{: ffdf}

  \item{idx}{: index vector (ff vector or ram vector -not
  prefered-)}

  \item{verbose}{: verbose the process.}
}
\description{
  Will index a ffdf by the index idx. (reorder). avoids
  memory overflow if one single col is able to go into
  memory....
}

