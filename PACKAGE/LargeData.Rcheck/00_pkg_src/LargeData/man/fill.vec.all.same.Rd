\name{fill.vec.all.same}
\alias{fill.vec.all.same}
\title{Fill a vector with characters to get a fixed width for all the items.}
\usage{
  fill.vec.all.same(x, fill = "*")
}
\arguments{
  \item{x}{: a vector}

  \item{fill}{: a character to "fill" the vector.}
}
\value{
  a character vector of fixed width (equal to the maximum
  input width)
}
\description{
  Will ALWAYS add the fill" character at the END for other
  types.
}

