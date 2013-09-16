\name{fill.vec}
\alias{fill.vec}
\title{Fill a vector with characters to get a fixed character length for all the items.}
\usage{
  fill.vec(x, fill = "*")
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
  Will add 0 at the beginning for integer and numeric
  vectors and the "fill" character at the END for other
  types.
}

