\name{fill.data.frame}
\alias{fill.data.frame}
\title{Fill a data.frame with characters to get a fixed character length for all the items.}
\usage{
  fill.data.frame(x, fill.char = "*", make.factors = F)
}
\arguments{
  \item{x}{: a data.frame}

  \item{fill}{: a character to "fill" the vectors.}

  \item{make.factors}{: if making the resulting columns as
  factors.}
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

