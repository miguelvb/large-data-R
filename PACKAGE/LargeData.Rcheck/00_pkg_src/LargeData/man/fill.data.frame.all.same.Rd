\name{fill.data.frame.all.same}
\alias{fill.data.frame.all.same}
\title{Fill a data.frame with characters to get a fixed width for all the items.}
\usage{
  fill.data.frame.all.same(x, fill.char = "*",
    make.factors = F)
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
  Will ALWAYS add the fill" character at the END for other
  types.
}

