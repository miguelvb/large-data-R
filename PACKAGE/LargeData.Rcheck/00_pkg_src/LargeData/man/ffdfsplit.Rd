\name{ffdfsplit}
\alias{ffdfsplit}
\title{A simple modification of ffdfdply from ffbase package.}
\usage{
  ffdfsplit(x, split, FUN,
    BATCHBYTES = getOption("ffbatchbytes"),
    RECORDBYTES = sum(.rambytes[vmode(x)]), trace = TRUE,
    ...)
}
\description{
  it will make the computation more clear by outputting
  time-info
}

