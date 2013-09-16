\name{redirffdfffdfsave}
\alias{redirffdfffdfsave}
\title{Redirecting a moved ffdf}
\usage{
  redirffdfffdfsave(ff, newdir = getwd())
}
\arguments{
  \item{ff}{An ffdf data base}

  \item{newdir}{A new directory where the ffdf was moved
  **manually**}
}
\value{
  A ffdf, with "corrected" file-path
}
\description{
  redirffdfffdfsave (ff, newdir) , allows to move
  "manually" a ffdf folder, saved with **ffdfsave**, and
  avoid file access error, making a redirffdfffdfsave(ffdf,
  newdir), with newdir, the dir (without counting the ffdf
  name dir) where we moved the ffdf.
}

