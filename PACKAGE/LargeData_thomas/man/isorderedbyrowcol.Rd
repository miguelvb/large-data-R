\name{isorderedbyrowcol}
\alias{isorderedbyrowcol}
\title{Find if a ffdf column is as 1:N (ordered "row numbers")}
\usage{
  isorderedbyrowcol(data, colname)
}
\arguments{
  \item{data}{An ffdf.}

  \item{colname}{= the name of the column to check.}
}
\description{
  Will check if data is ordered by column colname i.e. if
  colname is exactly as 1,2,3,4,5,.... ,nrows.
}

