% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/various.R
\name{catln}
\alias{catln}
\title{Concatenate and Print with End-Of-Line}
\usage{
catln(..., file = "", sep = " ", fill = FALSE, labels = NULL,
  append = FALSE, EOL = "\\n")
}
\arguments{
\item{...}{R objects (see \link{cat} 'Details' for the types
of objects allowed).}

\item{file}{see \link{read.table}.}

\item{sep}{see \link{read.table}.}

\item{fill}{see \link{read.table}.}

\item{labels}{see \link{read.table}.}

\item{append}{see \link{read.table}.}

\item{EOL}{Object to append (default is end-of-line \code{\\n}).}
}
\value{
None (invisible NULL).
}
\description{
Outputs the objects and EOF, concatenating the representations.
}
\note{
This function is mostly useful for printing values to console in plain R.
RStudio always ends the line for \code{cat}.
}
\examples{
catln("Two numbers:",1,2)
# Ended by tab and dot
catln("Number:",1,EOL="\\t.")
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\seealso{
\link{cat}
}

