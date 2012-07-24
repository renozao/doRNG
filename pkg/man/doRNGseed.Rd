\name{doRNGseed}
\alias{doRNGseed}
\title{Creating Initial Seed for Random Streams}
\usage{
  doRNGseed(seed = NULL, normal.kind = NULL,
    verbose = FALSE)
}
\arguments{
  \item{seed}{a single or 6-length numeric. If missing then
  the current seed is returned.}

  \item{normal.kind}{Type of Normal random generator. See
  \code{\link{RNG}}.}

  \item{verbose}{logical to toggle verbose messages}
}
\value{
  a 7-length numeric vector.
}
\description{
  This function generate the next random seed used to set
  the RNG of first %dorng% loop iteration. The seeds for
  the subsequent iterations are then obtained with the
  \code{\link{nextRNGStream}} function.
}
\examples{
## generate a seed for \%dorng\% loops
# random
doRNGseed()
doRNGseed()
doRNGseed(NULL)
# fixed
doRNGseed(1)
doRNGseed(1:6)

# `doRNGseed(1)` is identical to
set.seed(1)
doRNGseed()
}
\seealso{
  \code{\link{RNGseq}}
}

