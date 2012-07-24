\name{RNGseq}
\alias{RNGseq}
\title{Generate Sequence of Random Streams}
\usage{
  RNGseq(n, seed = NULL, unlist = TRUE, ...)
}
\arguments{
  \item{n}{Number of streams to be created}

  \item{seed}{seed used to initialise the set of streams
  using \code{\link{doRNGseed}}.}

  \item{unlist}{a logical that specifies if sequences of
  length 1 should be unlisted and returned as a single
  vector.}

  \item{...}{extra arguments passed to
  \code{\link{doRNGseed}}.}
}
\value{
  a list of integer vectors (or a single integer vector if
  \code{n=1} and \code{unlist=TRUE}).
}
\description{
  Create a given number of seeds for L'Ecuyer's RNG, that
  can be used to seed parallel computation, making them
  fully reproducible.
}
\details{
  This ensures complete reproducibility of the set of run.
  The streams are created using L'Ecuyer's RNG, implemented
  in R core since version 2.14.0 under the name
  \code{"L'Ecuyer-CMRG"} (see \code{\link{RNG}}).

  Generating a sequence without specifying a seed uses a
  single draw of the current RNG. The generation of a
  sequence using seed (a single or 6-length numeric) a
  should not affect the current RNG state.
}
\examples{
RNGseq(3)
RNGseq(3)
RNGseq(3, seed=123)
# or identically
set.seed(123)
identical(RNGseq(3), RNGseq(3, seed=123))
\dontshow{
set.seed(123)
stopifnot( identical(RNGseq(3), RNGseq(3, seed=123)) )
}

RNGseq(3, seed=1:6, verbose=TRUE)
# select Normal kind
RNGseq(3, seed=123, normal.kind="Ahrens")
}

