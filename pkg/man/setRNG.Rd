\name{setRNG}
\alias{setRNG}
\title{Setting the Random Seed}
\usage{
  setRNG(seed, ...)
}
\arguments{
  \item{seed}{integer vector that is a suitable value for
  \code{.Random.seed}, or a single numeric value passed to
  \code{set.seed}.}

  \item{...}{extra arguments passed to
  \code{\link{set.seed}} if seed is a single numeric
  value.}
}
\description{
  Directly sets the value of \code{\link{.Random.seed}} in
  the global environment, and returns its old value --
  invisible.
}

