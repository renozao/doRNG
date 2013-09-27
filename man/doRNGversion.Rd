\name{doRNGversion}
\alias{doRNGversion}
\title{Back Compatibility Option for doRNG}
\usage{
  doRNGversion(x)
}
\arguments{
  \item{x}{version number to switch to, or missing to get
  the currently active version number, or \code{NULL} to
  reset to the default behaviour, i.e. of the latest
  version.}
}
\value{
  a character string If \code{x} is missing this function
  returns the version number from the current behaviour. If
  \code{x} is specified, the function returns the old value
  of the version number (invisible).
}
\description{
  Sets the behaviour of \%dorng\% foreach loops from a
  given version number.
}
\section{Behaviour changes in versions}{
  \describe{ \item{1.4}{ The behaviour of \code{doRNGseed},
  and therefore of \code{\%dorng\%} loops, changed in the
  case where the current RNG was L'Ecuyer-CMRG. Using
  \code{set.seed} before a non-seeded loop used not to be
  identical to seeding via \code{.options.RNG}. Another bug
  was that non-seeded loops would share most of their RNG
  seed! } }
}
\examples{
\dontshow{ registerDoSEQ() }

## Seeding when current RNG is L'Ecuyer-CMRG
RNGkind("L'Ecuyer")

doRNGversion("1.4")
# in version >= 1.4 seeding behaviour changed to fix a bug
set.seed(123)
res <- foreach(i=1:3) \%dorng\% runif(1)
res2 <- foreach(i=1:3) \%dorng\% runif(1)
stopifnot( !identical(attr(res, 'rng')[2:3], attr(res2, 'rng')[1:2]) )
res3 <- foreach(i=1:3, .options.RNG=123) \%dorng\% runif(1)
stopifnot( identical(res, res3) )

# buggy behaviour in version < 1.4
doRNGversion("1.3")
res <- foreach(i=1:3) \%dorng\% runif(1)
res2 <- foreach(i=1:3) \%dorng\% runif(1)
stopifnot( identical(attr(res, 'rng')[2:3], attr(res2, 'rng')[1:2]) )
res3 <- foreach(i=1:3, .options.RNG=123) \%dorng\% runif(1)
stopifnot( !identical(res, res3) )

# restore default RNG
RNGkind("default")
# restore to current doRNG version
doRNGversion(NULL)
}

