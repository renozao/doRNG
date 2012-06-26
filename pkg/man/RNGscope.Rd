\name{RNGscope}
\alias{RNGscope}
\title{Local RNG scope}
\usage{
  RNGscope(seed)
}
\arguments{
  \item{seed}{numeric (integer) vector used to restore the
  RNG settings. Typically the result of a call to
  \code{RNGscope()}.}
}
\value{
  a numeric (integer) vector containing the current value
  of \code{\link{.Random.seed}} or \code{NULL}.
}
\description{
  Save and restore RNG settings enabling their local
  changes.
}
\examples{
f <- function(){
	orng <- RNGscope()
 on.exit(RNGscope(orng))
	RNGkind('Marsaglia')
	runif(10)
}

sample(NA)
s <- .Random.seed
f()
identical(s, .Random.seed)
\dontshow{ stopifnot(identical(s, .Random.seed)) }
}
\seealso{
  \code{\link{.Random.seed}}
}

