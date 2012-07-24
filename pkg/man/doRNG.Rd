\name{doRNG}
\alias{\%dorng\%}
\alias{doRNG}
\alias{infoDoRNG}
\alias{registerDoRNG}
\title{Reproducible Parallel Foreach Backend}
\usage{
  infoDoRNG(data, item)

  obj \%dorng\% ex

  registerDoRNG(seed = NULL, once = TRUE)
}
\arguments{
  \item{data}{a list of data used by the backend}

  \item{item}{the data item requested, as a character
  string (e.g. 'name', 'workers', 'version')}

  \item{obj}{a foreach object as returned by a call to
  \code{\link{foreach}}.}

  \item{ex}{the \code{R} expression to evaluate.}

  \item{seed}{a numerical seed to use (as a single or
  6-length numerical value)}

  \item{once}{a logical to indicate if the RNG sequence
  should be seeded at the beginning of each loop or only at
  the first loop.}
}
\value{
  \code{infoDoRNG} returns the requested info (usually as a
  character string or a numeric value).

  \code{\%dorng\%} returns the result of the foreach loop.
  See \code{\link{\%dopar\%}}. The whole sequence of RNG
  seeds is stored in the result object as an attribute. Use
  \code{attr(res, 'rng')} to retrieve it.
}
\description{
  \code{infoDoRNG} returns information about the doRNG
  backend.

  \code{\%dorng\%} provides an alternative operator
  \code{\%dopar\%}, that ensures reproducible foreach
  loops.

  Note that (re-)registering a foreach backend other than
  doRNG, after a call to \code{registerDoRNG} disables
  doRNG -- which then needs to be registered.
}
\examples{
library(doParallel)
registerDoParallel(cores=2)

# standard \%dopar\% loops are _not_ reproducible
set.seed(1234)
s1 <- foreach(i=1:4) \%dopar\% { runif(1) }
set.seed(1234)
s2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(s1, s2)

# single \%dorng\% loops are reproducible
r1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
r2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(r1, r2)
# the sequence os RNG seed is stored as an attribute
attr(r1, 'rng')

# sequences of \%dorng\% loops are reproducible
set.seed(1234)
s1 <- foreach(i=1:4) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4) \%dorng\% { runif(1) }
# two consecutive (unseed) \%dorng\% loops are not identical
identical(s1, s2)

# But the whole sequence of loops is reproducible
set.seed(1234)
s1.2 <- foreach(i=1:4) \%dorng\% { runif(1) }
s2.2 <- foreach(i=1:4) \%dorng\% { runif(1) }
identical(s1, s1.2) && identical(s2, s2.2)
# it gives the same result as with .options.RNG
identical(r1, s1)

# Works with SNOW-like and MPI clusters
# SNOW-like cluster
cl <- makeCluster(2)
registerDoParallel(cl)

s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)

stopCluster(cl)
registerDoSEQ()

# MPI cluster
if( require(doMPI) ){
cl <- startMPIcluster(2)
registerDoMPI(cl)

s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)

closeCluster(cl)
registerDoSEQ()
}
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

# One can make reproducible loops using the \%dorng\% operator
r1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
# or convert \%dopar\% loops using registerDoRNG
registerDoRNG(1234)
r2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r1, r2)
stopCluster(cl)

# Registering another foreach backend disables doRNG
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1234)
s1 <- foreach(i=1:4) \%dopar\% { runif(1) }
set.seed(1234)
s2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot(!identical(s1, s2)) }

# doRNG is re-nabled by re-registering it
registerDoRNG()
set.seed(1234)
r3 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r2, r3)
# NB: the results are identical independently of the task scheduling
# (r2 used 2 nodes, while r3 used 3 nodes)

# argument `once=FALSE` reseeds doRNG's seed at the beginning of each loop
registerDoRNG(1234, once=FALSE)
r1 <- foreach(i=1:4) \%dopar\% { runif(1) }
r2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r1, r2)

# Once doRNG is registered the seed can also be passed as an option to \%dopar\%
r1.2 <- foreach(i=1:4, .options.RNG=456) \%dopar\% { runif(1) }
r2.2 <- foreach(i=1:4, .options.RNG=456) \%dopar\% { runif(1) }
identical(r1.2, r2.2) && !identical(r1.2, r1)
\dontshow{ stopifnot(identical(r1.2, r2.2) && !identical(r1.2, r1)) }

stopCluster(cl)
}
\author{
  Renaud Gaujoux
}
\seealso{
  \code{\link{foreach}},
  \code{\link[doParallel]{doParallel}} ,
  \code{\link[doParallel]{registerDoParallel}},
  \code{\link[doMPI]{doMPI}}
}

