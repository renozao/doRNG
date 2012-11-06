\name{registerDoRNG}
\alias{registerDoRNG}
\title{Registering doRNG for Persistent Reproducible Parallel Foreach Loops}
\usage{
  registerDoRNG(seed = NULL, once = TRUE)
}
\arguments{
  \item{seed}{a numerical seed to use (as a single or
  6-length numerical value)}

  \item{once}{a logical to indicate if the RNG sequence
  should be seeded at the beginning of each loop or only at
  the first loop.}
}
\description{
  \code{registerDoRNG} registers the doRNG foreach backend.
  Subsequent \%dopar\% loops are then performed using the
  previously registered foreach backend, but are internally
  performed as \code{\link{\%dorng\%}} loops, making them
  fully reproducible.
}
\details{
  Briefly, the RNG is set, before each iteration, with
  seeds for L'Ecuyer's CMRG that overall generate a
  reproducible sequence of statistically independent random
  streams.

  Note that (re-)registering a foreach backend other than
  doRNG, after a call to \code{registerDoRNG} disables
  doRNG -- which then needs to be registered.
}
\examples{
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
\seealso{
  \code{\link{\%dorng\%}}
}

