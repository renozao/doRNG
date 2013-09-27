\name{\%dorng\%}
\alias{\%dorng\%}
\title{Reproducible Parallel Foreach Backend}
\usage{
  obj \%dorng\% ex
}
\arguments{
  \item{obj}{a foreach object as returned by a call to
  \code{\link{foreach}}.}

  \item{ex}{the \code{R} expression to evaluate.}
}
\value{
  \code{\%dorng\%} returns the result of the foreach loop.
  See \code{\link{\%dopar\%}}. The whole sequence of RNG
  seeds is stored in the result object as an attribute. Use
  \code{attr(res, 'rng')} to retrieve it.
}
\description{
  \code{\%dorng\%} is a foreach operator that provides an
  alternative operator \code{\%dopar\%}, which enable
  reproducible foreach loops to be performed.
}
\examples{
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

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

# stop cluster
stopCluster(cl)

# More examples can be found in demo `doRNG`
\dontrun{
demo('doRNG')
}
}
\seealso{
  \code{\link{foreach}},
  \code{\link[doParallel]{doParallel}} ,
  \code{\link[doParallel]{registerDoParallel}},
  \code{\link[doMPI]{doMPI}}
}

