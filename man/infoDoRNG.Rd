\name{infoDoRNG}
\alias{doRNG}
\alias{infoDoRNG}
\title{Getting Information About doRNG Foreach Backend}
\usage{
  infoDoRNG(data, item)

  doRNG(obj, ex, envir, data)
}
\arguments{
  \item{data}{a list of data used by the backend}

  \item{item}{the data item requested, as a character
  string (e.g. 'name', 'workers', 'version')}

  \item{obj}{a foreach description of the loop arguments}

  \item{ex}{the lopp expression}

  \item{envir}{the loop's evaluation environment}

  \item{data}{configuration data of the doRNG backend}
}
\value{
  \code{infoDoRNG} returns the requested info (usually as a
  character string or a numeric value).
}
\description{
  \code{infoDoRNG} returns information about the doRNG
  backend, e.g., version, number of workers. It is not
  meant to be called by the user.

  \code{doRNG} implements the generic reproducible foreach
  backend. It should not be called directly by the user.
}
\author{
  Renaud Gaujoux
}
\keyword{internal}

