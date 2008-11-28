\name{options}
\alias{sets_options}
\title{Options for the `sets' package}
\description{Function for getting and setting options for the
  \pkg{sets} package.}
\usage{
sets_options(option, value)
}
\arguments{
  \item{option}{character string indicating the option to get
    or set (see details). If missing, all options are returned as a list.}
  \item{value}{Value to be set. If omitted, the current value is returned.}
}
\details{
  Currently, the following options are available:

  \describe{
    \item{\code{"quote"}:}{logical specifying whether labels for
      character elements are quoted or not (default: \code{TRUE}).}

    \item{\code{"hash"}:}{logical specifying whether set elements
      are hashed or not (default: \code{TRUE}).}

    \item{\code{"matchfun"}:}{the default matching function for
      \code{\link{cset}} (default: \code{NULL}).}

    \item{\code{"orderfun"}:}{the default ordering function for
      \code{\link{cset}} (default: \code{NULL}).}

    \item{\code{"universe"}:}{the default universe for
      generalized sets (default: \code{NULL}).}
  }
}
\seealso{
  \code{\link{cset}}
}
\examples{
sets_options()
sets_options("quote", TRUE)
print(set("a"))
sets_options("quote", FALSE)
print(set("a"))
}
\keyword{math}
