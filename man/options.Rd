\name{options}
\alias{set_options}
\title{Options for the set package}
\description{Function for getting and setting options for the set package.}
\usage{
set_options(option, value)
}
\arguments{
  \item{option}{character string indicating the option to be retreived
    or set (see details). If missing, all options are returned as a list.}
  \item{value}{if missing, the specified option is returned, else set.}
}
\details{
  Currently, the following options are available:

  \describe{
    \item{\code{"quote"}:}{logical specifying whether labels for
      character elements are quoted or not (default: \code{FALSE}).}

    \item{\code{"matchfun"}:}{the default matching function for
      \code{\link{cset}} (default: \code{NULL}).}

    \item{\code{"orderfun"}:}{the default ordering function for
      \code{\link{cset}} (default: \code{NULL}).}
  }
}
\seealso{
  \code{\link{cset}}
}
\examples{
set_options()
set_options("quote", TRUE)
print(set("a"))
set_options("quote", FALSE)
print(set("a"))
}
\keyword{math}
