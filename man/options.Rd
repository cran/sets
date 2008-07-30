\name{options}
\alias{set_options}
\title{Options for the set package}
\description{Function for getting and setting options for the set package.}
\usage{
set_options(option = NULL, value = NULL)
}
\arguments{
  \item{option}{character string indicating the option to be retreived
    or set (see details). If \code{NULL}, all options are returned as a list.}
  \item{value}{if \code{NULL}, the specified option is returned, else set.}
}
\details{
  The only option currently available is \code{"quote"}, specifying if
  the labels for character elements are quoted or not (default: \code{FALSE}).
}

\examples{
set_options()
set_options("quote", TRUE)
print(set("a"))
set_options("quote", FALSE)
print(set("a"))
}
\keyword{math}
