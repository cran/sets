\name{closure}
\alias{closure}
\alias{closure.set}
\alias{binary_closure}
\alias{reduction}
\alias{reduction.set}
\alias{binary_reduction}
\title{Closure and reduction}
\description{Closure and reduction of (g)sets.}
\usage{
\method{closure}{set}(x, operation = c("union", "intersection"), ...)
binary_closure(x, operation = c("union", "intersection"))
\method{reduction}{set}(x, operation = c("union", "intersection"), ...)
binary_reduction(x, operation = c("union", "intersection"))
}
\arguments{
  \item{x}{For \code{binary_closure} and \code{binary_reduction}: a
    binary matrix.  A set of (g)sets otherwise.}
  \item{operation}{The set operation under which the closure or
    reduction shall be computed.}
  \item{\dots}{Currently not used.}
}
\details{
  The closure of a set \eqn{S} under some operation \eqn{OP}
  contains all elements
  of \eqn{S}, and the results of \eqn{OP} applied to all element pairs
  of  \eqn{S}.

  The reduction of a set \eqn{S} under some operation \eqn{OP}
  is the minimal subset
  of \eqn{S} having the same closure than \eqn{S} under \eqn{OP}.

  Note that the closure and reduction methods for sets
  are currently only implemented for sets of \emph{(g)sets} (families)
  and will give an error for other cases.

  \code{binary_closure} and \code{binary_reduction}
  interface efficient C code for computing
  closures and reductions of binary patterns.
  They are used by the
  high-level methods if \code{x} contains only objects of class \code{sets}.
}
\value{
  An object of same type than \code{x}.
}

\seealso{
  \code{\link{set}}, \code{\link{gset}}.
}
\author{
  The C code for binary closures is provided by Christian Buchta.
}
\examples{
## ordinary set
s <- set(set(1),set(2),set(3))
(cl <- closure(s))
(re <- reduction(cl))
stopifnot(s == re)

(cl <- closure(s, "intersection"))
(re <- reduction(cl, "intersection"))
stopifnot(s == re)

## multi set
s <- set(gset(1,1),gset(2,2),gset(3,3))
(cl <- closure(s))
(re <- reduction(cl))
stopifnot(s == re)

## fuzzy set
s <- set(gset(1,1/3),gset(2,2/3),gset(3,3/3))
(cl <- closure(s))
(re <- reduction(cl))
stopifnot(s == re)

## fuzzy multiset
s <- set(gset(1,list(set(1,0.8))), gset(2, list(gset(1,3))), gset(3,0.3))
(cl <- closure(s))
(re <- reduction(cl))
stopifnot(s == re)
}
\keyword{math}
