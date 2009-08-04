\name{outer}
\alias{set_outer}
\alias{gset_outer}
\alias{cset_outer}
\alias{tuple_outer}
\title{Outer Product of Sets (Tuples)}
\description{Outer \dQuote{product} of (g)sets (tuples).}
\usage{
set_outer(X, Y, FUN = "*", ..., SIMPLIFY = TRUE, quote = FALSE)
gset_outer(X, Y, FUN = "*", ..., SIMPLIFY = TRUE, quote = FALSE)
cset_outer(X, Y, FUN = "*", ..., SIMPLIFY = TRUE, quote = FALSE)
tuple_outer(X, Y, FUN = "*", ..., SIMPLIFY = TRUE, quote = FALSE)
}
\arguments{
  \item{X, Y}{Set (tuple) objects or vectors. If \code{Y} is omitted, \code{X}
    will be used instead. In this case, \code{FUN} can also be specified
    as \code{Y} for convenience.}
  \item{FUN}{A function or function name (character string).}
  \item{SIMPLIFY}{Logical. If \code{TRUE} and all return values of
    \code{FUN} are atomic and of length 1, the result will be an atomic
    matrix; otherwise, a recursive one (a list with \code{dim}
    attribute).}
  \item{quote}{logical indicating whether the character strings used for
    the row and column names of the returned matrix should
    be quoted.}
  \item{\dots}{Additional arguments passed to the \code{FUN}.}
}
\details{
  This function applies \code{FUN} to all pairs of elements specified in
  \code{X} and \code{Y}.  Basically intended as a replacement for
  \code{\link[base]{outer}} for sets (tuples), it will also accept any vector for
  \code{X} and \code{Y}.  The return value will be a matrix of dimension
  \code{length(X)} times \code{length(Y)}, atomic or recursive
  depending on the complexity of \code{FUN}'s return type and the
  \code{SIMPLIFY} argument.
}
\seealso{
  \code{\link{set}}, \code{\link{tuple}}, \code{\link[base]{outer}}.
}
\examples{
set_outer(set(1,2), set(1,2,3), "/")
X <- set_outer(set(1,2), set(1,2,3), pair)
X[[1,1]]
Y <- set_outer(set(1,2), set(1,2,3), set)
Y[[1,1]]
set_outer(2 ^ set(1,2,3), set_is_subset)

tuple_outer(pair(1,2), triple(1,2,3))
tuple_outer(1:5, 1:4, "^")
}
\keyword{math}
