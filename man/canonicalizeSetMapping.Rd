\name{canonicalize set and mapping}
\alias{canonicalize_set_and_mapping}
\title{Canonicalize set and mapping}
\description{Helper function that canonicalizes set elements, and
  possibly reorders a given mapping accordingly.}
\usage{
canonicalize_set_and_mapping(x, mapping = NULL, margin = NULL)
}
\arguments{
  \item{x}{An object to be transformed into a set.}
  \item{mapping}{A list, array or data frame representing a mapping of the set.}
  \item{margin}{Margins to be reordered (ignored if \code{mapping} is a
    list). If \code{NULL}, all margins with the same length of \code{x}
    will be used.}
}
\details{
  This helper function can be used when a set is to be created from some
  object \code{x}, and another object contains some meta-information
  on the set elements in the same order than the elements
  of \code{x}. The set creation can cause the input elements to be
  permuted. By the use of this function, the meta information can be kept in
  sync with the result of iterating over the associated set.
}
\value{
  A list with three named components:
  \item{set}{The set created from \code{x}.}
  \item{mapping}{\code{mapping}, possibly reorderd to match the order
    of \code{set}.}
  \item{order}{The order used for rearranging the mapping.}
}
\seealso{
  \code{\link{set}}.
}
\examples{
L <- list(c, "a", 3)
M1 <- list("a","b","c")
M2 <- matrix(1:9, ncol = 3)
canonicalize_set_and_mapping(L, M1)
canonicalize_set_and_mapping(L, M2)
canonicalize_set_and_mapping(L, M2, 1)
}
\keyword{math}
