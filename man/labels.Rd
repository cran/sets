\name{labels}
\alias{LABELS}
\alias{LABEL}
\title{Labels from objects}
\description{
  Creates \dQuote{nice} labels from objects.
}
\usage{
LABELS(x, max_width = NULL, dots = "...", unique = FALSE, limit = NULL)
LABEL(x, limit = NULL)
}
\arguments{
  \item{x}{For \code{LABELS}, a vector of \R objects (if the object is
    not a vector, it is converted using \code{as.list}).
    For \code{LABEL}, an \R object.}
  \item{max_width}{Integer vector (recycled as needed) specifying the
    maximum label width for each component of \code{x}.
    If \code{NULL}, there is no limit, otherwise, the label will be
    truncated to \code{max_width}.}
  \item{dots}{A character string appended to a truncated label.
    If \code{NULL}, nothing is appended.}
  \item{unique}{Logical indicating whether
    \code{\link[base]{make.unique}} should be called on the final
    result.}
  \item{limit}{maximum length of vectors or sets to be represented as
    is. Longer elements will be replaced by a label.}
}
\value{
  A character vector of labels generated from the supplied object(s).
  \code{LABELS} first checks whether the object has names and uses these
  if any; otherwise, \code{LABEL} is called for each element to generate
  a \dQuote{short} representation.

  \code{LABEL} is generic to allow user extensions.
  The current methods return the result of \code{\link{format}} if the
  argument is of length 1 (for objects of classes \code{\link{set}} and
  \code{\link{tuple}}: by default of length 5), and create a simple class
  information otherwise.
}
\examples{
LABELS(list(1, "test", X = "1", 1:5))
LABELS(set(X = as.tuple(1:20), "test", list(list(list(1,2)))))
LABELS(set(pair(1,2), set("a", 2), as.tuple(1:10)))
LABELS(set(pair(1,2), set("a", 2), as.tuple(1:10)), limit = 11)
}
\keyword{math}
