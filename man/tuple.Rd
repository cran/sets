\name{tuple}
\alias{tuple}
\alias{singleton}
\alias{pair}
\alias{triple}
% \alias{quadruple}
% \alias{quintuple}
% \alias{sextuple}
% \alias{septuple}
% \alias{octuple}
% \alias{nonuple}
% \alias{decuple}

\alias{as.tuple}
\alias{is.tuple}
\alias{tuple_is_ntuple}
\alias{tuple_is_singleton}
\alias{tuple_is_pair}
\alias{tuple_is_triple}
% \alias{tuple_is_quadruple}
% \alias{tuple_is_quintuple}
% \alias{tuple_is_sextuple}
% \alias{tuple_is_septuple}
% \alias{tuple_is_octuple}
% \alias{tuple_is_nonuple}
% \alias{tuple_is_decuple}

\title{Tuples}
\description{Creation and manipulation of tuples.}
\usage{
tuple(\dots)
as.tuple(x)
is.tuple(x)
singleton(\dots)
pair(\dots)
triple(\dots)
tuple_is_singleton(x)
tuple_is_pair(x)
tuple_is_triple(x)
tuple_is_ntuple(x, n)
}
\arguments{
  \item{x}{An \R object.}
  \item{n}{A non-negative integer.}
  \item{\dots}{Possibly named \R objects (for \code{singleton},
    \code{pair}, and
    \code{triple} exactly one, two, and three, respectively.)}
}
\details{
  These functions represent basic infrastructure for handling tuples of
  general (\R) objects.  Class \code{tuple} is used in particular to
  correctly handle cartesian products of sets.  Although tuple objects
  should behave like \dQuote{ordinary} vectors, some operations might
  yield unexpected results since tuple objects are in fact list objects
  internally. The \code{Summary} methods do work if
  defined for the set elements. The \code{\link{mean}} and
  \code{\link[stats]{median}}
  methods try to convert the object to a numeric vector before calling
  the default methods.
}
\seealso{
  \code{\link{set}}.
}
\examples{
## Constructor.
tuple(1,2,3, TRUE)
triple(1,2,3)
pair(Name = "David", Height = 185)
tuple_is_triple(triple(1,2,3))
tuple_is_ntuple(tuple(1,2,3,4), 4)

## Converter.
as.tuple(1:3)

## Operations.
c(tuple("a","b"), 1)
tuple(1,2,3) * tuple(2,3,4)
rep(tuple(1,2,3), 2)
min(tuple(1,2,3))
sum(tuple(1,2,3))
}
\keyword{math}
