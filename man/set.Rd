\name{set}
\alias{set}
\alias{as.set}
\alias{is.set}
\alias{set_is_empty}
\alias{set_is_subset}
\alias{set_is_proper_subset}
\alias{set_is_equal}
\alias{set_contains_element}
\alias{set_union}
\alias{set_intersection}
\alias{set_symdiff}
\alias{set_complement}
\alias{set_power}
\alias{set_cartesian}
\alias{set_combn}
\title{Sets}
\description{Creation and manipulation of sets.}
\usage{
set(\dots)
as.set(x)
is.set(x)

set_is_empty(x)
set_is_subset(x, y)
set_is_proper_subset(x, y)
set_is_equal(x, y)
set_contains_element(x, e)

set_union(\dots)
set_intersection(\dots)
set_symdiff(\dots)
set_complement(x, y)
set_power(x)
set_cartesian(\dots)
set_combn(x, m)
}
\arguments{
  \item{x}{For \code{as.set()} and \code{is.set()}:
    an \R object. A set object otherwise.}
  \item{y}{A set object.}
  \item{e}{An \R object.}
  \item{m}{Number of elements to choose.}
  \item{\dots}{For \code{set()}: \R objects, and set objects otherwise.}
}
\details{
  These functions represent basic infrastructure for handling sets
  of general (\R) objects. The \code{set_is_\var{foo}()} predicates
  are vectorized. In addition
  to the methods defined, one can use the following operators:
  \code{|} for the union,
  \code{-} for the difference (or complement), \code{&} for the
  intersection, \code{\%D\%} for the symmetric difference,
  \code{*} and \code{^n} for the
  (\eqn{n}-fold) cartesian product, \code{2^} for the power set,
  \code{\%e\%} for the element-of predicate,
  \code{<} and \code{<=} for
  the (proper) subset predicate, \code{>} and \code{>=} for
  the (proper) superset predicate, and \code{==} and \code{!=} for
  (in)equality. The \code{length} method for sets gives the
  cardinality. \code{set_combn} returns the set of all
  subsets of specfied length. The \code{Summary} methods do also work if
  defined for the set elements. The \code{\link{mean}} and
  \code{\link[stats]{median}}
  methods try to convert the object to a numeric vector before calling
  the default methods.

  Because set elements are unordered, it is not sensible to use indexing
  (except using labels). However, it is possible to iterate over
  \emph{all} elements using \code{for} and \code{lapply}/\code{sapply}.

  Note that \code{set_union}, \code{set_intersection}, and
  \code{set_symdiff} accept any number of arguments. The \eqn{n}-ary
  symmetric difference of sets contains
  just elements which are in an odd number of the sets.

  \code{set_contains_element} is vectorized in \code{e}, that is, if \code{e}
  is an atomic vector or list, the is-element operation is performed
  element-wise, and a logical vector returned. Note that, however,
  objects of class \code{"tuple"} are taken as atomic objects to
  correctly handle sets of tuples.
  
}
\seealso{
  \code{\link{set_outer}},
  \code{\link{gset}} for generalized sets,
  and \code{\link{tuple}} for tuples (\dQuote{vectors}).
}
\examples{
## constructor
s <- set(1,2,3)
s

## named elements
snamed <- set(one = 1, 2, three = 3)
snamed

## named elements can directly be accessed
snamed[["one"]]

## a more complex set
set(c, "test", list(1, 2, 3))

## converter
s2 <- as.set(2:5)
s2

## set of sets
set(set(), set(1))

## cartesian product
s * s2
s * s
s ^ 2 # same as above
s ^ 3

## power set
2 ^ s

## tuples
s3 <- set(tuple(1,2,3), tuple(2,3,4))
s3

## Predicates:

## element
1:2 \%e\% s
tuple(1,2,3) \%e\% s3

## subset
s <= s2
s2 >= s # same

## proper subset
s < s

## complement, union, intersection, symmetric difference:
s - 1
s + set("a") # or use: s | set("a")
s & s
s \%D\% s2
set(1,2,3) - set(1,2)
set_intersection(set(1,2,3), set(2,3,4), set(3,4,5))
set_union(set(1,2,3), set(2,3,4), set(3,4,5))
set_symdiff(set(1,2,3), set(2,3,4), set(3,4,5))

## subsets:
set_combn(as.set(1:3),2)

## iterators:
sapply(s, sqrt)
for (i in s) print(i)

## Summary methods
sum(s)
range(s)

## mean / median
mean(s)
median(s)
}
\keyword{math}
