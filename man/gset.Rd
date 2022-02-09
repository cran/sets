\name{gset}
\alias{gset}
\alias{as.gset}
\alias{is.gset}
\alias{gset_is_empty}
\alias{gset_is_subset}
\alias{gset_is_proper_subset}
\alias{gset_is_equal}
\alias{gset_contains_element}
\alias{gset_union}
\alias{gset_sum}
\alias{gset_product}
\alias{gset_mean}
\alias{gset_difference}
\alias{gset_intersection}
\alias{gset_symdiff}
\alias{gset_complement}
\alias{gset_power}
\alias{gset_cartesian}
\alias{gset_combn}
\alias{gset_core}
\alias{gset_peak}
\alias{gset_height}
\alias{gset_transform_memberships}
\alias{gset_concentrate}
\alias{gset_dilate}
\alias{gset_normalize}
\alias{gset_defuzzify}
\alias{\%D\%}
\alias{\%e\%}
\alias{e}
\alias{is_element}
\alias{gset_bound}
\alias{gset_cardinality}
\alias{gset_charfun}
\alias{gset_memberships}
\alias{gset_support}
\alias{gset_universe}
\alias{gset_has_missings}
\alias{gset_is_crisp}
\alias{gset_is_fuzzy_set}
\alias{gset_is_multiset}
\alias{gset_is_set_or_fuzzy_set}
\alias{gset_is_set_or_multiset}
\alias{gset_is_fuzzy_multiset}
\alias{gset_is_set}
\alias{length.gset}
\alias{lengths.gset}
\alias{mean.gset}
\alias{median.gset}
\alias{cut.gset}

\title{Generalized sets}
\description{Creation and manipulation of generalized sets.}
\usage{
gset(support, memberships, charfun, elements, universe, bound,
     assume_numeric_memberships)
as.gset(x)
is.gset(x)
gset_support(x)
gset_core(x, na.rm = FALSE)
gset_peak(x, na.rm = FALSE)
gset_height(x, na.rm = FALSE)
gset_universe(x)
gset_bound(x)

gset_memberships(x, filter = NULL)
gset_transform_memberships(x, FUN, \dots)
gset_concentrate(x)
gset_dilate(x)
gset_normalize(x, height = 1)
gset_defuzzify(x,
               method = c("meanofmax", "smallestofmax",
                          "largestofmax", "centroid"))

gset_is_empty(x, na.rm = FALSE)
gset_is_subset(x, y, na.rm = FALSE)
gset_is_proper_subset(x, y, na.rm = FALSE)
gset_is_equal(x, y, na.rm = FALSE)
gset_contains_element(x, e)

gset_is_set(x, na.rm = FALSE)
gset_is_multiset(x, na.rm = FALSE)
gset_is_fuzzy_set(x, na.rm = FALSE)
gset_is_set_or_multiset(x, na.rm = FALSE)
gset_is_set_or_fuzzy_set(x, na.rm = FALSE)
gset_is_fuzzy_multiset(x)
gset_is_crisp(x, na.rm = FALSE)
gset_has_missings(x)

gset_cardinality(x, type = c("absolute", "relative"), na.rm = FALSE)
gset_union(\dots)
gset_sum(\dots)
gset_difference(\dots)
gset_product(\dots)
gset_mean(x, y, type = c("arithmetic", "geometric", "harmonic"))
gset_intersection(\dots)
gset_symdiff(\dots)
gset_complement(x, y)
gset_power(x)
gset_cartesian(\dots)
gset_combn(x, m)

e(x, memberships = 1L)
is_element(e)

\method{cut}{gset}(x, level = 1, type = c("alpha", "nu"), strict = FALSE, \dots)
\method{mean}{gset}(x, \dots, na.rm = FALSE)
% \method{median}{gset}(x, na.rm = FALSE)
## S3 method for class 'gset'
## median(x, na.rm = FALSE, ...)     [R >= 3.4.0]
## median(x, na.rm)                  [R < 3.4.0]
\method{length}{gset}(x)
\method{lengths}{gset}(x, use.names = TRUE)
}
\arguments{
  \item{x}{For \code{e()}, \code{as.gset()} and \code{is.gset()}:
    an \R object. A (g)set object otherwise. \code{gset_memberships()}
    also accepts tuple objects.}
  \item{y}{A (g)set object.}
  \item{e}{An object of class \code{element}.}
  \item{filter}{Optional vector of elements to be filtered.}
  \item{m}{Number of elements to choose.}
  \item{support}{A set of elements giving the support of the gset
    (elements with non-zero memberships). Must be a subset of the
    universe, if specified.}
  \item{memberships}{For an (\dQuote{ordinary}) set: 1L (or simply missing).
    For a fuzzy set: a value between 0 and 1.  For a multiset: a
    positive integer.  For a fuzzy multiset: a list of
    multisets with elements from the unit interval (or a list of vectors
    interpreted as such).
    Otherwise, the argument will be transformed using \code{as.gset}.} 
  \item{elements}{A set (or list) of \code{e} objects which are
    object/memberships-pairs.}
  \item{charfun}{A function taking an object and returning the
    membership.}
  \item{bound}{Integer used to compute the absolute complement for
    (fuzzy) multisets. If \code{NULL},
    defaults to the value of \code{sets_options("bound")}.
    If the latter is also \code{NULL}, the maximum multiplicity
    will be used in computations.}
  \item{assume_numeric_memberships}{When applying \code{carfun()} to
        the universe, should numeric memberships (i.e. fuzzy sets or
	multisets) be assumed (default)? If \code{FALSE}, fuzzy multisets
	will be created.}
  \item{FUN}{A function, to be applied to a membership vector.}
  \item{type}{For \code{gset_cardinality()}:
    cardinality type (either \code{"absolute"} or
    \code{"relative"}). For \code{gset_mean()}: mean type
    (\code{"arithmetic"}, \code{"geometric"}, or \code{"harmonic"}).
    For \code{"cut"}: either \code{"alpha"} or \code{"nu"}.}
  \item{strict}{Logical indicating whether the cut level must be 
    exceeded strictly (\dQuote{greater than}) or not (\dQuote{greater
      than or equal}).}
  \item{height}{Double from the unit interval for scaling memberships.}
  \item{universe}{An optional set of elements. If \code{NULL},
    defaults to the value of \code{sets_options("universe")}.
    If the latter is also \code{NULL}, the support
    will be used in computations.}
  \item{method}{\code{"centroid"} computes the arithmetic
    mean of the set elements, using the membership values as
    weights. \code{"smallestofmax"} / \code{"meanofmax"} /
    \code{"largestofmax"} returns the minimum/mean/maximum of all
    set elements with maximal membership degree.}
  \item{level}{The minimum membership level.}
  \item{na.rm}{logical indicating whether \code{NA} values should be
    removed.}
  \item{use.names}{logical; should the names of \code{x} be used in the result?}
  \item{\dots}{For \code{gset_\var{foo}()}: (g)set objects.  For
    the mean and sort methods: additional parameters internally passed to
    \code{\link{mean}} and \code{\link{order}}, respectively. For
    \code{gset_transform_memberships}: further arguments passed to
    \code{FUN}. For \code{cut}: currently not used.}
}
\details{
  These functions represent basic infrastructure for handling
  \emph{generalized} sets of general (\R) objects.

  A generalized set (or gset) is set of pairs \eqn{(e, f)}, where
  \eqn{e} is some set element and \eqn{f} is the characteristic (or
  membership) function. For (\dQuote{ordinary}) sets
  \eqn{f} maps to \eqn{\{0, 1\}},
  for fuzzy sets into the unit interval, for multisets into the natural
  numbers, and for fuzzy multisets \eqn{f} maps to the set of multisets
  over the unit interval.
  
  The \code{gset_is_\var{foo}()} predicates
  are vectorized. In addition
  to the methods defined, one can use the following operators:
  \code{|} for the union, \code{&} for the
  intersection, \code{+} for the sum, \code{-} for
  the difference,  \code{\%D\%} for the symmetric difference,
  \code{*} and \code{^\var{n}} for the
  (\eqn{n}-fold) cartesian product, \code{2^} for the power set,
  \code{\%e\%} for the element-of predicate,
  \code{<} and \code{<=} for
  the (proper) subset predicate, \code{>} and \code{>=} for
  the (proper) superset predicate, and \code{==} and \code{!=} for
  (in)equality.
  The \code{Summary} methods do also work if
  defined for the set elements. 
  The \code{\link{mean}} and \code{\link[stats]{median}}
  methods try to convert the object to a numeric vector before calling
  the default methods. \code{set_combn} returns the gset of all
  subsets of specified length. 

  \code{gset_support}, \code{gset_core}, and \code{gset_peak}
  return the set of elements with memberships greater than zero, equal
  to one, and equal to the maximum membership, respectively.
  \code{gset_memberships} returns the membership
  vector(s) of a given (tuple of) gset(s), optionally
  restricted to the elements specified by \code{filter}.
  \code{gset_height} returns only
  the largest membership degree.
  \code{gset_cardinality} computes either the absolute or the
  relative cardinality, i.e. the memberships sum, or the absolute
  cardinality divided by the number of elements, respectively.
  The \code{length} method for gsets gives the (absolute) cardinality.
  The \code{lengths} method coerces the set to a list
  before applying the \code{length} method on its elements.
  \code{gset_transform_memberships} applies function \code{FOO} to 
  the membership vector of the supplied gset and returns the transformed
  gset. The transformed memberships are guaranteed to be in the unit
  interval.
  \code{gset_concentrate} and \code{gset_dilate} are convenience
  functions, using the square and the square root,
  respectively. \code{gset_normalize} divides the memberships by their
  maximum and scales with \code{height}.
  \code{gset_product} (\code{gset_mean}) of some gsets
  compute the gset with the corresponding memberships multiplied (averaged).
  
  The \code{cut} method provides both \eqn{\alpha}- and \eqn{\nu}-cuts.
  \eqn{\alpha}-cuts \dQuote{filter} all elements with memberships
  greater than (or equal to) \code{level}---the result, thus, is a crisp
  (multi)set. \eqn{\nu}-cuts select those elements with a
  \emph{multiplicity} exceeding \code{level}
  (only sensible for (fuzzy) multisets). 
  
  Because set elements are unordered, it is not allowed to use
  \emph{positional} indexing. However, it is possible to
  do indexing using element labels or
  simply the elements themselves (useful, e.g., for subassignment).
  In addition, it is possible to iterate over
  \emph{all} elements using \code{for} and \code{lapply}/\code{sapply}.

  \code{gset_contains_element} is vectorized in \code{e}, that is, if \code{e}
  is an atomic vector or list, the is-element operation is performed
  element-wise, and a logical vector returned. Note that, however,
  objects of class \code{tuple} are taken as atomic objects to
  correctly handle sets of tuples.
  
}
\seealso{
  \code{\link{set}} for \dQuote{ordinary} sets, 
  \code{\link{gset_outer}}, and
  \code{\link{tuple}} for tuples (\dQuote{vectors}).
}
\references{
  D. Meyer and K. Hornik (2009),
  Generalized and customizable sets in R,
  \emph{Journal of Statistical Software} \bold{31}(2), 1--27.
  \doi{10.18637/jss.v031.i02}.
}
\examples{
## multisets
(A <- gset(letters[1:5], memberships = c(3, 2, 1, 1, 1)))
(B <- gset(c("a", "c", "e", "f"), memberships = c(2, 2, 1, 2)))
rep(B, 2)
gset_memberships(tuple(A, B), c("a","c"))

gset_union(A, B)
gset_intersection(A, B)
gset_complement(A, B)

gset_is_multiset(A)
gset_sum(A, B)
gset_difference(A, B)

## fuzzy sets
(A <- gset(letters[1:5], memberships = c(1, 0.3, 0.8, 0.6, 0.2)))
(B <- gset(c("a", "c", "e", "f"), memberships = c(0.7, 1, 0.4, 0.9)))
cut(B, 0.5)
A * B
A <- gset(3L, memberships = 0.5, universe = 1:5)
!A

## fuzzy multisets
(A <- gset(c("a", "b", "d"),
         memberships = list(c(0.3, 1, 0.5), c(0.9, 0.1),
                            gset(c(0.4, 0.7), c(1, 2)))))
(B <- gset(c("a", "c", "d", "e"),
         memberships = list(c(0.6, 0.7), c(1, 0.3), c(0.4, 0.5), 0.9)))
gset_union(A, B)
gset_intersection(A, B)
gset_complement(A, B)

## other operations
mean(gset(1:3, c(0.1,0.5,0.9)))
median(gset(1:3, c(0.1,0.5,0.9)))

## vectorization
list(gset(1, 0.5), gset(2, 2L), gset()) <= gset(1, 2L)
}
\keyword{math}
