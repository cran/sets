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
\alias{gset_difference}
\alias{gset_intersection}
\alias{gset_symdiff}
\alias{gset_complement}
\alias{gset_power}
\alias{gset_cartesian}
\alias{gset_combn}
\alias{\%D\%}
\alias{\%e\%}
\alias{e}
\alias{gset_cardinality}
\alias{gset_charfun}
\alias{gset_memberships}
\alias{gset_support}
\alias{gset_similarity}
\alias{gset_is_crisp}
\alias{gset_is_fuzzy_set}
\alias{gset_is_multiset}
\alias{gset_is_fuzzy_multiset}
\alias{gset_is_set}
\alias{length.gset}
\alias{mean.gset}
\alias{median.gset}
\alias{sort.gset}
\alias{cut.gset}

\title{Generalized sets}
\description{Creation and manipulation of generalized sets.}
\usage{
gset(support, memberships, charfun, elements)
as.gset(x)
is.gset(x)

gset_is_empty(x)
gset_is_subset(x, y)
gset_is_proper_subset(x, y)
gset_is_equal(x, y)
gset_contains_element(x, e)

gset_is_multiset(x)
gset_is_set(x)
gset_is_fuzzy_set(x)
gset_is_fuzzy_multiset(x)
gset_is_crisp(x)

gset_cardinality(x)
gset_union(\dots)
gset_sum(\dots)
gset_difference(x, y)
gset_intersection(\dots)
gset_symdiff(\dots)
gset_complement(x, y)
gset_power(x)
gset_cartesian(\dots)
gset_combn(x, m)

gset_similarity(x, y, method = "Jaccard")

e(x, memberships = 1L)

\method{sort}{gset}(x, decreasing = FALSE, \dots)

\method{cut}{gset}(x, level = 1, \dots)
\method{mean}{gset}(x, \dots)
\method{median}{gset}(x, na.rm = FALSE)
\method{length}{gset}(x)
}
\arguments{
  \item{x}{For \code{e()}, \code{as.gset()} and \code{is.gset()}:
    an \R object. A (g)set object otherwise.}
  \item{y}{A (g)set object.}
  \item{e}{An \R object.}
  \item{m}{Number of elements to choose.}
  \item{support}{A set of elements giving the support of the gset.}
  \item{memberships}{For an ordinary set: 1L (or simply missing).
    For a fuzzy set: a value between 0 and 1.  For a multiset: a
    positive integer.  For a fuzzy multiset: a multiset with elements
    from the unit interval.  Otherwise, the argument will be transformed
    using \code{as.gset}.} 
  \item{elements}{A set (or list) of \code{e} objects which are
    object/memberships-pairs.}
  \item{charfun}{A function taking an object and returning the
    membership.}
  \item{method}{Currently, only \code{"Jaccard"} is implemented.}
  \item{level}{The minimum membership level.}
  \item{na.rm}{logical indicating whether \code{NA} values should be
    removed.}
  \item{decreasing}{Logical indicating the sorting direction.}
  \item{\dots}{For \code{gset_\emph{foo}()}: (g)set objects objects. For
    the mean and sort methods: additional parameters internally passed to
    \code{\link{mean}} and \code{\link{order}}, respectively. For
    \code{cut}: currently not used.}
}
\details{
  These functions represent basic infrastructure for handling
  \emph{generalized} sets of general (\R) objects.

  A generalized set (or gset) is set of pairs \eqn{(e, f)}, where
  \eqn{e} is some set element and \eqn{f} is the characteristic (or
  membership) function. For ordinary sets \eqn{f} maps to \eqn{\{0, 1\}},
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
  (in)equality. The \code{length} method for gsets gives the
  cardinality. \code{set_combn} returns the gset of all
  subsets of specfied length. The \code{Summary} methods do also work if
  defined for the set elements.

  The \code{\link{mean}} and \code{\link[stats]{median}}
  methods try to convert the object to a numeric vector before calling
  the default methods. The \code{\link{sort}} method internally rearranges the
  elements to produce a sorted output. The \code{cut} method
  \dQuote{filters} all elements with membership not less then
  \code{level} --- the result, thus, is a crisp
  (multi)set. \code{gset_similarity} computes the simple Jaccard
  similarity between two generalized sets \eqn{A} and \eqn{B},
  i.e., the cardinality of the
  intersection divided by the cardinality of the union of \eqn{A} and \eqn{B}.

  Because set elements are unordered, it is not allowed to use indexing
  (except using labels). However, it is possible to iterate over
  \emph{all} elements using \code{for} and \code{lapply}/\code{sapply}.

  Note that \code{gset_union}, \code{gset_intersection},
  \code{gset_sum} and
  \code{gset_symdiff} accept any number of arguments.

  \code{gset_contains_element} is vectorized in \code{e}, that is, if \code{e}
  is an atomic vector or list, the is-element operation is performed
  element-wise, and a logical vector returned. Note that, however,
  objects of class \code{"tuple"} are taken as atomic objects to
  correctly handle sets of tuples.
  
}
\seealso{
  \code{\link{set}} for \dQuote{ordinary} sets, 
  \code{\link{gset_outer}}, and
  \code{\link{tuple}} for tuples (\dQuote{vectors}).
}
\examples{
## multisets
A = gset(letters[1:5], memberships = c(3, 2, 1, 1, 1))
B = gset(c("a", "c", "e", "f"), memberships = c(2, 2, 1, 2))
cut(B, 0.5)
rep(B, 2)

gset_union(A, B)
gset_intersection(A, B)
gset_complement(A, B)

gset_is_multiset(A)
gset_sum(A, B)
gset_difference(A, B)

## fuzzy sets
A = gset(letters[1:5], memberships = c(1, 0.3, 0.8, 0.6, 0.2))
B = gset(c("a", "c", "e", "f"), memberships = c(0.7, 1, 0.4, 0.9))
A * B

## fuzzy multisets
A = gset(c("a", "b", "d"),
         memberships = list(c(0.3, 1, 0.5), c(0.9, 0.1), gset(c(0.4, 0.7), c(1, 2))))
B = gset(c("a", "c", "d", "e"),
         memberships = list(c(0.6, 0.7), c(1, 0.3), c(0.4, 0.5), 0.9))
gset_union(A, B)
gset_intersection(A, B)
gset_complement(A, B)

## other operations
mean(gset(1:3, c(0.1,0.5,0.9)))
median(gset(1:3, c(0.1,0.5,0.9)))
}
\keyword{math}
