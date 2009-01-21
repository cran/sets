\name{cset}
\alias{cset}
\alias{as.cset}
\alias{is.cset}
\alias{cset_is_empty}
\alias{cset_is_subset}
\alias{cset_is_proper_subset}
\alias{cset_is_equal}
\alias{cset_contains_element}
\alias{cset_core}
\alias{cset_universe}
\alias{cset_height}
\alias{cset_union}
\alias{cset_sum}
\alias{cset_difference}
\alias{cset_product}
\alias{cset_mean}
\alias{cset_intersection}
\alias{cset_symdiff}
\alias{cset_complement}
\alias{cset_power}
\alias{cset_cartesian}
\alias{cset_combn}
\alias{cset_cardinality}
\alias{cset_charfun}
\alias{cset_memberships}
\alias{cset_transform_memberships}
\alias{cset_concentrate}
\alias{cset_dilate}
\alias{cset_normalize}
\alias{cset_support}
\alias{cset_similarity}
\alias{cset_is_crisp}
\alias{cset_is_fuzzy_set}
\alias{cset_is_multiset}
\alias{cset_is_set_or_fuzzy_set}
\alias{cset_is_set_or_multiset}
\alias{cset_is_fuzzy_multiset}
\alias{cset_is_set}
\alias{length.cset}
\alias{matchfun}
\alias{mean.cset}
\alias{median.cset}
\alias{cut.cset}
\alias{cset_orderfun}
\alias{cset_matchfun}
\alias{cset_orderfun<-}
\alias{cset_matchfun<-}

\title{Customizable sets}
\description{Creation and manipulation of customizable sets.}
\usage{
cset(gset,
     orderfun = sets_options("orderfun"),
     matchfun = sets_options("matchfun"))
cset_support(x)
cset_core(x)
cset_height(x)
cset_memberships(x)
cset_universe(x)

cset_transform_memberships(x, FUN, \dots)
cset_concentrate(x)
cset_dilate(x)
cset_normalize(x, height = 1)

matchfun(FUN)

cset_orderfun(x)
cset_matchfun(x)
cset_orderfun(x) <- value
cset_matchfun(x) <- value

as.cset(x)
is.cset(x)

cset_is_empty(x)
cset_is_subset(x, y)
cset_is_proper_subset(x, y)
cset_is_equal(x, y)
cset_contains_element(x, e)

cset_is_set(x)
cset_is_multiset(x)
cset_is_fuzzy_set(x)
cset_is_set_or_multiset(x)
cset_is_set_or_fuzzy_set(x)
cset_is_fuzzy_multiset(x)
cset_is_crisp(x)

cset_cardinality(x, type = c("absolute", "relative"))
cset_union(\dots)
cset_mean(x, y, type = c("arithmetic", "geometric", "harmonic"))
cset_product(\dots)
cset_difference(\dots)
cset_intersection(\dots)
cset_symdiff(\dots)
cset_complement(x, y)
cset_power(x)
cset_cartesian(\dots)
cset_combn(x, m)

cset_similarity(x, y, method = "Jaccard")

\method{cut}{cset}(x, level = 1, \dots)
\method{mean}{cset}(x, \dots)
\method{median}{cset}(x, na.rm = FALSE)
\method{length}{cset}(x)
}
\arguments{
  \item{x}{For \code{as.cset()} and \code{is.cset()}:
    an \R object. A (c)set object otherwise.}
  \item{y}{A (c)set object.}
  \item{gset}{A generalized set (or some other \R object coercible to it).}
  \item{matchfun}{A function for matching similar elements, comparable 
    to \code{\link{match}}, taking two arguments: \code{x} (vector of
    elements to be matched) and \code{table} (vector of elements to be
    matched against). The return value is an integer vector of the
    matching positions (or \code{NA} if there is no match).
    Note that the default beavior is to test for \emph{identity}.}
  \item{FUN}{A predicate testing for equality of two objects.}
  \item{orderfun}{A function taking a list and returning an integer vector,
    specifying the order in which an iterator processes the set
    elements. Alternatively, the index vector can be specified directly.}
  \item{value}{A new match function (order function).}
  \item{type}{For \code{gset_cardinality()}:
    cardinality type (either \code{"absolute"} or
    \code{"relative"}). For \code{gset_mean()}: mean type
    (\code{"arithmetic"}, \code{"geometric"}, or \code{"harmonic"}).}
  \item{height}{Double from the unit interval for scaling memberships.}
  \item{e}{An object of class \code{element}.}
  \item{m}{Number of elements to choose.}
  \item{method}{Currently, only \code{"Jaccard"} is implemented.}
  \item{level}{The minimum membership level.}
  \item{na.rm}{logical indicating whether \code{NA} values should be
    removed.}
  \item{\dots}{For \code{cset_\var{foo}()}: (c)set objects. For
    the mean and sort methods: additional parameters internally passed to
    \code{\link{mean}} and \code{\link{order}}, respectively. For
    \code{gset_transform_memberships}: further arguments passed to
    \code{FUN}. For \code{cut}: currently not used.}
}
\details{
  Customizable sets extend generalized sets in two ways: First, users
  can control the way elements are matched, i.e., define equivalence
  classes of elements. Second, an order function (or permutation index)
  can be specified for each set for changing the order in which
  iterators such as \code{as.list} process the elements. The latter in
  particular influences the labeling and print methods for
  customizable sets.

  The match function needs to be vectorized in a similar way than
  \code{\link{match}}. \code{matchfun} can be used to create such a
  function from a \dQuote{simple} predicate testing for equality
  (such as, e.g., \code{\link{identical}}). Make sure, however, to
  create the same function only once.

  Note that operations on customizable sets require the same match
  function for all sets involved. The order function can differ, but
  will then be stripped from the result.

  \code{sets_options} can be used to
  conveniently switch the default match and/or
  order function if a number of \code{cset} objects need to be created.
}
\seealso{
  \code{\link{set}} for (\dQuote{ordinary}) sets, 
  \code{\link{gset}} for generalized sets, 
  \code{\link{cset_outer}}, and
  \code{\link{tuple}} for tuples (\dQuote{vectors}).
}
\examples{
## default behavior of sets: matching of elements is very strict
## Note that on most systems, 3.3 - 2.2 != 1.1
x <- set("1", 1L, 1, 3.3 - 2.2, 1.1)
print(x)

y <- set(1, 1.1, 2L, "2")
print(y)
1L \%e\% y

set_union(x, y)
set_intersection(x, y)
set_complement(x, y)

## Now use the more sloppy match()-function. 
## Note that 1 == "1" == 1L ...
X <- cset(x, matchfun = match)
print(X)
Y <- cset(y, matchfun = match)
print(Y)
1L \%e\% Y

cset_union(X, Y)
cset_intersection(X, Y)
cset_complement(X, Y)

## Same using all.equal().
## This is a non-vectorized predicate, so use matchfun
## to generate a vectorized version:
FUN <- matchfun(function(x, y) isTRUE(all.equal(x, y)))
X <- cset(x, matchfun = FUN)
print(X)
Y <- cset(y, matchfun = FUN)
print(Y)
1L \%e\% Y

cset_union(X, Y)
cset_intersection(X, Y)
cset_complement(X, Y)

### change default functions via set_option
sets_options("matchfun", match)
cset(x)
cset(y)

cset(1:3) <= cset(c(1,2,3))

### restore package defaults
sets_options("matchfun", NULL)

### customized order function
FUN <- function(x) order(as.character(x), decreasing = TRUE)
Z <- cset(letters[1:5], orderfun = FUN)
print(Z)
as.character(Z)

## converter for ordered factors keeps order
o <- ordered(c("a", "b", "a"), levels = c("b", "a"))
as.set(o)
as.cset(o)

## converter for other data types keep order if the elements are unique:
as.cset(c("A", "quick", "brown", "fox"))
as.cset(c("A", "quick", "brown", "fox", "quick"))
}
\keyword{math}
