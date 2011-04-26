\name{interval}
\alias{as.character.interval}
\alias{as.double.interval}
\alias{as.interval}
\alias{as.interval.default}
\alias{as.interval.interval}
\alias{as.interval.list}
\alias{as.interval.NULL}
\alias{as.interval.numeric}
\alias{as.interval.set}
\alias{as.interval.tuple}
\alias{as.list.interval}
\alias{integers}
\alias{integers2reals}
\alias{interval}
\alias{interval_complement}
\alias{interval_contains_element}
\alias{interval_difference}
\alias{interval_division}
\alias{interval_domain}
\alias{interval_intersection}
\alias{interval_is_bounded}
\alias{interval_is_closed}
\alias{interval_is_countable}
\alias{interval_is_degenerate}
\alias{interval_is_empty}
\alias{interval_is_equal}
\alias{interval_is_less_than_or_equal}
\alias{interval_is_greater_than_or_equal}
\alias{interval_is_less_than}
\alias{interval_is_greater_than}
\alias{interval_is_finite}
\alias{interval_is_half_bounded}
\alias{interval_is_left_bounded}
\alias{interval_is_left_closed}
\alias{interval_is_left_open}
\alias{interval_is_left_unbounded}
\alias{interval_is_proper}
\alias{interval_is_proper_subinterval}
\alias{interval_is_right_bounded}
\alias{interval_is_right_closed}
\alias{interval_is_right_open}
\alias{interval_is_right_unbounded}
\alias{interval_is_subinterval}
\alias{interval_is_unbounded}
\alias{interval_is_uncountable}
\alias{interval_measure}
\alias{interval_power}
\alias{interval_product}
\alias{interval_sum}
\alias{interval_symdiff}
\alias{interval_union}
\alias{is.interval}
\alias{max.interval}
\alias{mean.interval}
\alias{min.interval}
\alias{naturals}
\alias{naturals0}
\alias{range.interval}
\alias{reals}
\alias{reals2integers}
\alias{prod.interval}
\alias{sum.interval}
\alias{\%..\%}
\alias{\%<\%}
\alias{\%>\%}
\alias{\%<=\%}
\alias{\%>=\%}
\title{Intervals}
\description{Interval class for countable and uncountable numeric sets.}
\usage{
interval(l=NULL, r=l,
         bounds=c("[]", "[)", "(]", "()", "[[", "]]", "][",
                  "open", "closed", "left-open", "right-open",
                  "left-closed", "right-closed"),
         domain=NULL)

reals(l=NULL, r=NULL,
      bounds=c("[]", "[)", "(]", "()", "[[", "]]", "][",
               "open", "closed", "left-open", "right-open",
               "left-closed", "right-closed"))
integers(l=NULL, r=NULL)
naturals(l=NULL, r=NULL)
naturals0(l=NULL, r=NULL)
l \%..\% r

interval_domain(x)

as.interval(x)
integers2reals(x, min=-Inf, max=Inf)
reals2integers(x)

interval_complement(x, y=NULL)
interval_intersection(\dots)
interval_symdiff(\dots)
interval_union(\dots)

interval_difference(\dots)
interval_division(\dots)
interval_product(\dots)
interval_sum(\dots)

is.interval(x)
interval_contains_element(x, y)
interval_is_bounded(x)
interval_is_closed(x)
interval_is_countable(\dots)
interval_is_degenerate(x)
interval_is_empty(x)
interval_is_equal(x, y)
interval_is_less_than_or_equal(x, y)
interval_is_less_than(x, y)
interval_is_greater_than_or_equal(x, y)
interval_is_greater_than(x, y)
interval_is_finite(x)
interval_is_half_bounded(x)
interval_is_left_bounded(x)
interval_is_left_closed(x)
interval_is_left_open(\dots)
interval_is_left_unbounded(x)
interval_measure(x)
interval_is_proper(\dots)
interval_is_proper_subinterval(x, y)
interval_is_right_bounded(x)
interval_is_right_closed(x)
interval_is_right_open(\dots)
interval_is_right_unbounded(x)
interval_is_subinterval(x, y)
interval_is_unbounded(x)
interval_is_uncountable(x)
interval_power(x, n)
x \%<\% y
x \%>\% y
x \%<=\% y
x \%>=\% y
}
\arguments{
  \item{x}{For \code{as.interval()} and \code{is.interval()}: an \R
    object. For all other functions: an interval object (or any other \R object
    coercible to one).}
  \item{y}{An interval object (or any other \R object coercible to
    one).}
  \item{min, max}{Integers defining the range to be coerced.}
  \item{l, r}{Numeric values defining the bounds of the interval. For
    integer domains, these will be rounded.}
  \item{bounds}{Character string specifying whether the interval is
    open, closed, or left/right-open/closed. Symbolic shortcuts such as
    \code{"()"} or \code{"]["} for an open interval, etc., are also accepted.}
  \item{domain}{Character string specifying the domain of the interval:
    \code{"R"}, \code{"Z"}, \code{"N"}, and \code{"N0"} for the reals,
    integers, positive integers and non-negative integers,
    respectively. If unspecified, the domain will be guessed from the
    mode of the numeric values specifying the bounds.}
  \item{n}{Integer exponent.}
  \item{\dots}{Interval objects (or other \R objects coercible to
    interval objects).}
}
\details{
  An interval object represents a multi-interval, i.e., a union of
  disjoint, possibly unbounded (i.e., infinite)
  ranges of numbers---either the extended reals, or sequences of
  integers. The usual set operations (union, complement, intersection)
  and predicates (equality, (proper) inclusion) are implemented. If
  (numeric) sets and interval objects are mixed, the result will be an
  interval object. Some basic interval arithmetic operations
  (addition, subtraction, multiplication, division, power) as well
  mathematical functions (\code{log}, \code{log2}, \code{log10}, \code{exp},
  \code{abs}, \code{sqrt}, \code{trunc}, \code{round}, \code{floor},
  \code{ceiling}, \code{signif}, and the trigonometric functions)
  are defined. Note that the rounding functions will discretize the
  interval. 

  Coercion methods for the \code{as.numeric}, \code{as.list}, and
  \code{as.set} generics are implemented. \code{reals2integers()}
  discretizes a real multi-interval. \code{integers2reals()} returns a
  multi-interval of corresponding (degenerate) real intervals.

  The summary functions \code{min}, \code{max}, \code{range},
  \code{sum}, \code{mean} and \code{prod} are implemented and work
  on the interval bounds.

  \code{sets_options()} allows to change the style of open bounds
  according to the ISO 31-11 standard using reversed brackets instead of
  round parentheses (see examples).
}
\value{For the predicates: a logical value. For all other functions:
  an interval object.}
\seealso{
  \code{\link{set}} and \code{\link{gset}} for \emph{finite} (generalized) sets.
}
\examples{
#### * general interval constructor

interval(1,5)
interval(1,5, "[)")
interval(1,5, "()")

## ambiguous notation -> use alternative style
sets_options("openbounds", "][")
interval(1,5, "()")
sets_options("openbounds", "()")

interval(1,5, domain = "Z")
interval(1L, 5L)

## degenerate interval
interval(3)

## empty interval
interval()

#### * reals
reals()
reals(1,5)
reals(1,5,"()")
reals(1) ## half-unbounded

## (auto-)complement
!reals(1,5)
interval_complement(reals(1,5), reals(2, Inf))

## combine/c(reals(2,4), reals(3,5))
reals(2,4) | reals(3,5)

## intersection
reals(2,4) & reals(3,5)

## overlapping intervals
reals(2,4) & reals(3,5)
reals(2,4) & reals(4,5,"(]")

## non-overlapping
reals(2,4) & reals(7,8)
reals(2,4) | reals(7,8)
reals(2,4,"[)") | reals(4,5,"(]")

## degenerated cases
reals(2,4) | interval()
c(reals(2,4), set())

reals(2,4) | interval(6)
c(reals(2,4), set(6), 9)

## predicates
interval_is_empty(interval())
interval_is_degenerate(interval(4))
interval_is_bounded(reals(1,2))
interval_is_bounded(reals(1,Inf)) ## !! FALSE, because extended reals
interval_is_half_bounded(reals(1,Inf))
interval_is_left_bounded(reals(1,Inf))
interval_is_right_unbounded(reals(1,Inf))
interval_is_left_closed(reals(1,Inf))
interval_is_right_closed(reals(1,Inf)) ## !! TRUE

reals(1,2) <= reals(1,5)
reals(1,2) < reals(1,2)
reals(1,2) <= reals(1,2,"[)")
reals(1,2,"[)") < reals(1,2)

#### * integers
integers()
naturals()
naturals0()

3 \%..\% 5
integers(3, 5)
integers(3, 5) | integers(6,9)
integers(3, 5) | integers(7,9)

interval_complement(naturals(), integers())

naturals() <= naturals0()
naturals0() <= integers()

## mix reals and integers
c(reals(2,5), integers(7,9))
interval_complement(reals(2,5), integers())
interval_complement(integers(2,5), reals())

try(interval_complement(integers(), reals()), silent = TRUE) ## infeasible --> error

integers() <= reals()
reals() <= integers()

### interval arithmetic
x <- interval(2,4)
y <- interval(3,6)
x + y
x - y
x * y
x / y

## summary functions
min(x, y)
max(y)
range(y)
mean(y)
}
\keyword{math}
