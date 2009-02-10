\name{fuzzyfuns}
\alias{charfun_generator}
\alias{is.charfun_generator}
\alias{fuzzy_tuple}
\alias{fuzzy_normal}
\alias{fuzzy_two_normals}
\alias{fuzzy_bell}
\alias{fuzzy_cone}
\alias{fuzzy_sigmoid}
\alias{fuzzy_trapezoid}
\alias{fuzzy_triangular}
\alias{fuzzy_normal_gset}
\alias{fuzzy_two_normals_gset}
\alias{fuzzy_bell_gset}
\alias{fuzzy_sigmoid_gset}
\alias{fuzzy_trapezoid_gset}
\alias{fuzzy_triangular_gset}
\alias{fuzzy_cone_gset}
\title{Fuzzy membership functions}
\description{Fuzzy membership and set creator functions.}
\usage{
charfun_generator(FUN, height = 1)
fuzzy_tuple(FUN = fuzzy_normal, n = 5, \dots,
                 universe = NULL, names = NULL)
is.charfun_generator(x)

fuzzy_normal(mean = NULL, sd = 1, log = FALSE, height = 1, chop = 0)
fuzzy_two_normals(mean = NULL, sd = c(1,1), log = c(FALSE, FALSE),
                  height = 1, chop = 0)
fuzzy_bell(center = NULL, cross = NULL, slope = 4, height = 1, chop = 0)
fuzzy_sigmoid(cross = NULL, slope = 0.5, height = 1, chop = 0)
fuzzy_trapezoid(corners = NULL, height = c(1,1), return_base_corners = TRUE)
fuzzy_triangular(corners = NULL, height = 1, return_base_corners = TRUE)
fuzzy_cone(center = NULL, radius = 2, height = 1, return_base_corners = TRUE)

fuzzy_normal_gset(mean = NULL, sd = 1, log = FALSE, height = 1,
                  chop = 0, universe = NULL)
fuzzy_two_normals_gset(mean = NULL, sd = c(1,1), log = c(FALSE, FALSE),
                      height = 1, chop = 0, universe = NULL)
fuzzy_bell_gset(center = NULL, cross = NULL, slope = 4, height = 1,
                chop = 0, universe = NULL)
fuzzy_sigmoid_gset(cross = NULL, slope = 0.5, height = 1,
                   chop = 0, universe = NULL)
fuzzy_trapezoid_gset(corners = NULL, height = c(1,1), universe = NULL,
                     return_base_corners = TRUE)
fuzzy_triangular_gset(corners = NULL, height = 1, universe = NULL,
                      return_base_corners = TRUE)
fuzzy_cone_gset(center = NULL, radius = 2, height = 1, universe = NULL,
                return_base_corners = TRUE)
}
\arguments{
  \item{x}{An \R object.}
  \item{n}{Positive integer indicating the number of sets to be created,
    or integer vector of location paramters used to create the sets.}
  \item{FUN}{Function to be used for creating a membership function.
    Needs to be vectorized, i.e., is expected to take a vector of set
    elements and to return a vector of numeric values.}
  \item{height}{Numeric value in the unit interval specifying the height
    of the set resulting from applying the membership function to the
    universe, i.e., the maximum value to which the values will be scaled to.}
  \item{chop}{Threshold value below which the membership function is
    truncated, i.e., has a value of 0.}
  \item{center, mean}{Numeric mean value(s) used for
    the resulting membership function.}
  \item{sd}{Numeric scale factor(s) (standard deviation(s)) used for the
    resulting membership function.}
  \item{radius}{Double added/subtracted from \code{center} to get the
    base line corners of the cone.}
  \item{log}{Logical (vector), indicating whether normal or log-normal
    distributions should be used.}
  \item{cross}{Double indicating the cross-over point for the sigmoidal
    distribution.}
  \item{slope}{Double indicating the slope at the cross-over point.}
  \item{corners}{Double values (vector of length four)
    indicating the abscisses of the four corners of the resulting
    trapezoid.}
  \item{return_base_corners}{Logical indicating whether membership grades
    for the base line corner elements should be returned as small values
    instead of 0.  Otherwise, if a set is created from the memberships,
    the corner elements would be removed from the set.}
  \item{universe}{Universal set used for computing the memberships
    grades.}
  \item{names}{optional character vector of component labels for the return value.}
  \item{\dots}{Further arguments passed to \code{FUN}.}
  }
\details{
  These functions can be used to create sets with certain membership
  patterns. 

  The core functions are function \emph{generators}, taking parameters
  and returning a corresponding fuzzy function (i.e., with values in the
  unit interval).  All of them are normalized, i.e., scaled to have a
  maximum value of \code{height} (default: 1):

  \describe{
    \item{\code{fuzzy_normal}}{is simply based on \code{\link[stats]{dnorm}}.}
    
    \item{\code{fuzzy_two_normals}}{returns a function composed of the left and
      right parts of two normal distributions (each normalized),
      with possibly different means and standard
      deviations.}
    
    \item{\code{fuzzy_bell}}{returns a function defined as:
      \eqn{\frac{1}{\left(1 + |\frac{x - c}{w}| \right) ^ {2s}}}{1 / (1
      + |(x - c) / w|) ^ 2s}  with center \eqn{c},
      crossover points \eqn{c \pm w}{c +/- w}, and slope at the crossover
      points of \eqn{\frac{s}{2w}}{s / 2w}.}
    
    \item{\code{fuzzy_sigmoid}}{yields a function whose values are
      computed as \eqn{\frac{1}{1 + e ^ {s (c - x)}}}{1 / (1 + exp{-s (x -
	  c)})}  with slope \eqn{s} at crossover point \eqn{c}.}
    
    \item{\code{fuzzy_trapezoid}}{creates a function with
      trapezoidal shape, defined by four \code{corners} elements and two
      \code{height} values for the second and third corner (the heights of
      the first and fourth corner being fixed at 0).}
    
    \item{\code{fuzzy_triangular}}{similar to the above with only three
      corners.}
    
    \item{\code{fuzzy_cone}}{is a special case of \code{fuzzy_triangular},
      defining an isosceles triangle with corners (element, membership degree):
      \eqn{(\code{center} - \code{radius}, 0)},
      \eqn{(\code{center}, \code{height})}, and
      \eqn{(\code{center} + \code{radius}, 0)}.}
  }

  \code{charfun_generator} takes a vectorized function as argument,
  returning a function normalized by \code{height}.

  The \code{fuzzy_\var{foo}_gset} functions directly generate
  generalized sets from \code{fuzzy_\var{foo}}, using the values defined by
  \code{universe}, \code{sets_options("universe")}, or \code{seq(0, 20, by
    = 0.1)} (in that order, whichever is not \code{NULL}).

  \code{fuzzy_tuple} generates a sequence of \code{n}
  sets based on any of the generating functions (except
  \code{fuzzy_trapezoid} and \code{fuzzy_triangular}).  The chosen
  generating function \code{FUN} is called with \code{n} different
  values chosen along the \code{universe} passed to the
  first argument, thus varing the position or the resulting graph.
}
\value{
  For \code{charfun_generator}, a \emph{generating function}
  taking an argument list of parameters, and returning a membership
  function, mapping elements to membership values (from of the unit
  interval).
  
  For \code{fuzzy_tuple}, a tuple of \code{n} fuzzy sets.
  
  For \code{is.charfun_generator}, a logical.

  For \code{fuzzy_\var{foo}_gset}, a fuzzy set.

  For the other functions, a membership function.
}

\seealso{
  \code{\link{set}}, \code{\link{gset}}, and \code{\link{tuple}} for the
  set types, and \code{\link{plot.gset}} for the available plot functions.
}
\examples{
## creating a fuzzy normal function
N <- fuzzy_normal(mean = 0, sd = 1)
N(-3:3)

## create a fuzzy set with it
gset(charfun = N, universe = -3:3)

## same using wrapper
fuzzy_normal_gset(universe = -3:3)

## creating a user-defined fuzzy function
fuzzy_poisson <- charfun_generator(dpois)
gset(charfun = fuzzy_poisson(10), universe = seq(0, 20, 2))

## creating a series of fuzzy normal sets
fuzzy_tuple(fuzzy_normal, 5)

## creating a series of fuzzy cones with specific locations
fuzzy_tuple(fuzzy_cone, c(2,3,7))
}
\keyword{math}
