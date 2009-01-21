\name{plot}
\alias{plot.gset}
\alias{plot.set}
\alias{plot.cset}
\alias{plot.tuple}
\alias{plot.charfun_generator}
\alias{lines.gset}
\alias{lines.set}
\alias{lines.cset}
\alias{lines.tuple}
\alias{lines.charfun_generator}
\title{Plot functions for generalized sets}
\description{Plot and lines functions for (tuples of) generalized sets and
  function generators of characteristic functions.}
\usage{
\method{plot}{gset}(x, type = NULL, ylim = NULL,
         xlab = "Universe", ylab = "Membership Grade", \dots)
\method{plot}{cset}(x, \dots)
\method{plot}{set}(x, \dots)
\method{plot}{tuple}(x, type = "l", ylim = NULL,
         xlab = "Universe", ylab = "Membership Grade", col = 1,
         continuous = TRUE, \dots)
\method{plot}{charfun_generator}(x, universe = NULL, \dots)

\method{lines}{gset}(x, type = "l", col = 1, continuous = TRUE,
         universe = NULL, \dots)
\method{lines}{cset}(x, \dots)
\method{lines}{set}(x, \dots)
\method{lines}{tuple}(x, col = 1, universe = NULL, \dots)
\method{lines}{charfun_generator}(x, universe = NULL, \dots)
}
\arguments{
  \item{x}{For a method for class \var{foo}, an object of class
    \var{foo}.}
  \item{type}{Same as the \code{type} argument of
    \code{\link[graphics]{plot}}.  For \code{plot.gset}
    and \code{plot.cset}, \code{"barplot"} can also be used.}
  \item{universe}{Universal set used for setting up the plot region. By
    default, this is deduced from the object(s) to be plotted.}
  \item{col}{Character or integer vector specifying the color of the
    object(s) to be plotted.}
  \item{continuous}{Logical indicating whether zero membership degrees
    \dQuote{inside} the graph should be ignored.}
  \item{xlab, ylab}{Character labels for the axes.}
  \item{ylim}{Double vector of length 2 defining the range of the y axis.}
  \item{\dots}{Further arguments passed to the default plot methods.}
}
\value{
  The main argument (invisibly).
}

\seealso{
  \code{\link{set}}, \code{\link{gset}}, and \code{\link{tuple}} for the
  set types, and \code{\link{fuzzy_normal}} for available characteristic
  functions.
}
\examples{
## basic plots
plot(gset(1:3, 1:3/3))
plot(gset(1:3, 1:3/3, universe = 0:4))
plot(gset(c("a", "b"), list(1:2/2, 0.3)))

## characteristic functions
plot(fuzzy_normal)
plot(tuple(fuzzy_normal, fuzzy_bell), col = 1:2)

## superposing plots using lines()
x <- fuzzy_normal_gset()
y <- fuzzy_trapezoid_gset(corners = c(5,10,15,17), height = c(0.7, 1))
plot(tuple(x, y))
lines(x | y, col = 2)
lines(x & y, col = 3)

## another example using gset_mean
x <- fuzzy_two_normals_gset(sd = c(2,1))
y <- fuzzy_trapezoid_gset(corners = c(5,9,11,15))
plot(tuple(x, y))
lines(tuple(gset_mean(x, y),
            gset_mean(x, y, "geometric"),
            gset_mean(x, y, "harmonic")),
      col = 2:4)

## creating a sequence of sets
plot(fuzzy_tuple(fuzzy_cone, 10), col = gray.colors(10))
}
\keyword{math}
