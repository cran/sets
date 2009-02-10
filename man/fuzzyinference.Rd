\name{fuzzyinference}
\alias{fuzzy_rule}
\alias{fuzzy_variable}
\alias{fuzzy_partition}
\alias{fuzzy_system}
\alias{fuzzy_inference}
\alias{\%is\%}
\title{Fuzzy inference}
\description{Basic infrastructure for building and using fuzzy inference
  systems.}  
\usage{
fuzzy_inference(system, values, implication = c("minimum", "product"))
fuzzy_rule(antecedent, consequent)
fuzzy_system(variables, rules)
fuzzy_partition(varnames, FUN = fuzzy_normal, universe = NULL, \dots)
fuzzy_variable(\dots)
x \%is\% y
}
\arguments{
  \item{\dots}{For \code{fuzzy_variable}: named list of fuzzy sets (or
    membership functions from which the fuzzy sets will be created using the
    default universe). For \code{fuzy_partition}: further arguments passed to \code{FUN}.}
  \item{antecedent, consequent}{parts of an inference rule (see
    details).}
  \item{variables}{Set of fuzzy variables.}
  \item{rules}{Set of rules.}
  \item{system}{A fuzzy system.}
  \item{values}{Named list of input values to the system. The names must
    match the labels of the variable set.}
  \item{implication}{A vectorized function taking two arguments, or a
    character string indicating the parallel minimum or the product
    function.}
  \item{varnames}{Either a character vector of fuzzy category labels, to
    be used with the default locations, or
    a named numeric vector of locations.}
  \item{FUN}{Function generator for membership functions to be used for
    the fuzzy partition.}
  \item{universe}{Universal set used for computing the memberships
    grades.}
  \item{x}{The name of a fuzzy variable.}
  \item{y}{The name of a category, belonging to a fuzzy variable.}
}
\details{
  These functions can be used to create simple fuzzy inference machines
  based on fuzzy (\dQuote{linguistic}) variables and fuzzy rules. This
  involves five steps:

  \enumerate{
    \item Fuzzification of the input variables.
    \item Application of fuzzy operators (AND, OR, NOT) in the
      antecedents of some given rules.
    \item Implication from the antecedent to the consequent.
    \item Aggregation of the consequents accross the rules.
    \item Defuzzification of the resulting fuzzy set.
  }
  
  Implication is based on either the minimum or the product.
  The evaluation of the logical expressions in the antecedents, as
  well as the aggregation of the evaluation result for each single rule,
  depends on the fuzzy logic currently set.
}
\value{
  For \code{fuzzy_inference}: a generalized set. For
  \code{fuzzy_rule} and \code{fuzzy_system}: an object of class
  \code{fuzzy_rule} and \code{fuzzy_system}, respectively.
  For \code{fuzzy_variable} and \code{fuzzy_partition}: an object of
  class \code{fuzzy_variable}, inheriting from \code{tuple}.
}
\seealso{
  \code{\link{set}} and \code{\link{gset}} for the
  set types, \code{\link{fuzzy_tuple}} for available fuzzy functions,
  and \code{\link{fuzzy_logic}} on the behavior of the implemented fuzzy operators.
}
\examples{
## set universe
sets_options("universe", seq(from = 0, to = 25, by = 0.1))

## set up fuzzy variables
variables <-
set(service = fuzzy_partition(varnames = c(poor = 0, good = 5, excellent = 10), sd = 1.5),
    food = fuzzy_variable(rancid = fuzzy_trapezoid(corners = c(-2, 0, 2, 4)),
                          delicious = fuzzy_trapezoid(corners = c(7, 9, 11, 13))),
    tip = fuzzy_partition(varnames = c(cheap = 5, average = 12.5, generous = 20),
                          FUN = fuzzy_cone, radius = 5)
    )

## set up rules
rules <-
set(
    fuzzy_rule(service \%is\% poor || food \%is\% rancid, tip \%is\% cheap),
    fuzzy_rule(service \%is\% good, tip \%is\% average),
    fuzzy_rule(service \%is\% excellent || food \%is\% delicious, tip \%is\% generous)
    )

## combine to a system
system <- fuzzy_system(variables, rules)
print(system)
plot(system) ## plots variables

## do inference
fi <- fuzzy_inference(system, list(service = 3, food = 8))

## plot resulting fuzzy set
plot(fi)

## defuzzify
gset_defuzzify(fi, "centroid")

## reset universe
sets_options("universe", NULL)
}
\keyword{math}
