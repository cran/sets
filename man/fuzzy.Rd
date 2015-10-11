\title{Fuzzy logic}
\encoding{UTF-8}
\name{fuzzy}
\alias{.N.}
\alias{.T.}
\alias{.S.}
\alias{.I.}
\alias{fuzzy_logic}
\description{Fuzzy Logic}
\usage{
fuzzy_logic(new, \dots)
.N.(x)
.T.(x, y)
.S.(x, y)
.I.(x, y)
}
\arguments{
  \item{x, y}{Numeric vectors.}
  \item{new}{A character string specifying one of the available fuzzy
    logic \dQuote{families} (see details).}
  \item{\dots}{optional parameters for the selected family.}
}
\details{
  A call to \code{fuzzy_logic()} without arguments returns the currently
  set fuzzy logic, i.e., a named list with four
  components \code{N}, \code{T}, \code{S}, and \code{I} containing the
  corresponding functions for negation, conjunction
  (\dQuote{t-norm}), disjunction (\dQuote{t-conorm}), and residual
  implication (which may not be available).

  The package provides several fuzzy logic \emph{families}.
  A concrete fuzzy logic is selected 
  by calling \code{fuzzy_logic} with a character
  string specifying the family name, and optional parameters.  Let us
  refer to \eqn{N(x) = 1 - x} as the \emph{standard} negation, and,
  for a t-norm \eqn{T}, let \eqn{S(x, y) = 1 - T(1 - x, 1 - y)} be the
  \emph{dual} (or complementary) t-conorm.  Available specifications and
  corresponding families are as follows, with the standard negation used
  unless stated otherwise.
  
  \describe{
    \item{\code{"Zadeh"}}{Zadeh's logic with \eqn{T = \min} and
      \eqn{S = \max}.  Note that the minimum t-norm, also known as the
      Gödel t-norm, is the pointwise largest t-norm, and that the
      maximum t-conorm is the smallest t-conorm.}
    \item{\code{"drastic"}}{the drastic logic with t-norm
      \eqn{T(x, y) = y} if \eqn{x = 1}, \eqn{x} if \eqn{y = 1}, and 0
      otherwise, and complementary t-conorm \eqn{S(x, y) = y} if
      \eqn{x = 0}, \eqn{x} if \eqn{y = 0}, and 1 otherwise.  Note that
      the drastic t-norm and t-conorm are the smallest t-norm and
      largest t-conorm, respectively.}
    \item{\code{"product"}}{the family with the product t-norm
      \eqn{T(x, y) = xy} and dual t-conorm \eqn{S(x, y) = x + y - xy}.} 
    \item{\code{"Lukasiewicz"}}{the Lukasiewicz logic with t-norm
      \eqn{T(x, y) = \max(0, x + y - 1)} and dual t-conorm
      \eqn{S(x, y) = \min(x + y, 1)}.}
    \item{\code{"Fodor"}}{the family with Fodor's \emph{nilpotent
	minimum} t-norm given by \eqn{T(x, y) = \min(x, y)} if
      \eqn{x + y > 1}, and 0 otherwise, and the dual t-conorm given by
    \eqn{S(x, y) = \max(x, y)} if \eqn{x + y < 1}, and 1 otherwise.}
    \item{\code{"Frank"}}{the family of Frank t-norms \eqn{T_p},
      \eqn{p \ge 0}, which gives the Zadeh, product and Lukasiewicz
      t-norms for \eqn{p = 0}, 1, and \eqn{\infty}{Inf}, respectively,
      and otherwise is given by 
      \eqn{T(x, y) = \log_p (1 + (p^x - 1) (p^y - 1) / (p - 1))}.}
    \item{\code{"Hamacher"}}{the three-parameter family of Hamacher,
      with negation \eqn{N_\gamma(x) = (1 - x) / (1 + \gamma x)},
      t-norm
      \eqn{T_\alpha(x, y) = xy / (\alpha + (1 - \alpha)(x + y - xy))},
      and t-conorm
      \eqn{S_\beta(x, y) = (x + y + (\beta - 1) xy) / (1 + \beta xy)},
      where \eqn{\alpha \ge 0} and \eqn{\beta, \gamma \ge -1}.  This
      gives a deMorgan triple (for which \eqn{N(S(x, y)) = T(N(x), N(y))}
      iff \eqn{\alpha = (1 + \beta) / (1 + \gamma)}.  The parameters can
      be specified as \code{alpha}, \code{beta} and \code{gamma},
      respectively.  If \eqn{\alpha} is not given, it is taken as
      \eqn{\alpha = (1 + \beta) / (1 + \gamma)}. 
      The default values for \eqn{\beta} and \eqn{\gamma} are 0, so that
      by default, the product family is obtained.}
  }

  The following parametric families are obtained by combining the
  corresponding families of t-norms with the standard negation.

  \describe{
    \item{\code{"Schweizer-Sklar"}}{the Schweizer-Sklar family
      \eqn{T_p}, \eqn{-\infty \le p \le \infty}{-Inf <= p <= Inf}, which
      gives the Zadeh (minimum), product and drastic t-norms for
      \eqn{p = -\infty}{p = -Inf}, \eqn{0}, and \eqn{\infty}{Inf},
      respectively, and otherwise is given by
      \eqn{T_p(x, y) = \max(0, (x^p + y^p - 1)^{1/p})}.}
    \item{\code{"Yager"}}{the Yager family \eqn{T_p}, \eqn{p \ge 0},
      which gives the drastic and minimum t-norms for \eqn{p = 0}
      and \eqn{\infty}{Inf}, respectively, and otherwise is given by
      \eqn{T_p(x, y) = \max(0, 1 - ((1-x)^p + (1-y)^p)^{1/p})}.}
    \item{\code{"Dombi"}}{the Dombi family \eqn{T_p}, \eqn{p \ge 0},
      which gives the drastic and minimum t-norms for \eqn{p = 0}
      and \eqn{\infty}{Inf}, respectively, and otherwise is given by
      \eqn{T_p(x, y) = 0} if \eqn{x = 0} or \eqn{y = 0}, and
      \eqn{T_p(x, y) = 1 / (1 + ((1/x - 1)^p + (1/y - 1)^p)^{1/p})} if
      both \eqn{x > 0} and \eqn{y > 0}.}
    \item{\code{"Aczel-Alsina"}}{the family of t-norms \eqn{T_p},
      \eqn{p \ge 0}, introduced by Aczél and Alsina, which gives the
      drastic and minimum t-norms for \eqn{p = 0} and
      \eqn{\infty}{Inf}, respectively, and otherwise is given by
      \eqn{T_p(x, y) = \exp(-(|\log(x)|^p + |\log(y)|^p)^{1/p})}.}
    \item{\code{"Sugeno-Weber"}}{the family of t-norms \eqn{T_p},
      \eqn{-1 \le p \le \infty}{-1 <= p <= Inf}, introduced by Weber
      with dual t-conorms introduced by Sugeno, which gives the
      drastic and product t-norms for \eqn{p = -1} and
      \eqn{\infty}{Inf}, respectively, and otherwise is given by
      \eqn{T_p(x, y) = \max(0, (x + y - 1 + pxy) / (1 + p))}.}
    \item{\code{"Dubois-Prade"}}{the family of t-norms \eqn{T_p},
      \eqn{0 \le p \le 1}, introduced by Dubois and Prade, which gives
      the minimum and product t-norms for \eqn{p = 0} and \eqn{1},
      respectively, and otherwise is given by
      \eqn{T_p(x, y) = xy / \max(x, y, p)}.}
    \item{\code{"Yu"}}{the family of t-norms \eqn{T_p}, \eqn{p \ge -1},
      introduced by Yu, which gives the product and drastic t-norms for
      \eqn{p = -1} and \eqn{\infty}{Inf}, respectively, and otherwise is
      given by \eqn{T(x, y) = \max(0, (1 + p) (x + y - 1) - p x y)}.}
  }

  By default, the Zadeh logic is used.

  \code{.N.}, \code{.T.}, \code{.S.}, and \code{.I.} are dynamic
  functions, i.e., wrappers that call the corresponding function of the
  current fuzzy logic.  Thus, the behavior of code using these
  functions will change according to the chosen logic.
}
\references{
  C. Alsina, M. J. Frank and B. Schweizer (2006),
  \emph{Associative Functions: Triangular Norms and Copulas}.
  World Scientific. ISBN 981-256-671-6.
  
  J. Dombi (1982),
  A general class of fuzzy operators, the De Morgan class of fuzzy
  operators and fuzziness measures induced by fuzzy operators,
  \emph{Fuzzy Sets and Systems} \bold{8}, 149--163.

  J. Fodor and M. Roubens (1994),
  \emph{Fuzzy Preference Modelling and Multicriteria Decision Support}.
  Kluwer Academic Publishers, Dordrecht.

  D. Meyer and K. Hornik (2009),
  Generalized and customizable sets in R,
  \emph{Journal of Statistical Software} \bold{31}(2), 1--27.
  \url{http://www.jstatsoft.org/v31/i02/}

  B. Schweizer and A. Sklar (1983),
  \emph{Probabilistic Metric Spaces}.
  North-Holland, New York.  ISBN 0-444-00666-4.
}
\examples{
x <- c(0.7, 0.8)
y <- c(0.2, 0.3)

## Use default family ("Zadeh")
.N.(x)
.T.(x, y)
.S.(x, y)
.I.(x, y)

## Switch family and try again
fuzzy_logic("Fodor")
.N.(x)
.T.(x, y)
.S.(x, y)
.I.(x, y)
}
\keyword{math}
